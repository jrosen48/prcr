# helpers.R

scale_vector <- function(x) {
    x / stats::sd(x, na.rm = TRUE)
}

center_vector <- function(x) {
    x - mean(x, na.rm = TRUE)
}

center_and_scale_vector <- function(x) {
    if (stats::sd(x, na.rm = TRUE) == 0) {
        x - mean(x, na.rm = TRUE)
    } else {
        (x - mean(x, na.rm = TRUE)) / stats::sd(x, na.rm = TRUE)
    }
}

distance_function <- function(x, distance_metric){
    if (distance_metric != "squared_euclidean") {
        distance <- stats::dist(x, method = distance_metric)
    } else {
        distance <- stats::dist(x, method = "euclidean")
        distance <- distance ^ 2
    }
    return(distance)
}

hclust_to_kmeans_function <- function(data, out, n_profiles){
    cut_hclust <- stats::cutree(out, n_profiles) # cuts the results of the hierarchical cluster at the specified # of clusters
    clusters_list <- list() # rewrite this to not loop (using purrr)
    for (i in seq(n_profiles)) { 
        clusters_list[[i]] <- data[cut_hclust == i,]
    }
    ordered_clusters <- list() # rewrite this to not loop (using purrr)
    cluster_freqs <- list()
    for (i in seq(length(clusters_list))) { 
        ordered_clusters[[i]] <- colSums(as.matrix(clusters_list[[i]]) / nrow(clusters_list[[i]]))
        cluster_freqs[[i]] <- ordered_clusters[[i]]
    }
    return(cluster_freqs)
}

try_kmeans <- function(x, s) {
    out <- tryCatch(
        {
            stats::kmeans(x, s, iter.max = 50)
        },
        error = function(cond) {
            # message(cond)
            return(NA)
        },
        warning = function(cond) {
            # message(cond)
            return(NA)
        }
    )    
    return(out)
}

kmeans_function <- function(data, cluster_freqs) {
    start <- data.frame(matrix(unlist(cluster_freqs), nrow = length(cluster_freqs[[1]]), byrow = TRUE), stringsAsFactors = F)
    start <- as.matrix(start)
    start <- t(start)
    return(try_kmeans(data, start))
}

# prcr <- function() {
#     structure(list(), class = "prcr")
# }

p <- function(df, ..., to_center, to_scale){
    # if (!is.data.frame(df)) stop("df must be a data.frame (or tibble)")
    df <- tibble::as_tibble(df)
    df_ss <- dplyr::select(df, ...)
    
    cases_to_keep <- stats::complete.cases(df_ss) # to use later for comparing function to index which cases to keep
    
    # cases_to_keep <- dplyr::data_frame(row_names = 1:nrow(df_ss),
    #                                    keep = cases_to_keep)
    
    df_ss_wo_incomplete_cases <- df_ss[cases_to_keep, ] # removes incomplete cases
    
    prepared_data <- list()
    
    # if (remove_mv_outliers == TRUE) {
    #     outliers <- detect_outliers(df_ss_wo_incomplete_cases)
    #     prepared_data[[11]] <- outliers
    #     names(prepared_data[[11]]) <- "outliers"
    #     message("Removed ", length(outliers), " multivariate outliers; view the outliers slot of the output to view the cases removed")
    #     df_ss_wo_incomplete_cases <- df_ss_wo_incomplete_cases[-outliers, ]
    # }
    
    df_wo_incomplete_cases <- df[cases_to_keep, ]
    # 
    # prepared_data[[10]] <- df_ss_wo_incomplete_cases
    # names(prepared_data)[[10]] <- "df_with_dummies"
    
    if (to_center == TRUE & to_scale == TRUE) {
        prepared_data[[1]] <- dplyr::mutate_all(df_ss_wo_incomplete_cases, center_and_scale_vector)
    } else if (to_center == TRUE & to_scale == FALSE) {
        prepared_data[[1]] <- dplyr::mutate_all(df_ss_wo_incomplete_cases, center_vector)
    } else if (to_center == FALSE & to_scale == TRUE) {
        prepared_data[[1]] <- dplyr::mutate_all(df_ss_wo_incomplete_cases, scale_vector)
    } else if (to_center == FALSE & to_scale == FALSE) {
        prepared_data[[1]] <- df_ss_wo_incomplete_cases
    }
    
    names(prepared_data)[[1]] <- "prepared_tibble"
    class(prepared_data) <- c("prcr")
    attributes(prepared_data)$cases_to_keep <- cases_to_keep
    prepared_data[[2]] <- df_wo_incomplete_cases
    names(prepared_data)[[2]] <- ".data"
    message("Prepared data: Removed ", sum(!cases_to_keep), " incomplete cases")
    return(prepared_data)
}

cluster_observations <- function(prepared_data,
                                 n_profiles,
                                 distance_metric,
                                 linkage){
    distance_matrix <- distance_function(prepared_data[[1]], distance_metric)
    clustered_data <- prepared_data
    clustered_data[[3]] <- stats::hclust(distance_matrix, method = linkage) # hierarhical clustering
    message(paste0("Hierarchical clustering carried out on: ", nrow(prepared_data[[1]]), " cases"))
    names(clustered_data)[[3]] <- "hierarchical_clustering_output"
    starting_points <- hclust_to_kmeans_function(prepared_data[[1]], clustered_data[[3]], n_profiles)

    clustered_data[[4]] <- kmeans_function(prepared_data[[1]], starting_points) # Fits k-means algorithm with hierarchical vals as start value

    if (length(clustered_data[[4]]) == 1) {
        return(clustered_data)
    }
    
    if (clustered_data[[4]]$iter == 1) {
        message(paste0("K-means algorithm converged: ", clustered_data[[4]]$iter, " iteration"))
    } else {
        message(paste0("K-means algorithm converged: ", clustered_data[[4]]$iter, " iterations"))
    }
    names(clustered_data)[[4]] <- "kmeans_clustering_output"
    if (class(clustered_data[[4]]) == "kmeans") {
        attributes(clustered_data)$n_profiles <- n_profiles
        message("Clustered data: Using a ", n_profiles, " cluster solution")    
    }
    clustered_data
}

calculate_statistics <- function(clustered_data, n_profiles, to_center, to_scale){
    clustering_stats <- clustered_data
    clustering_stats[[5]] <- clustering_stats[[4]]$betweenss / clustering_stats[[4]]$totss
    names(clustering_stats)[[5]] <- "r_squared"
    clustering_stats[[6]] <- tibble::as_tibble(data.frame(clustering_stats[[1]], cluster = clustering_stats[[4]]$cluster))
    names(clustering_stats)[[6]] <- ".data"
    cluster_centroids <- tibble::as_tibble(clustering_stats[[4]]$centers)
    cluster_centroids$Cluster <- paste0("Profile ", 1:nrow(cluster_centroids), " (", clustering_stats[[4]]$size," obs.)")
    clustering_stats[[7]] <- dplyr::select(cluster_centroids, dplyr::contains("Cluster"), dplyr::everything())
    names(clustering_stats)[[7]] <- "clustered_processed_data"
    df_to_plot <- tidyr::gather_(clustering_stats[[7]], key_col = "Variable", value_col = "Value", names(clustering_stats[[7]])[names(clustering_stats[[7]]) != 'Cluster'])
    message("Calculated statistics: R-squared = ", round(clustering_stats[[5]], 3))
    clustering_stats[[2]]$cluster <- clustering_stats[[4]]$cluster
    return(clustering_stats)
}


#' Identifies potential outliers
#' @details * add an argument to `create_profiles_cluster()` to remove multivariate outliers based on Hadi's (1994) procedure
#' @param df data.frame (or tibble) with variables to be clustered; all variables must be complete cases
#' @param return_index Boolean (TRUE or FALSE) for whether to return only the row indices of the possible multivariate outliers; if FALSE, then all of the output from the function (including the indices) is returned
#' @return either the row indices of possible multivariate outliers or all of the output from the function, depending on the value of return_index
#' @export
#' 

detect_outliers <- function(df, return_index = TRUE) {
    x <- outlierHadi(as.matrix(df))
    if (return_index == TRUE) {
        print(sort(x$Outliers))
        mv_outliers <- sort(x$Outliers)
    } else {
        print(x)
        x
    }
}

#' Return plot of profile centroids
#' @details Returns ggplot2 plot of cluster centroids
#' @param d summary data.frame output from create_profiles_cluster()
#' @param to_center whether to center the data before plotting
#' @param to_scale whether to scale the data before plotting
#' @return A ggplot2 object
#' @importFrom magrittr %>%  
#' @import ggplot2
#' @import dplyr
#' @export

plot_profiles <- function(d, to_center = F, to_scale = F){
    
    d %>% 
        dplyr::mutate_if(is.double, scale, center = to_center, scale = to_scale) %>% 
        group_by(cluster) %>% 
        summarize_all(mean) %>% 
        tidyr::gather(key, val, -cluster) %>% 
        ggplot(aes(x = cluster, y = val, fill = key)) +
        geom_col(position = "dodge") +
        theme_bw() +
        scale_fill_brewer("", type = "qual", palette=6) 
    
}

#' Concise summary of prcr cluster solution
#' @details Prints a concise summary of prcr cluster solution
#' @param object A `prcr` object
#' @param ... Additional arguments
#' @export

summary.prcr <- function(object, ...){
    # cat(paste0(attributes(object)$n_profiles,
    #            " cluster solution (R-squared = ", 
    #            round(object$r_squared, 3), ")\n\n"))
    print(object$clustered_processed_data)
}

#' Prints details of prcr cluster solution
#' @details Prints details of of prcr cluster solution
#' @param x A `prcr` object
#' @param ... Additional arguments
#' @export

print.prcr <- function(x, ...){
    print(x$.data)
}


#' student questionnaire data with four variables from the 2015 PISA for students in the United States
#'
#' @source http://www.oecd.org/pisa/data/
#' @format Data frame with columns
#' #' \describe{
#'   \item{CNTSTUID}{international student ID}
#'   \item{SCHID}{international school ID}
#'   ...
#' }
#' @import tibble

"pisaUSA15"

# quiets concerns (notes) of R CMD check re: the vars that are evaluated using non-standard evaluation
if (getRversion() >= "2.15.1") utils::globalVariables(c("cluster", "key", "val"))