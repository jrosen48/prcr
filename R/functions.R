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
            message(cond)
            return(NA)
        },
        warning = function(cond) {
            message(cond)
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

prcr <- function() {
    structure(list(), class = "prcr")
}

prepare_data <- function(df, ..., to_center, to_scale){
    if (!is.data.frame(df)) stop("df must be a data.frame (or tibble)")
    df <- tibble::as_tibble(df)
    df_ss <- dplyr::select(df, ...)
    cases_to_keep <- stats::complete.cases(df_ss) # to use later for comparing function to index which cases to keep
    df_ss_wo_incomplete_cases <- df_ss[cases_to_keep, ] # removes incomplete cases
    df_wo_incomplete_cases <- df[cases_to_keep, ]
    prepared_data <- prcr()
    prepared_data[[1]] <- tibble::as_tibble(scale(as.matrix(df_ss_wo_incomplete_cases), to_center, to_scale))
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
    clustered_data[[4]] <- kmeans_function(prepared_data[[1]], starting_points) # Fits k-means ithm with hierarchical vals as start value
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
    return(clustered_data)
}

calculate_statistics <- function(clustered_data, n_profiles){
    clustering_stats <- clustered_data
    clustering_stats[[5]] <- clustering_stats[[4]]$betweenss / clustering_stats[[4]]$totss
    names(clustering_stats)[[5]] <- "r_squared"
    clustering_stats[[6]] <- tibble::as_tibble(data.frame(clustering_stats[[1]], cluster = clustering_stats[[4]]$cluster))
    names(clustering_stats)[[6]] <- "clustered_raw_data"
    cluster_centroids <- tibble::as_tibble(clustering_stats[[4]]$centers)
    cluster_centroids$Cluster <- paste0("Cluster ", 1:nrow(cluster_centroids), " (", clustering_stats[[4]]$size," obs.)")
    clustering_stats[[7]] <- dplyr::select(cluster_centroids, dplyr::contains("Cluster"), dplyr::everything())
    names(clustering_stats)[[7]] <- "clustered_processed_data"
    
    df_to_plot <- tidyr::gather_(clustering_stats[[7]], key_col = "Variable", value_col = "Value", names(clustering_stats[[7]])[names(clustering_stats[[7]]) != 'Cluster'])
    
    p <- ggplot2::ggplot(df_to_plot, ggplot2::aes(x = df_to_plot$Cluster, y = df_to_plot$Value, fill = df_to_plot$Variable)) +
        ggplot2::geom_col(position = "dodge") +
        ggplot2::theme(legend.title = ggplot2::element_blank()) # this should be y[[7]]
    
    clustering_stats[[8]] <- p
    names(clustering_stats)[[8]] <- "ggplot_obj"
    message("Calculated statistics: R-squared = ", round(clustering_stats[[5]], 3))
    tmp <- as.data.frame(stats::model.matrix(~ factor(clustering_stats[[4]]$cluster) - 1))
    names(tmp) <- paste0("cluster_", 1:n_profiles)
    clustering_stats[[9]] <- dplyr::bind_cols(clustering_stats[[2]], tmp)
    names(clustering_stats)[[9]] <- "data_with_dummy_codes"
    clustering_stats[[2]]$cluster <- clustering_stats[[4]]$cluster
    clustering_stats[[2]] <- dplyr::select_(clustering_stats[[2]], "cluster", dplyr::everything())
    return(clustering_stats)
}

#' Create profiles of observed variables using two-step cluster analysis
#' @details Function to create a specified number of profiles of observed variables using a two-step (hierarchical and k-means) cluster analysis. 
#' @param df with two or more columns with continuous variables
#' @param ... unquoted variable names separated by commas
#' @param n_profiles The specified number of profiles to be found for the clustering solution
#' @param to_center (TRUE or FALSE) for whether to center the raw data with M = 0
#' @param to_scale Boolean (TRUE or FALSE) for whether to scale the raw data with SD = 1
#' @param distance_metric Distance metric to use for hierarchical clustering; "squared_euclidean" is default but more options are available (see ?hclust)
#' @param linkage Linkage method to use for hierarchical clustering; "complete" is default but more options are available (see ?dist)
#' @return A list containing the prepared data, the output from the hierarchical and k-means cluster analysis, the r-squared value, raw clustered data, processed clustered data of cluster centroids, and a ggplot object.
#' @examples
#' df <- mtcars
#' create_profiles(df, disp, hp, wt, n_profiles = 2, to_scale = TRUE)
#' @export

create_profiles <- function(df, 
                            ...,
                            n_profiles, 
                            to_center = F,
                            to_scale = F,
                            distance_metric = "squared_euclidean",
                            linkage = "complete"){
    prepped_data <- prepare_data(df, ..., to_center = to_center, to_scale = to_center)
    y <- cluster_observations(prepped_data, n_profiles, distance_metric, linkage)
    if (class(y[[4]]) == "kmeans") {
        z <- calculate_statistics(y, n_profiles)
        return(z)
    } else {
        y[[5]] <- NA
        names(y)[[5]] <- "r_squared"
        return(y)
    }
}

#' Return plot of cluster centroids
#' @details Returns ggplot2 plot of cluster centroids
#' @param x A `prcr` object
#' @param ... Additional arguments
#' @return A ggplot2 object
#' @export

plot.prcr <- function(x, ...){
    print(x$ggplot_obj)
}

#' Concise summary of prcr cluster solution
#' @details Prints a concise summary of prcr cluster solution
#' @param object A `prcr` object
#' @param ... Additional arguments
#' @export

summary.prcr <- function(object, ...){
    cat(paste0(attributes(object)$n_profiles,
               " cluster solution (R-squared = ", 
               round(object$r_squared, 3), ")\n\n"))
    cat("Profile n and means:\n\n")
    print(object$clustered_processed_data)
}

#' Prints details of prcr cluster solution
#' @details Prints details of of prcr cluster solution
#' @param x A `prcr` object
#' @param ... Additional arguments
#' @export

print.prcr <- function(x, ...){
    cat("$clustered_processed_data\n\n")
    print(x$clustered_processed_data)
    cat("\n")
    cat("$clustered_raw_data\n\n")
    print(x$clustered_raw_data)
}