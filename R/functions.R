distance_function <- function(x, distance_metric){
    if (distance_metric != "squared_euclidean") {
        distance <- stats::dist(x, method = distance_metric)
    } else {
        distance <- stats::dist(x, method = "euclidean")
        distance <- distance ^ 2
    }
    return(distance)
}

hclust_to_kmeans_function <- function(data, out, n_clusters){
    cut_hclust <- stats::cutree(out, n_clusters) # cuts the results of the hierarchical cluster at the specified # of clusters
    clusters_list <- list() # rewrite this to not loop (using purrr)
    for (i in seq(n_clusters)) { 
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

select_vars <- function(df, ...){
    if (!is.data.frame(df)) stop("df must be a data.frame (or tibble)")
    df_ss <- dplyr::select(df, ...)
    return(df_ss)
}

prepare_data <- function(df, to_center, to_scale){
    df_ss <- select_vars(df)
    cases_to_keep <- stats::complete.cases(df_ss) # to use later for comparing function to index which cases to keep
    df_wo_incomplete_cases <- df_ss[cases_to_keep, ] # removes incomplete cases
    prepared_data <- prcr()
    prepared_data[[1]] <- tibble::as_tibble(scale(as.matrix(df_wo_incomplete_cases), to_center, to_scale))
    names(prepared_data)[[1]] <- "prepared_tibble"
    class(prepared_data) <- c("prcr")
    attributes(prepared_data)$cases_to_keep <- cases_to_keep
    message("Prepared data: Removed ", sum(!cases_to_keep), " incomplete cases")
    return(prepared_data)
}

cluster_observations <- function(prepared_data,
                                 n_clusters,
                                 distance_metric,
                                 linkage){
    distance_matrix <- distance_function(prepared_data[[1]], distance_metric)
    clustered_data <- prepared_data
    clustered_data[[2]] <- stats::hclust(distance_matrix, method = linkage) # hierarhical clustering
    message(paste0("Hierarchical clustering carried out on: ", nrow(prepared_data[[1]]), " cases"))
    names(clustered_data)[[2]] <- "hierarchical_clustering_output"
    starting_points <- hclust_to_kmeans_function(prepared_data[[1]], clustered_data[[2]], n_clusters)
    clustered_data[[3]] <- kmeans_function(prepared_data[[1]], starting_points) # Fits k-means ithm with hierarchical vals as start value
    if (clustered_data[[3]]$iter == 1) {
        message(paste0("K-means algorithm converged: ", clustered_data[[3]]$iter, " iteration"))
    } else {
        message(paste0("K-means algorithm converged: ", clustered_data[[3]]$iter, " iterations"))
    }
    names(clustered_data)[[3]] <- "kmeans_clustering_output"
    if (class(clustered_data[[3]]) == "kmeans") {
        attributes(clustered_data)$n_clusters <- n_clusters
        message("Clustered data: Using a ", n_clusters, " cluster solution")    
    }
    return(clustered_data)
}

calculate_statistics <- function(clustered_data){
    clustering_stats <- clustered_data
    clustering_stats[[4]] <- clustering_stats[[3]]$betweenss / clustering_stats[[3]]$totss
    names(clustering_stats)[[4]] <- "r_squared"
    clustering_stats[[5]] <- tibble::as_tibble(data.frame(clustering_stats[[1]], cluster = clustering_stats[[3]]$cluster))
    names(clustering_stats)[[5]] <- "clustered_raw_data"
    cluster_centroids <- tibble::as_tibble(clustering_stats[[3]]$centers)
    cluster_centroids$Cluster <- paste0("Cluster ", 1:nrow(cluster_centroids), " (", clustering_stats[[3]]$size," obs.)")
    clustering_stats[[6]] <- dplyr::select(cluster_centroids, dplyr::contains("Cluster"), dplyr::everything())
    names(clustering_stats)[[6]] <- "clustered_processed_data"
    
    df_to_plot <- tidyr::gather_(clustering_stats[[6]], key_col = "Variable", value_col = "Value", names(clustering_stats[[6]])[names(clustering_stats[[6]]) != 'Cluster'])
    
    p <- ggplot2::ggplot(df_to_plot, ggplot2::aes(x = df_to_plot$Cluster, y = df_to_plot$Value, fill = df_to_plot$Variable)) +
        ggplot2::geom_col(position = "dodge") +
        ggplot2::theme(legend.title = ggplot2::element_blank()) # this should be y[[7]]
    
    clustering_stats[[7]] <- p
    names(clustering_stats)[[7]] <- "ggplot_obj"
    message("Calculated statistics: R-squared = ", round(clustering_stats[[4]], 3))
    return(clustering_stats)
}

#' Create profiles of observed variables using two-step cluster analysis
#' @details Function to create a specified number of profiles of observed variables using a two-step (hierarchical and k-means) cluster analysis. 
#' @param dta.frame` with two or more columns with continuous variables
#' @param n_clusters The specified number of profiles to be found for the clustering solution
#' @param to_center Boolean (TRUE or FALSE) for whether to center the raw data with M = 0
#' @param to_scale Boolean (TRUE or FALSE) for whether to scale the raw data with SD = 1
#' @param distance_metric Distance metric to use for hierarchical clustering; "squared_euclidean" is default but more options are available (see ?hclust)
#' @param linkage Linkage method to use for hierarchical clustering; "complete" is default but more options are available (see ?dist)
#' @return A list containing the prepared data, the output from the hierarchical and k-means cluster analysis, the r-squared value, raw clustered data, processed clustered data of cluster centroids, and a ggplot object.
#' @examples
#' df <- mtcars[, c("disp", "hp", "wt")]
#' create_profiles(df, 2, to_scale = TRUE)
#' @export

create_profiles <- function(df, 
                            n_clusters, 
                            to_center = FALSE,
                            to_scale = FALSE,
                            distance_metric = "squared_euclidean",
                            linkage = "complete"){
    prepped_data <- prepare_data(df, to_center, to_scale)
    y <- cluster_observations(prepped_data, n_clusters, distance_metric, linkage)
    if (class(y[[3]]) == "kmeans") {
        z <- calculate_statistics(y)
        return(z)
    } else {
        y[[4]] <- NA
        names(y)[[4]] <- "r_squared"
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
    cat(paste0(attributes(object)$n_clusters,
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