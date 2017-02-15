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
    # This function processes the output from the hierarchical clustering to be used as starting points for the kmeans clustering
    cut_hclust <- stats::cutree(out, n_clusters) # cuts the results of the hierarchical cluster at the specified # of clusters
    clusters_list <- list()
    for (i in seq(n_clusters)){
        clusters_list[[i]] <- data[cut_hclust == i,]
    }
    ordered_clusters <- list()
    cluster_freqs <- list()
    for (i in seq(length(clusters_list))){
        ordered_clusters[[i]] <- colSums(as.matrix(clusters_list[[i]]) / nrow(clusters_list[[i]]))
        cluster_freqs[[i]] <- ordered_clusters[[i]]
    }
    return(cluster_freqs)
}

kmeans_function <- function(data, cluster_freqs){
    start <- data.frame(matrix(unlist(cluster_freqs), nrow=length(cluster_freqs[[1]]), byrow = T), stringsAsFactors = F)
    start <- as.matrix(start)
    start <- t(start)
    return(stats::kmeans(data, start))
}

prepare_data <- function(df, to_center, to_scale){
    cases_to_keep <- complete.cases(df) # to use later for comparing function to index which cases to keep
    df_wo_incomplete_cases <- as.matrix(df[cases_to_keep, ]) # removes incomplete cases
    prepared_data <- list()
    prepared_data[[1]] <- tibble::as_tibble(scale(df_wo_incomplete_cases, to_center, to_scale))
    names(prepared_data)[[1]] <- "prepared_tibble"
    class(prepared_data) <- c("prcr")
    attributes(prepared_data)$cases_to_keep <- cases_to_keep
    message("Prepared data: Removed ", sum(!cases_to_keep), " incomplete cases")
    invisible(prepared_data)
}

cluster_observations <- function(prepared_data,
                                 n_clusters,
                                 distance_metric,
                                 linkage){
    distance_matrix <- distance_function(prepared_data[[1]], distance_metric)
    clustered_data <- prepared_data
    clustered_data[[2]] <- hclust(distance_matrix, method = linkage) # hierarhical clustering
    names(clustered_data)[[2]] <- "hierarchical_clustering_output"
    starting_points <- hclust_to_kmeans_function(prepared_data[[1]], clustered_data[[2]], n_clusters)
    clustered_data[[3]] <- kmeans_function(prepared_data[[1]], starting_points) # Fits k-means algorithm with hierarchical vals as start value
    names(clustered_data)[[3]] <- "kmeans_clustering_output"
    attributes(clustered_data)$n_clusters <- n_clusters
    message("Clustered data: Using a ", n_clusters, " cluster solution")
    return(clustered_data)
}

calculate_statistics <- function(clustered_data){
    clustering_stats <- clustered_data
    clustering_stats[[4]] <- sum(clustering_stats[[3]]$withinss) / sum(clustering_stats[[3]]$totss + sum(clustering_stats[[3]]$withinss))
    names(clustering_stats)[[4]] <- "r_squared"
    clustering_stats[[5]] <- tibble::as_tibble(data.frame(clustering_stats[[1]], cluster = clustering_stats[[3]]$cluster))
    names(clustering_stats)[[5]] <- "clustered_raw_data"
    cluster_centroids <- tibble::as_tibble(clustering_stats[[3]]$centers)
    cluster_centroids$Cluster <- paste0("Cluster ", 1:nrow(cluster_centroids), " (", clustering_stats[[3]]$size," obs.)")
    clustering_stats[[6]] <- dplyr::select(cluster_centroids, Cluster, dplyr::everything())
    names(clustering_stats)[[6]] <- "clustered_processed_data"
    df_to_plot <- tidyr::gather(clustering_stats[[6]], Variable, Value, -Cluster)

    p <- ggplot2::ggplot(df_to_plot, ggplot2::aes(x = Variable, y = Value, fill = Cluster)) +
            ggplot2::geom_col(position = "dodge") +
            ggplot2::theme(legend.title = ggplot2::element_blank()) # this should be y[[7]]

    clustering_stats[[7]] <- p
    names(clustering_stats)[[7]] <- "ggplot_obj"
    message("Calculated statistics: R-squared = ", round(clustering_stats[[4]], 3))
    invisible(clustering_stats)
}

#' Create profiles of observed variables using two-step cluster analysis
#' @details Function to create a specified number of profiles of observed variables using a two-step (hierarchical and k-means) cluster analysis. 
#' @param df A `data.frame` with two or more columns with continuous variables
#' @param n_clusters The specified number of profiles to be found for the clustering solution
#' @return A list containing the prepared data, the output from the hierarchical and k-means cluster analysis, the r-squared value, raw clustered data, processed clustered data of cluster centroids, and a ggplot object.
#' @examples
#' df <- mtcars[, c("disp", "hp", "wt")]
#' create_profiles(df, 2, to_scale = T)
#' @export

create_profiles <- function(df, 
                            n_clusters, 
                            to_center = FALSE,
                            to_scale = FALSE,
                            distance_metric = "squared_euclidean",
                            linkage = "complete"){
    x <- prepare_data(df, to_center, to_scale)
    y <- cluster_observations(x, n_clusters, distance_metric, linkage)
    z <- calculate_statistics(y)
    print(z$ggplot_plot)
    invisible(z)
}
