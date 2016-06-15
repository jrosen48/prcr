# primary_functions.R

#' Pre-processing function to prepare data for subsequent analysis
#'@param raw_data data frame or matrix of any dimensions with numeric data
#'@param method_of_centering string indicating variable-wise centering, options include "grand" (for grand mean centering), "group" (for group mean centering; requires a grouping vector, described next) and "raw", which does not center the variables
#'@param grouping_vector a vector indicating how the cases are to be grouped for group mean centering
#'@param to_standardize boolean indicating whether to standardize (TRUE) or not (FALSE)
#'@export

prepare_data <- function(raw_data, method_of_centering = "raw", grouping_vector = NULL, to_standardize = F){
    cases_to_keep <- complete.cases(raw_data_matrix) # to use later for comparing function to index which cases to keep
    data_tmp <- raw_data_matrix[cases_to_keep, ] # removes incomplete cases
    print("### Created the following output ... ")
    print("### 1. Prepared data ###")
    print(paste0("### Note: ", table(cases_to_keep)[1], " incomplete cases out of ", sum(table(cases_to_keep)), " cases removed, so ", sum(table(cases_to_keep)) - table(cases_to_keep)[1], " used in subsequent analysis ###"))
    grouping_vector <- grouping_vector[cases_to_keep]
    out <- centering_function(data_tmp, method_of_centering, grouping_vector, to_standardize)
    attributes(out) <- list(method_of_centering = method_of_centering, cases_to_keep = cases_to_keep)
    return(out)
}

#' Create profiles function
#'@param prepared_data output from the prepare_data() function
#'@param n_clusters the number of clusters; specified a priori
#'@param distance_metric metric for calculating the distance matrix used in hierarchical clustering, options include "euclidean", "squared_euclidean", and others (see ?dist() for more details)
#'@param linkage method for combining clusters in hierarchical clustering, options include "complete", "average", and others (see ?hclust() for details)
#'@export

create_profiles <- function(prepared_data,
                         n_clusters,
                         distance_metric = "squared_euclidean",
                         linkage = "complete") {

    df <- data.frame(matrix(unlist(prepared_data), ncol = length(prepared_data), byrow = F))
    names(df) <- names(prepared_data)
    args <- list(prepared_data, n_clusters, distance_metric, linkage)
    out <- list() # this collects the output
    distance_matrix <- distance_function(df, distance_metric)
    out[[1]] <- hclust(distance_matrix, method = linkage) # hierarhical clustering
    starting_points <- hclust_to_kmeans_function(df, out[[1]], n_clusters)
    out[[2]] <- kmeans_function(df, starting_points) # Fits k-means algorithm using results from hierarchical algorithm as start value
    attributes(out) <- list(n_clusters_attr = n_clusters, data_attr = df, args_attr = args, cases_to_keep_attr = attributes(prepared_data)$cases_to_keep)
    print("### Created the following output ... ")
    print("### 1. Hierarchical cluster analysis output ###")
    print("### 2. K-means custer analysis output ###")

    invisible(out)

}

#' Function to calculate statistics about cluster solution found via cluster_data()
#'@param clustering_output output from cluster_data() function
#'@param names_of_clusters optional names for clusters, useful for interpreting findings
#'@export
#'@import ggplot2

calculate_stats <- function(clustering_output, names_of_clusters = NULL){
    out <- list()
    # this function takes a list, clustering output, from the cluster_data function
    options(max.print = 100000)
    out[[1]] <- dissim_function(clustering_output[[1]]) # agglomeration schedule - currently out of order
    out[[2]] <- as.dendrogram(clustering_output[[1]]) # dendrogram
    out[[3]] <- cutree(clustering_output[[1]], attributes(clustering_output)$n_clusters_attr) # hclust assignment
    out[[4]] <- clValid::dunn(distance = NULL, clusters = out[[3]], Data = attributes(clustering_output)$data_attr , method = "euclidean")
    out[[5]] <- clustering_output[[2]]$cluster # kmeans assignment
    out[[6]] <- (clustering_output[[2]]$totss - sum(clustering_output[[2]]$withinss)) / clustering_output[[2]]$totss # proportion of variance explained
    out[[7]] <- clValid::dunn(distance = NULL, clusters = out[[5]], Data = attributes(clustering_output)$data_attr, method = "euclidean")
    out[[8]] <- manova_function(attributes(clustering_output)$data_attr, out[[5]], names_of_clusters)
    out[[9]] <- cluster_freq_function(attributes(clustering_output)$data_attr, attributes(clustering_output)$n_clusters_attr, clustering_output[[2]], names_of_clusters)
    out[[10]] <- cluster_plot_function(out[[9]])
    out[[11]] <- clValid::connectivity(clusters = out[[5]], Data = attributes(clustering_output)$data_attr)

    attributes(out) <- list(n_clusters_attr = attributes(clustering_output)$n_clusters_attr, data_attr = prepared_data, args_attr = args, cases_to_keep = attributes(clustering_output)$cases_to_keep)

    print("### Created the following output ... ")
    print("### 1. Hierarchical cluster analysis diagnostics: Agglomeration schedule ###")
    print("### 2. Hierarchical cluster analysis diagnostics: Dendrogram ###")
    print("### 3. Hierarchical cluster analysis assignments ###")
    print("### 4. Hierarchical cluster analysis diagnostics: Dunn Index ###")
    print("### 5. K-means cluster analysis assignments ###")
    print("### 6. K-means cluster analysis diagnostics: Proportion of variance explained (R^2) ###")
    print("### 7. K-means cluster analysis diagnostics: Dunn Index ###")
    print("### 8. Overall diagnostics: MANOVA ###")
    print("### 9. Overall output: Cluster centroids ###")
    print("### 10. Overall output: ggplot2 object for plot of cluster centroids ###")

    invisible(out)
}
#
# cross_validate <- function(x){
#
#     process()
#
# }
#
# compare_cluster_statistics <- function(args, vars_to_vary = NULL){ # can also be method_of_centering (and grouping vector) and to_standardize for now
#     args_tmp <- attributes(output)$args_attr
#     if (is.null(vars_to_vary)) {
#         out <- cluster_data(args_tmp[[1]], args_tmp[[2]], args_tmp[[3]], args_tmp[[4]])
#     }
#     if (vars_to_vary == tolower("n_clusters")) {
#         out <- data.frame(proportion_of_variance_explained = rep(0, 5),
#                           dunn_index = rep(0, 5),
#                           connectivity = rep(0, 5))
#         for (i in 1:5){
#             print(paste0("### Preparing ", i, "/", 5, " cluster solutions ###"))
#             tmp <- cluster_data(args_tmp[[1]], (i + 3), args_tmp[[3]], args_tmp[[4]])
#             tmp <- calculate_stats(tmp)
#             out$proportion_of_variance_explained[i] <- tmp[[6]]
#             out$dunn_index[i] <- tmp[[7]]
#             out$connectivity[i] <- tmp[[11]]
#         }
#         tmp <- sapply(out, function(x) round(x, 3))
#         row.names(tmp) <- paste0(4:8, " clusters")
#     }
#     out_list <- list()
#     out_list[[1]] <- tmp
#     out_list[[2]] <- NULL
#     return(out_list)
# }

#' Function to explore frequency of clusters across select factors
#'@param cluster_assignments cluster assignments from calculate_stats() function, in particular the fifth list item from its output
#'@param cases_to_keep cases to keep from calculates_stats() function, in particular the attribute "cases_to_keep" from its output
#'@param factor_data_frame data frame of select factors
#'@param factor_to_explore specific factor to explore
#'@param variable_to_find_proportion variable to normalize clusters as a unit of analysis
#'@param cluster_names optional names for clusters, useful for interpreting findings
#'@details Performs the clustering half of the process, including assembling
#'  and cleaning the corpus, deviationalizing and clustering.
#'@export

explore_factors <- function(cluster_assignments, cases_to_keep, factor_data_frame, factor_to_explore, variable_to_find_proportion = NULL, cluster_names = NULL){
    out <- list()
    data <- merge_assignments_and_factors(cluster_assignments, cases_to_keep, factor_data_frame)
    
    dummy_coded_data <- dummmy_code_cluster_assignments(data)
    out[[1]] <- create_crosstab(data, factor_to_explore)
    out[[2]] <- create_raw_data(dummy_coded_data, factor_to_explore, variable_to_find_proportion)
    out[[3]] <- create_processed_data(out[[2]], factor_to_explore, variable_to_find_proportion)
    out[[4]] <- create_plot_to_explore_factors(out[[3]], factor_to_explore, cluster_names)
    out[[5]] <- find_n(out[[2]], factor_to_explore)
    out[[6]] <- create_compare_anova(out[[2]], variable_to_find_proportion, cluster_names, factor_to_explore)
    # out[[7]] <- create_compare_manova()
    
    print("### Created the following output ... ")
    print("### 1. Comparison table ###")
    print("### 2. Processed data: Raw ###")
    print("### 3. Processed data: Summary ###")
    print("### 4. ggplot2 object  ###")
    print("### 5. Number by factor  ###")
    print("### 6. ANOVA [[1]] and Tukey HSD [[2]] ###")
    # print("### 7. MANOVA ###")

    invisible(out)
}

cross_validate <- function(x){
    process()
}

# compare_cluster_statistics <- function(args, vars_to_vary = NULL){ # can also be method_of_centering (and grouping vector) and to_standardize for now
#     args_tmp <- attributes(output)$args_attr
#     if (is.null(vars_to_vary)) {
#         out <- cluster_data(args_tmp[[1]], args_tmp[[2]], args_tmp[[3]], args_tmp[[4]])
#     }
#     if (vars_to_vary == tolower("n_clusters")) {
#         out <- data.frame(proportion_of_variance_explained = rep(0, 5),
#                           dunn_index = rep(0, 5),
#                           connectivity = rep(0, 5))
#         for (i in 1:5){
#             print(paste0("### Preparing ", i, "/", 5, " cluster solutions ###"))
#             tmp <- cluster_data(args_tmp[[1]], (i + 3), args_tmp[[3]], args_tmp[[4]])
#             tmp <- calculate_stats(tmp)
#             out$proportion_of_variance_explained[i] <- tmp[[6]]
#             out$dunn_index[i] <- tmp[[7]]
#             out$connectivity[i] <- tmp[[11]]
#         }
#         tmp <- sapply(out, function(x) round(x, 3))
#         row.names(tmp) <- paste0(4:8, " clusters")
#     }
#     out_list <- list()
#     out_list[[1]] <- tmp
#     out_list[[2]] <- NULL
#     return(out_list)
# }
