library(tidyverse)

# functions_new.R

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
    if (distance_metric == "squared_euclidean") {
        distance <- stats::dist(x, method = "euclidean")
        distance <- distance ^ 2
    } else {
        distance <- stats::dist(x, method = distance_metric)
    }
    return(distance)
}

distance_function <- function(x, distance_metric){
    if (distance_metric != "squared_euclidean") {
        distance <- stats::dist(x, method = distance_metric)
    } 
    else {
        distance <- stats::dist(x, method = "euclidean")
        distance <- distance ^ 2
    }
    
    distance
}

hclust_to_kmeans_function <- function(df, hclust_output, n_profiles){
    h_clust_classification <- stats::cutree(hclust_output, n_profiles)
    df %>% 
        mutate(classification = h_clust_classification) %>% 
        group_by(classification) %>% 
        summarize_all(mean)
}

possibly_kmeans <- possibly(kmeans, NA)

# prcr <- function() {
#     structure(data.frame(), class = "prcr")
# }

prepare_data <- function(df, ..., to_center, to_scale){
    if (!is.data.frame(df)) stop("df must be a data.frame (or tibble)")
    df <- tibble::as_tibble(df)
    df_ss <- dplyr::select(df, ...)
    cases_to_keep <- stats::complete.cases(df_ss) # to use later for comparing function to index which cases to keep
    df_complete <- df_ss[cases_to_keep, ] # removes incomplete cases
    
    # prepared_data[[10]] <- df_ss_wo_incomplete_cases
    
    if (to_center == TRUE & to_scale == TRUE) {
        df_complete <- dplyr::mutate_all(df_complete, center_and_scale_vector)
    } else if (to_center == TRUE & to_scale == FALSE) {
        df_complete <- dplyr::mutate_all(df_complete, center_vector)
    } else if (to_center == FALSE & to_scale == TRUE) {
        df_complete <- dplyr::mutate_all(df_complete, scale_vector)
    } else if (to_center == FALSE & to_scale == FALSE) {
        df_complete <- df_complete
    }
    
    
    # class(df_complete)
    #names(prepared_data)[[1]] <- "prepared_tibble"
    #class(prepared_data) <- c("prcr")
    
    attributes(df_complete)$orig_data <- df
    attributes(df_complete)$cases_to_keep <- cases_to_keep
    attributes(df_complete)$.data <- df_ss[cases_to_keep, ]
    
    # prepared_data[[2]] <- df_wo_incomplete_cases
    # names(prepared_data)[[2]] <- ".data"
    row.names(df_complete) <- NULL
    
    class(df_complete) <- c("tbl_df", "tbl", "data.frame", "prcr")
    
    message("Prepared data: Removed ", sum(!cases_to_keep), " incomplete cases")
    return(df_complete)
}

is.kmeans <- function(x) inherits(x, "kmeans")

cluster_observations <- function(df,
                                 ...,
                                 n_profiles,
                                 center_data = TRUE,
                                 scale_data = TRUE,
                                 distance_metric = "euclidean",
                                 linkage = "complete",
                                 to_return = "clustered_data"){
    df <- prepare_data(df, ..., to_center = center_data, to_scale = scale_data)
    
    distance_matrix <- distance_function(df, distance_metric)
    clustered_data <- stats::hclust(distance_matrix, method = linkage) # hierarhical clustering
    message(paste0("Hierarchical clustering carried out on: ", nrow(df), " cases"))
    #names(clustered_data)[[3]] <- "hierarchical_clustering_output"
    starting_centroids <- hclust_to_kmeans_function(df, clustered_data, n_profiles = n_profiles)
    
    kmeans_output <- possibly_kmeans(df, dplyr::select(starting_centroids, -classification)) # Fits k-means algorithm with hierarchical vals as start value
    
    if (length(kmeans_output) == 1) {
        return(kmeans_output)
    }
    
    if (kmeans_output$iter == 1) {
        message(paste0("K-means algorithm converged: ", kmeans_output$iter, " iteration"))
    } else {
        message(paste0("K-means algorithm converged: ", kmeans_output$iter, " iterations"))
    }
    #names(kmeans_output) <- "kmeans_clustering_output"
    if (is.kmeans(kmeans_output)) {
        attributes(clustered_data)$n_profiles <- n_profiles
        message("Clustered data: Using a ", n_profiles, " cluster solution")    
    }
    
    # NEED TO RENAME
    
    tmp <- data_frame(row_names = as.character(1:length(attributes(df)$cases_to_keep)),
                      condition = attributes(df)$cases_to_keep)
    
    tmp_clustered_vars <- filter(tmp, condition == TRUE)
    
    tmp_clustered_vars <- data_frame(row_names = tmp_clustered_vars$row_names,
                                     profile = kmeans_output$cluster)
    
    tmp_all_vars <- left_join(tmp, tmp_clustered_vars, by = "row_names")
    
    attributes(df)$orig_data <- tibble::rownames_to_column(attributes(df)$orig_data, var = "row_names")
    
    # NEED TO ADD ATTRIBUTES TO OTHER CONDITIONS
    
    if (to_return == "clustered_data") {
        return_df <- df
        return_df$profile <- tmp_clustered_vars$profile
    } else if (to_return == "raw_selected_data") {
        return_df <- attributes(df)$.data
        return_df$profile <- tmp_clustered_vars$profile
    } else if (to_return == "raw_unprocessed_data") {
        return_df <- left_join(tmp_all_vars, attributes(df)$orig_data, by = "row_names")
        return_df <- select(return_df, everything(), -row_names, -condition)
        return_df <- select(return_df, everything(), -profile, profile)
        #return_df$profile <- tmp$profile
    }
    
    return_df
    
}

x <- cluster_observations(iris, 
                          Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
                          n_profiles = 3,
                          to_return="raw_selected_data")

# Test code

iris %>% 
    cluster_observations(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width,
                         n_profiles = 3)


