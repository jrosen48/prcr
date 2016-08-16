# primary_functions.R

#' Pre-processing function to prepare data for subsequent analysis
#'@param raw_data_matrix data frame or matrix of any dimensions with numeric data
#'@param method_of_centering string indicating variable-wise centering, options include "grand" (for grand mean centering), "group" (for group mean centering; requires a grouping vector, described next) and "raw", which does not center the variables
#'@param grouping_vector a vector indicating how the cases are to be grouped for group mean centering
#'@param to_standardize boolean indicating whether to standardize (TRUE) or not (FALSE)
#'@param remove_uv_outliers boolean indicating whether to remove (TRUE) univariate outlier or not (FALSE)
#'@param remove_mv_outliers boolean indicating whether to remove (TRUE) multivariate outlier or not (FALSE)
#'@param print_status boolean indicating whether to print information about the output (TRUE) or to not print information about the output (FALSE)
#'@export

prepare_data <- function(raw_data_matrix, method_of_centering = "raw", grouping_vector = NULL, to_standardize = F, remove_uv_outliers = F, remove_mv_outliers = F, print_status = F){
    cases_to_keep <- complete.cases(raw_data_matrix) # to use later for comparing function to index which cases to keep
    removed_obs_df <- removed_obs_df_maker(raw_data_matrix, cases_to_keep)
    data_tmp <- raw_data_matrix[cases_to_keep, ] # removes incomplete cases
    print("### Created the following output ... ")
    print("### 1. Prepared data ###")
    if(print_status == T){
        if(length(table(cases_to_keep)) > 1){
            print(paste0("### Note: ", table(cases_to_keep)[1], " incomplete cases out of ", sum(table(cases_to_keep)), " total cases removed, so ", sum(table(cases_to_keep)) - table(cases_to_keep)[1], " used in subsequent analysis ###"))
        } else{
            print(paste0("### Note: 0 incomplete cases out of ", nrow(data_tmp), " total cases removed, so ", nrow(data_tmp), " used in subsequent analysis ###"))
        }
    }
    if (remove_uv_outliers == T){
        tmp1 <- remove_uv_main_func(data_tmp, removed_obs_df, cases_to_keep, print_status)
        data_tmp <- tmp1[[1]]
        removed_obs_df <- tmp1[[2]]
    }
    if(any(as.character(removed_obs_df$reason_removed) == "univariate_outlier", na.rm = T)){
        found_uv_outlier_bool <- T
    } else{
        found_uv_outlier_bool <- F
    }
    if (remove_mv_outliers == T){
        tmp2 <- remove_mv_main_func(data_tmp, removed_obs_df, cases_to_keep, found_uv_outlier_bool, uv_outliers = tmp1, print_status)
        data_tmp <- tmp2[[1]]
        removed_obs_df <- tmp2[[2]]
    }
    grouping_vector <- grouping_vector[cases_to_keep]
    out <- centering_function(as.data.frame(data_tmp), method_of_centering, grouping_vector, to_standardize)
    cases_to_keep = row.names(raw_data_matrix) %in% removed_obs_df$row[is.na(removed_obs_df$reason_removed)]
    attributes(out) <- list(uncentered_cleaned_data = data_tmp, 
                            method_of_centering = method_of_centering, cases_to_keep = cases_to_keep, 
                            cases_removed_df = removed_obs_df[, 2:5], variable_names = names(raw_data_matrix))
    if(print_status == T){
        print("### Note. Print the cases_removed_df attribute to view cases removed ###")
    }
    return(out)
}

#' Create profiles function
#'@param prepared_data output from the prepare_data() function
#'@param n_clusters the number of clusters; specified a priori
#'@param distance_metric metric for calculating the distance matrix used in hierarchical clustering, options include "euclidean", "squared_euclidean", and others (see ?dist() for more details)
#'@param linkage method for combining clusters in hierarchical clustering, options include "complete", "average", and others (see ?hclust() for details)
#'@param print_status boolean indicating whether to print information about the output (TRUE) or to not print information about the output (FALSE)
#'@export

create_profiles <- function(prepared_data,
                            n_clusters,
                            distance_metric = "squared_euclidean",
                            linkage = "complete", 
                            print_status = T) {
    df <- data.frame(matrix(unlist(prepared_data), ncol = length(prepared_data), byrow = F))
    names(df) <- names(prepared_data)
    args <- list(prepared_data, n_clusters, distance_metric, linkage)
    out <- list() # this collects the output
    distance_matrix <- distance_function(df, distance_metric)
    out[[1]] <- hclust(distance_matrix, method = linkage) # hierarhical clustering
    starting_points <- hclust_to_kmeans_function(df, out[[1]], n_clusters)
    out[[2]] <- kmeans_function(df, starting_points) # Fits k-means algorithm using results from hierarchical algorithm as start value
    attributes(out) <- list(cleaned_data = df,
                            uncentered_cleaned_data = attributes(prepared_data)$uncentered_cleaned_data, 
                            n_clusters_attr = n_clusters, data_attr = df, args_attr = args, 
                            cases_to_keep_attr = attributes(prepared_data)$cases_to_keep, 
                            variable_names = attributes(prepared_data)$variable_names)
    if(print_status == T){
        print("### Created the following output ... ")
        print("### 1. Hierarchical cluster analysis output ###")
        print("### 2. K-means cluster analysis output ###")
    }
    invisible(out)
}

#' Function to calculate statistics about cluster solution found via cluster_data()
#'@param clustering_output output from cluster_data() function
#'@param cluster_names optional names for clusters, useful for creating plot
#'@param print_status boolean indicating whether to print information about the output (TRUE) or to not print information about the output (FALSE)
#'@export
#'@import ggplot2

calculate_stats <- function(clustering_output, 
                            cluster_names = NULL, 
                            to_standardize = F,
                            print_status = T,
                            plot_uncentered_data = F){
    out <- list()
    variable_names <- attributes(clustering_output)$variable_names
    # this function takes a list, clustering output, from the cluster_data function
    options(max.print = 100000)
    out[[1]] <- dissim_function(clustering_output[[1]]) # agglomeration schedule - currently out of order
    out[[2]] <- clustering_output[[1]] # dendrogram
    out[[3]] <- cutree(clustering_output[[1]], attributes(clustering_output)$n_clusters_attr) # hclust assignment
    out[[4]] <- clustering_output[[2]]$cluster # kmeans assignment
    out[[5]] <- (clustering_output[[2]]$totss - sum(clustering_output[[2]]$withinss)) / clustering_output[[2]]$totss # proportion of variance explained
    out[[6]] <- try_manova(attributes(clustering_output)$data_attr, out[[4]], variable_names)
    if (to_standardize == T){
        tmp <- standardize_function(attributes(clustering_output)$data_attr)
        out[[7]] <- cluster_freq_function(tmp, attributes(clustering_output)$n_clusters_attr, clustering_output[[2]], variable_names)
    } else {
        out[[7]] <- cluster_freq_function(attributes(clustering_output)$data_attr, attributes(clustering_output)$n_clusters_attr, clustering_output[[2]], variable_names)
    }
    out[[8]] <- cluster_plot_function(out[[7]], cluster_names)
    if (plot_uncentered_data == T){
        tmp <- cluster_freq_function(attributes(clustering_output)$uncentered_cleaned_data, attributes(clustering_output)$n_clusters_attr, clustering_output[[2]], variable_names)
        out[[9]] <- cluster_plot_function(tmp, cluster_names)
    }
    attributes(out) <- list(cleaned_data = attributes(clustering_output)$cleaned_data,
                            cluster_names = out[[7]]$Cluster, n_clusters_attr = attributes(clustering_output)$n_clusters_attr, 
                            args_attr = args, 
                            cases_to_keep = attributes(clustering_output)$cases_to_keep,
                            variable_names = attributes(clustering_output)$variable_names)
    if(print_status == T){
        print("### Created the following output ... ")
        print("### 1. Hierarchical cluster analysis diagnostics: Agglomeration schedule ###")
        print("### 2. Hierarchical cluster analysis diagnostics: hclust object to coerce using as.dendrogram then to plot() ###")
        print("### 3. Hierarchical cluster analysis assignments ###")
        print("### 4. K-means cluster analysis assignments ###")
        print("### 5. K-means cluster analysis diagnostics: Proportion of variance explained (R^2) ###")
        print("### 6. Overall diagnostics: MANOVA ###")
        print("### 7. Overall output: Cluster centroids ###")
        print("### 8. Overall output: ggplot2 object for plot of cluster centroids with centered data ###")
        if(plot_uncentered_data == T){
            print("### 9. Overall output: ggplot2 object for plot of cluster centroids with raw data ###")
        }
    }
    invisible(out)
}

#' Function to explore frequency of clusters across select factors
#'@param cluster_assignments cluster assignments from calculate_stats() function, in particular the fifth list item from its output
#'@param cases_to_keep cases to keep from calculates_stats() function, in particular the attribute "cases_to_keep" from its output
#'@param factor_data_frame data frame of select factors
#'@param factor_to_explore specific factor to explore
#'@param variable_to_find_proportion variable to normalize clusters as a unit of analysis
#'@param cluster_names optional names for clusters, useful for interpreting findings
#'@param print_status boolean indicating whether to print information about the output (TRUE) or to not print information about the output (FALSE)
#'@details To explore the frequency of clusters across factors
#'  and cleaning the corpus, deviationalizing and clustering.
#'@export

explore_factors <- function(statistics,
                            factor_data_frame, 
                            factor_to_explore, 
                            variable_to_find_proportion = NULL, 
                            cluster_names = NULL,
                            print_status = T){
    cluster_assignments = statistics[[4]]
    cases_to_keep = attributes(statistics)$cases_to_keep
    variable_names = attributes(statistics)$variable_names
    out <- list()
    factor_data_frame[] <- lapply(factor_data_frame, factor)
    data <- merge_assignments_and_factors(cluster_assignments, cases_to_keep, factor_data_frame)
    data_for_descriptive_stats <- data.frame(attributes(statistics)$cleaned_data, data)
    dummy_coded_data <- dummmy_code_cluster_assignments(data)
    out[[1]] <- create_crosstab(data, factor_to_explore)
    out[[2]] <- create_raw_data(dummy_coded_data, factor_to_explore, variable_to_find_proportion)
    out[[3]] <- create_processed_data(out[[2]], factor_to_explore, variable_to_find_proportion)
    out[[4]] <- create_plot_to_explore_factors(out[[3]], factor_to_explore, cluster_names)
    out[[5]] <- find_n(out[[2]], factor_to_explore)
    out[[6]] <- create_compare_anova(out[[2]], variable_to_find_proportion, cluster_names, factor_to_explore)
    # out[[7]] <- create_compare_manova()
    
    if(print_status == T){
        print("### Created the following output ... ")
        print("### 1. Comparison table ###")
        print("### 2. Processed data: Raw ###")
        print("### 3. Processed data: Summary ###")
        print("### 4. ggplot2 object  ###")
        print("### 5. Number by factor  ###")
        print("### 6. ANOVA [[1]] and Tukey HSD [[2]] ###")
        # print("### 7. MANOVA ###")
    }
    attributes(out) <- list(cleaned_df = data_for_descriptive_stats)
    invisible(out)

}

#' Function to compare the proportion of variance explained for cluster solutions with varying number of clusters
#'@param prepared_data output from the prepare_data() function
#'@param args list of arguments passed to the create_profiles() function, which is returned as an attribute of its output (i.e., attributes(output)$args_attr)
#'@param lower_num integer representing the lower bound of the range of the number of clusters in the cluster solution
#'@param upper_num integer representing the lower bound of the range of the number of clusters in the cluster solution
#'@details Function to compare the proportion of variance explained for cluster solutions with varying number of clusters
#'@export

compare_cluster_statistics <- function(prepared_data, args, lower_num, upper_num){ # can also be method_of_centering (and grouping vector) and to_standardize for now
    if(lower_num == 1) {
        lower_num <- 2
        warning("Cannot find cluster solution with 1 cluster; skipped calculation of cluster solution with 1 cluster")
    }
    tmp_vec <- vector(length = upper_num)
    for (i in lower_num:upper_num){
        tmp <- try_to_cluster(prepared_data, args, i)
        if(!is.character(tmp)){
            tmp <- calculate_stats(tmp, print_status = F)[[5]]
            # print(paste0("### Proportion of variance explained (R^2) = ", round(tmp, 3)))
        } else{
            tmp <- NA
        }
        tmp_vec[i] <- tmp
    }
    number_of_clusters <- 1:upper_num
    proportion_of_variance_explained <- tmp_vec
    number_of_clusters <- number_of_clusters[proportion_of_variance_explained != 0]
    proportion_of_variance_explained <- proportion_of_variance_explained[proportion_of_variance_explained != 0]
    if(any(is.na(proportion_of_variance_explained))) {
        number_of_clusters <- number_of_clusters[!is.na(proportion_of_variance_explained)]
        proportion_of_variance_explained <- proportion_of_variance_explained[!is.na(proportion_of_variance_explained)]
    }
    out <- data.frame(number_of_clusters, proportion_of_variance_explained)
    out_plot <- comparision_of_statistics_plot(out, min(out$number_of_clusters), max(out$number_of_clusters))
    out <- list(out, out_plot)
    print("### Created the following output ... ")
    print("### 1. Data frame of proportion of variance explained by number of clusters ###")
    print("### 2. ggplot object of of proportion of variance explained by number of clusters ###")
    return(out)
}

#' Function to cross-validate the cluster solution using split half or other cross validation 
#'@param prepared_data output from the prepare_data() function
#'@param output output from the create_profiles() function
#'@param k integer for the number of cross-validation attempts
#'@details Function to cross-validate the cluster solution using split half or other cross validation 
#'@export

cross_validate <- function(prepared_data, output, variable_names, k){
    kappa_collector <- vector()
    agree_collector <- vector()
    for (i in 1:k){
        # print(paste0("### Processing cross validation iteration ", i))
        x <- splitting_halves(prepared_data)
        y <- cluster_the_halves(x, attributes(output)$args_attr)
        test <- all(!is.na(y[[1]]) & !is.na(y[[2]]))
        if(test){
            z <- calculate_the_stats(y, variable_names)
            a_assign_star <- find_nearest_centroid(split_halves = x, calculated_stats = z)
            zzz <- calculate_agreement(a_assign_star, z[[1]][4])
            kappa_collector[[i]] <- round(zzz[[1]]$value, 3)
            agree_collector[[i]] <- round(zzz[[2]]$value * .01, 3)
        } else{
            kappa_collector[[i]] <- NA
            agree_collector[[i]] <- NA
        }
        # print(paste0("Kappa: ", kappa_collector[[i]]))
        # print(paste0("Agreement: ", agree_collector[[i]]))
    }
    mean_kappa <- paste0("Mean Kappa for ", k, " attempts: ", round(mean(kappa_collector, na.rm = T), 3))
    mean_agree <- paste0("Mean agreement for ", k, " attempts: ", round(mean(agree_collector, na.rm = T), 3))    
    out <- list(kappa_collector, agree_collector, mean_kappa, mean_agree)
    print("### Created the following output ... ")
    print(paste0("### 1. Vector of Cohen's Kappa of length ", k, " ###"))
    print(paste0("### 2. Vector of agreement of length ", k, " ###"))
    print(paste0("### 3. Mean Cohen's Kappa for ", k, " attempts ###"))
    print(paste0("### 4. Mean agreement for ", k, " attempts  ###"))
    return(out)
}

# Wrapper function to perform all analyses at once - still in development
# prcr <- function(raw_data_matrix, method_of_centering = "raw", grouping_vector = NULL, 
#                  to_standardize = T, remove_uv_outliers = T, remove_mv_outliers = T, print_status = F,
#                  n_clusters, distance_metric = "squared_euclidean", linkage = "complete",
#                  variable_to_find_proportion = NULL, factor_data_frame, factor_to_explore, 
#                  compare_statistics = F, cross_validate = F)