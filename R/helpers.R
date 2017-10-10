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

prcr <- function() {
    structure(list(), class = "prcr")
}

p <- function(df, ..., to_center, to_scale){
    if (!is.data.frame(df)) stop("df must be a data.frame (or tibble)")
    df <- tibble::as_tibble(df)
    df_ss <- dplyr::select(df, ...)
    
    cases_to_keep <- stats::complete.cases(df_ss) # to use later for comparing function to index which cases to keep
    
    # cases_to_keep <- dplyr::data_frame(row_names = 1:nrow(df_ss),
    #                                    keep = cases_to_keep)
    
    df_ss_wo_incomplete_cases <- df_ss[cases_to_keep, ] # removes incomplete cases
    
    prepared_data <- prcr()
    
    # if (remove_mv_outliers == TRUE) {
    #     outliers <- detect_outliers(df_ss_wo_incomplete_cases)
    #     prepared_data[[11]] <- outliers
    #     names(prepared_data[[11]]) <- "outliers"
    #     message("Removed ", length(outliers), " multivariate outliers; view the outliers slot of the output to view the cases removed")
    #     df_ss_wo_incomplete_cases <- df_ss_wo_incomplete_cases[-outliers, ]
    # }
    
    df_wo_incomplete_cases <- df[cases_to_keep, ]
    
    prepared_data[[10]] <- df_ss_wo_incomplete_cases
    names(prepared_data)[[10]] <- "df_with_dummies"
    
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

calculate_statistics <- function(clustered_data, n_profiles, to_center, to_scale, plot_centered_data, plot_raw_data){
    clustering_stats <- clustered_data
    clustering_stats[[10]] <- data.frame(clustering_stats[[10]], cluster = clustering_stats[[4]]$cluster)
    clustering_stats[[5]] <- clustering_stats[[4]]$betweenss / clustering_stats[[4]]$totss
    names(clustering_stats)[[5]] <- "r_squared"
    
    clustering_stats[[6]] <- tibble::as_tibble(data.frame(clustering_stats[[1]], cluster = clustering_stats[[4]]$cluster))
    names(clustering_stats)[[6]] <- "clustered_raw_data"
    
    cluster_centroids <- tibble::as_tibble(clustering_stats[[4]]$centers)
    
    cluster_centroids$Cluster <- paste0("Profile ", 1:nrow(cluster_centroids), " (", clustering_stats[[4]]$size," obs.)")
    
    clustering_stats[[7]] <- dplyr::select(cluster_centroids, dplyr::contains("Cluster"), dplyr::everything())
    names(clustering_stats)[[7]] <- "clustered_processed_data"
    
    df_to_plot <- tidyr::gather_(clustering_stats[[7]], key_col = "Variable", value_col = "Value", names(clustering_stats[[7]])[names(clustering_stats[[7]]) != 'Cluster'])
    
    p <- ggplot2::ggplot(df_to_plot, ggplot2::aes(x = df_to_plot$Cluster, y = df_to_plot$Value, fill = df_to_plot$Variable)) +
        ggplot2::geom_col(position = "dodge") +
        ggplot2::theme(legend.title = ggplot2::element_blank()) +
        ggplot2::theme(text = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::xlab(NULL) +
        ggplot2::ylab("Value")
    
    if (plot_centered_data == T) {
        tmp <- sapply(clustering_stats[[6]][, -(ncol(clustering_stats[[6]]))], function(x) scale(x, center = plot_centered_data))
        tmp_1 <- clustering_stats[[6]][, ncol(clustering_stats[[6]])]
        df_to_plot <- data.frame(tmp, tmp_1)
        df_to_plot <- dplyr::rename(df_to_plot, Cluster = df_to_plot$cluster)
        df_to_plot <- tidyr::gather_(df_to_plot, key_col = "Variable", value_col = "Value", names(clustering_stats[[7]])[names(clustering_stats[[7]]) != 'Cluster'])
        df_to_plot <- dplyr::group_by(df_to_plot, df_to_plot$Cluster, df_to_plot$Variable)
        df_to_plot <- dplyr::summarise(df_to_plot, Value = mean(df_to_plot$Value), n = dplyr::n())
        df_to_plot$Cluster <- paste0("Profile ", df_to_plot$Cluster, " (", df_to_plot$n, " obs.)")
        
        p <- ggplot2::ggplot(df_to_plot, ggplot2::aes(x = df_to_plot$Cluster, y = df_to_plot$Value, fill = df_to_plot$Variable)) +
            ggplot2::geom_col(position = "dodge") +
            ggplot2::theme(legend.title = ggplot2::element_blank()) +
            ggplot2::theme(text = ggplot2::element_text(angle = 45, hjust = 1)) +
            ggplot2::xlab(NULL) +
            ggplot2::ylab("Value")
    }
    
    if (plot_raw_data == T) {
        
        message(paste0("Raw data is plotted, although to_center == ", to_center, " and to_scale == ", to_scale, "."))
        
        df_to_plot <- tidyr::gather_(clustering_stats[[10]], key_col = "Variable", value_col = "Value", names(clustering_stats[[7]])[names(clustering_stats[[7]]) != 'Cluster'])
        df_to_plot <- dplyr::rename(df_to_plot, Cluster = df_to_plot$cluster)
        df_to_plot <- dplyr::group_by(df_to_plot, df_to_plot$Cluster, df_to_plot$Variable)
        df_to_plot <- dplyr::summarise(df_to_plot, Value = mean(df_to_plot$Value), n = dplyr::n())
        df_to_plot$Cluster <- paste0("Profile ", df_to_plot$Cluster, " (", df_to_plot$n, " obs.)")
        
        p <- ggplot2::ggplot(df_to_plot, ggplot2::aes(x = df_to_plot$Cluster, y = df_to_plot$Value, fill = df_to_plot$Variable)) +
            ggplot2::geom_col(position = "dodge") +
            ggplot2::theme(legend.title = ggplot2::element_blank()) +
            ggplot2::theme(text = ggplot2::element_text(angle = 45, hjust = 1)) +
            ggplot2::xlab(NULL) +
            ggplot2::ylab("Value")
        
    }
    
    clustering_stats[[8]] <- p
    names(clustering_stats)[[8]] <- "ggplot_obj"
    message("Calculated statistics: R-squared = ", round(clustering_stats[[5]], 3))
    tmp <- as.data.frame(stats::model.matrix(~ factor(clustering_stats[[4]]$cluster) - 1))
    names(tmp) <- paste0("cluster_", 1:n_profiles)
    
    clustering_stats[[9]] <- dplyr::bind_cols(clustering_stats[[2]], tmp)
    
    names(clustering_stats)[[9]] <- "data_with_dummy_codes"
    clustering_stats[[2]]$cluster <- clustering_stats[[4]]$cluster
    return(clustering_stats)
}