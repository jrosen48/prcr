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

p <- function(df, ..., to_center, to_scale){
    if (!is.data.frame(df)) stop("df must be a data.frame (or tibble)")
    df <- tibble::as_tibble(df)
    df_ss <- dplyr::select(df, ...)
    cases_to_keep <- stats::complete.cases(df_ss) # to use later for comparing function to index which cases to keep
    df_ss_wo_incomplete_cases <- df_ss[cases_to_keep, ] # removes incomplete cases
    prepared_data <- prcr()
    df_wo_incomplete_cases <- df[cases_to_keep, ]
    prepared_data[[10]] <- df_ss_wo_incomplete_cases
    
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
        df_to_plot <- dplyr::rename(df_to_plot, Cluster = cluster)
        df_to_plot <- tidyr::gather_(df_to_plot, key_col = "Variable", value_col = "Value", names(clustering_stats[[7]])[names(clustering_stats[[7]]) != 'Cluster'])
        df_to_plot <- dplyr::group_by(df_to_plot, Cluster, Variable)
        df_to_plot <- dplyr::summarise(df_to_plot, Value = mean(Value), n = n())
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
        df_to_plot <- dplyr::rename(df_to_plot, Cluster = cluster)
        df_to_plot <- dplyr::group_by(df_to_plot, Cluster, Variable)
        df_to_plot <- dplyr::summarise(df_to_plot, Value = mean(Value), n = n())
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

#' Create profiles of observed variables using two-step cluster analysis
#' @details Function to create a specified number of profiles of observed variables using a two-step (hierarchical and k-means) cluster analysis. 
#' @param df with two or more columns with continuous variables
#' @param ... unquoted variable names separated by commas
#' @param n_profiles The specified number of profiles to be found for the clustering solution
#' @param to_center Boolean (TRUE or FALSE) for whether to center the raw data with M = 0
#' @param to_scale Boolean (TRUE or FALSE) for whether to scale the raw data with SD = 1
#' @param distance_metric Distance metric to use for hierarchical clustering; "squared_euclidean" is default but more options are available (see ?hclust)
#' @param linkage Linkage method to use for hierarchical clustering; "complete" is default but more options are available (see ?dist)
#' @param plot_centered_data Boolean (TRUE or FALSE) for whether to center the data before plotting (should not be used if to_center = T; only if to_center = F, in cases in which raw data is used to create profiles but centered profiles are desired for visualization purposes)
#' @param plot_raw_data Boolean (TRUE or FALSE) for whether to plot the raw data, regardless of whether the data are centered or scaled before clustering.
#' @return A list containing the prepared data, the output from the hierarchical and k-means cluster analysis, the r-squared value, raw clustered data, processed clustered data of cluster centroids, and a ggplot object.
#' @examples
#' create_profiles(mtcars, disp, hp, wt, n_profiles = 2, to_scale = TRUE)
#' @export 

create_profiles <- function(df, 
                            ...,
                            n_profiles, 
                            to_center = FALSE,
                            to_scale = FALSE,
                            distance_metric = "squared_euclidean",
                            linkage = "complete",
                            plot_centered_data = FALSE,
                            plot_raw_data = FALSE) {
    args <- match.call()
    prepped_data <- p(df, ..., to_center = to_center, to_scale = to_scale)
    
    y <- cluster_observations(prepped_data, n_profiles, distance_metric, linkage)
    
    if (to_center == TRUE & plot_centered_data == TRUE) {
        message("Data is already being centered before clustering, so plot_centered_data == TRUE is ignored.")
        plot_centered_data == FALSE
    }
    
    if (class(y[[4]]) == "kmeans") {
        z <- calculate_statistics(y, n_profiles, to_center = to_center, to_scale = to_scale, plot_centered_data = plot_centered_data, plot_raw_data = plot_raw_data)
        z[[11]] <- args
        invisible(z)
    } else {
        y[[5]] <- NA
        names(y)[[5]] <- "r_squared"
        y[[6]] <- args
        invisible(y)
    }
}

#' Plot R^2 (r-squared) values for a range of number of profiles
#' @details Returns ggplot2 plot of cluster centroids
#' @param df with two or more columns with continuous variables
#' @param ... unquoted variable names separated by commas
#' @param to_center (TRUE or FALSE) for whether to center the raw data with M = 0
#' @param to_scale Boolean (TRUE or FALSE) for whether to scale the raw data with SD = 1
#' @param distance_metric Distance metric to use for hierarchical clustering; "squared_euclidean" is default but more options are available (see ?hclust)
#' @param linkage Linkage method to use for hierarchical clustering; "complete" is default but more options are available (see ?dist)
#' @param lower_bound the smallest number of profiles in the range of number of profiles to explore; defaults to 2
#' @param upper_bound the largest number of profiles in the range of number of profiles to explore; defaults to 9
#' @param r_squared_table if TRUE, then a table, rather than a plot, is returned; defaults to FALSE
#' @return A list containing a ggplot2 object and a tibble for the R^2 values
#' @examples
#' df <- mtcars
#' plot_r_squared(df, mpg, wt, hp, qsec, to_center = TRUE, lower_bound = 2, upper_bound = 4)
#' @export

plot_r_squared <- function(df,    
                           ...,
                           to_center = FALSE,
                           to_scale = FALSE,
                           distance_metric = "squared_euclidean",
                           linkage = "complete",
                           lower_bound = 2, 
                           upper_bound = 9,
                           r_squared_table = FALSE) {
    
    out <- data.frame(
        cluster = lower_bound:upper_bound,
        r_squared_value = rep(NA, (upper_bound - lower_bound) + 1)
    )
    
    message("################################")
    
    for (i in lower_bound:upper_bound) {
        
        message("Clustering data for iteration ", i)
        
        out[(i - 1), "r_squared_value"] <- 
            suppressWarnings(
                suppressMessages(
                    create_profiles(df,
                                    ...,
                                    n_profiles = i, 
                                    to_center = to_center, 
                                    to_scale = to_scale, 
                                    distance_metric = distance_metric, 
                                    linkage = linkage)))[[5]]
    }
    
    message("################################")
    
    out$r_squared_value <- round(out$r_squared_value, 3)
    
    out$cluster <- as.integer(out$cluster)
    
    p <- ggplot2::ggplot(out, ggplot2::aes_string(x = "cluster", y = "r_squared_value")) +
        ggplot2::geom_point() +
        ggplot2::geom_line()
    
    if (r_squared_table == T) {
        suppressWarnings(return(out))
    } else {
        suppressWarnings(print(p))
        suppressWarnings(return(p))
    }
}

core_cross_validate <- function(df,
                                ...,
                                to_center,
                                to_scale,
                                n_profiles,
                                distance_metric,
                                linkage,
                                k) {
    
    out <- dplyr::data_frame(k_iteration = rep(NA, k),
                             kappa = rep(NA, k),
                             percentage_agree = rep(NA, k))
    
    message("################################")
    
    for (i in seq(k)){
        
        message("Clustering data for iteration ", i)
        
        df <- dplyr::select(df, ...)
        
        df$ID <- 1:nrow(df)
        
        if (nrow(df) %% 2 == 0) {
            dat1 <- dplyr::sample_n(df, nrow(df) / 2)
            dat2 <- dplyr::filter(df, !(df$ID  %in% dat1$ID))
        } else {
            dat1 <- dplyr::sample_n(df, ceiling(nrow(df) / 2)) 
            dat2 <- dplyr::filter(df, !(df$ID  %in% dat1$ID))
            dat1 <- dat1[-nrow(dat1), ]
        }
        
        dat1 <- dplyr::select(dat1, -ID)
        row.names(dat1) <- NULL
        dat2 <- dplyr::select(dat2, -ID)
        
        # step 1 (cluster half the data)
        
        two_prof_dat1 <- 
            suppressWarnings(
                suppressMessages(
                    create_profiles(dat1, 
                                    ...,
                                    n_profiles = n_profiles, 
                                    to_center = to_center, 
                                    to_scale = to_center, 
                                    distance_metric = distance_metric,
                                    linkage = linkage)))
        
        # step 2 (cluster the other half of the data)
        two_prof_dat2 <- 
            suppressWarnings(
                suppressMessages(
                    create_profiles(dat2, 
                                    ...,
                                    n_profiles = n_profiles, 
                                    to_center = to_center, 
                                    to_scale = to_center, 
                                    distance_metric = distance_metric,
                                    linkage = linkage)))
        
        if (is.na(two_prof_dat1[[4]][1]) | is.na(two_prof_dat2[[4]][1]) ) {
            message("")
            message("Could not calculate agreement because k-means algorithm did not converge")
            out$k_iteration[i] <- i
            out$kappa[i] <- NA
            out$percentage_agree[i] <- NA
            next()
        }
        
        # these are sample 1 data and cluster assignments
        two_prof_cross_1 <- two_prof_dat1$clustered_raw_data 
        
        # these are sample 2 data and cluster assignments
        two_prof_cross_2 <- two_prof_dat2$clustered_raw_data 
        
        # step 3 (Assign observations in one half (say, sample 2) . . . 
        # . . . to the profile to which they are most similar in the other half (say, sample 1))
        #reclassify by nearest neighbor
        
        two_prof_cross_2$cluster_nn <- class::knn1(dat1, dat2, two_prof_cross_1$cluster) 
        
        # step 4 (Re-code the profiles in sample 1 on the basis of which profile they are the most similar to in sample 2 . . . 
        # . . . optimizing for the re-coding  that maximizes the agreement across all profiles)
        
        #two_prof_cross$cluster.f <- two_prof_cross$cluster
        
        two_prof_df <- dplyr::data_frame(orig_cluster = two_prof_cross_2$cluster, cluster_nn = two_prof_cross_2$cluster_nn)
        two_prof_df$cluster_nn <- as.integer(two_prof_df$cluster_nn)
        two_prof_tab <- table(two_prof_df)
        
        #solve assignment
        res <- lpSolve::lp.assign(-two_prof_tab)
        
        l <- apply(res$solution > 0.5, 1, which)
        
        # step 5 (Calculate the agreement between re-coded sample 1 observations assigned to the sample 2 profile to which they are most similar . . . 
        # . . . and the original sample 1 profiles
        two_prof_cross_2$cluster_nn_rc <- l[two_prof_cross_2$cluster]
        
        recode_df <- dplyr::select(two_prof_cross_2, cluster_nn, cluster_nn_rc)
        
        try_kappa <- function(df) {
            out <- tryCatch(
                {
                    x <- irr::kappam.fleiss(df)
                    x$value
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
        
        Kap <- try_kappa(recode_df)
        agreement <- irr::agree(recode_df)
        
        out$k_iteration[i] <- i
        out$kappa[i] <- round(as.numeric(Kap), 2)
        out$percentage_agree[i] <- round(agreement$value / 100, 2)
        
    }
    
    message("################################")
    
    message(paste0("Mean Fleiss's Kappa for ", k, " iterations is ", round(mean(out$kappa, na.rm = T), 2)))
    message(paste0("Mean percentage agreement for ", k, " iterations is ", round(mean(out$percentage_agree, na.rm = T), 2)))
    
    out
    
}

#' Returns statistics from double-split cross validation
#' @details Performs double-split cross validation and returns Cohen's Kappa and percentage agreement statistics.
#' @param x A `prcr` object
#' @param ... Additional arguments
#' @return A ggplot2 object
#' @export
#' 

cross_validate <- function(df,
                           ...,
                           to_center = FALSE,
                           to_scale = FALSE,
                           n_profiles,
                           distance_metric = "squared_euclidean",
                           linkage = "complete", 
                           k = 30,
                           lower_bound = 2,
                           upper_bound = 9) {
    
    if (n_profiles == "iterate") {
        
        out_list <- list()
        
        for (i in lower_bound:upper_bound){
            
            out <- suppressWarnings(
                suppressMessages(core_cross_validate(df,
                                                     ...,
                                                     to_center = to_center,
                                                     to_scale = to_scale,
                                                     n_profiles = i,
                                                     distance_metric = distance_metric,
                                                     linkage = linkage, 
                                                     k = k)))
            
            out_list[[i - 1]] <- out
            
        }
        
        profile <- paste0("Profile ", lower_bound:upper_bound)

        mean_kappa <- purrr::map_dbl(out_list, function(x) mean(x$kappa, na.rm = T))
        print(str(mean_kappa))
        mean_percentage_agree <- purrr::map_dbl(out_list, function(x) mean(x$percentage_agree, na.rm = T))
        
        out <- data.frame(profile, mean_kappa, mean_percentage_agree)
        
        return(out)
        
    } else {
        
        out <- core_cross_validate(df,
                                   ...,
                                   to_center = to_center,
                                   to_scale = to_scale,
                                   n_profiles = n_profiles,
                                   distance_metric = distance_metric,
                                   linkage = linkage, 
                                   k = k)
        
    }
    
    return(out)
    
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
}