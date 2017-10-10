
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
        
        dat1$ID <- NULL
        row.names(dat1) <- NULL
        dat2$ID <- NULL
        
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
        
        recode_df <- dplyr::select(two_prof_cross_2, two_prof_cross_2$cluster_nn, two_prof_cross_2$cluster_nn_rc)
        
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