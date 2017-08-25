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

p <- function(df, ..., to_center, to_scale, remove_mv_outliers){
    if (!is.data.frame(df)) stop("df must be a data.frame (or tibble)")
    df <- tibble::as_tibble(df)
    df_ss <- dplyr::select(df, ...)
    cases_to_keep <- stats::complete.cases(df_ss) # to use later for comparing function to index which cases to keep
    df_ss_wo_incomplete_cases <- df_ss[cases_to_keep, ] # removes incomplete cases
    
    if (remove_mv_outliers == TRUE) {
        outliers <- detect_outliers(df_ss_wo_incomplete_cases)
        prepared_data[[11]] <- outliers
        names(prepared_data[[11]]) <- "outliers"
        message("Removed ", length(outliers), " multivariate outliers; view the outliers slot of the output to view the cases removed")
        df_ss_wo_incomplete_cases <- df_ss_wo_incomplete_cases[-outliers, ]
    }
    
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

outlierHadi <- function(X) {
    # -----------------------------------------------------------------
    #  Hadi, Ali S. (1994), "A Modification of a Method for the
    #  Detection of Outliers in Multivariate Samples," Journal of the
    #  Royal Statistical Society (B), 2, 393-396.
    # -----------------------------------------------------------------
    n <- dim(X) [1]
    p <- dim(X) [2]
    h <- trunc((n + p + 1)/2)     
    id <- 1:n
    r <- p
    out <- 0
    cf <- (1 + ((p + 1)/(n - p)) + (2/(n - 1 - (3*p))) )^2
    # cf <- (1 + ((p + 1)/(n - p)) + (1/(n - p - h)) )^2
    alpha <- 0.05
    tol <- max(10^-(p+5), 10^-12)
    # -----------------------------------------------------------------
    # **  Compute Mahalanobis distance
    # -----------------------------------------------------------------
    C <- apply(X, 2, mean)
    S <- var(X)
    if (det(S) < tol) stop ()
    D <- mahalanobis(X, C, S)
    mah.out <- 0
    cv <- qchisq(1-(alpha/n), p)
    for (i in 1:n) if (D[i] >= cv) mah.out <- cbind(mah.out, i)
    mah.out <- mah.out[-1]
    mah <- sqrt(D)
    Xbar <- C
    Covariance <- S   #
    # ----------------------------------------------------------------
    # **  Step 0
    # ----------------------------------------------------------------
    #  **  Compute Di(Cm, Sm)
    C <- apply(X, 2, median)
    #original code was 
    #  C <- t(array(C, dim = c(n, p)))
    #but resulted in nonconformable arrays. 
    #so i removed the transpose
    C <- array(C, dim = c(n, p))
    Y <- X - C
    S <- ((n - 1)^-1)*(t(Y) %*% Y)
    D <- mahalanobis(X, C[1, ], S)
    Z <- sort.list(D)
    # ----------------------------------------------------------------
    #  **  Compute Di(Cv, Sv)
    repeat {
        Y <- X[Z[1:h], ]
        C <- apply(Y, 2, mean)
        S <- var(Y)
        if (det(S) > tol) {
            D <- mahalanobis(X, C, S)
            Z <- sort.list(D); break }
        else h <- h + 1
    }
    # ----------------------------------------------------------------
    #  **  Step 1
    # ----------------------------------------------------------------
    repeat {
        r <- r + 1
        if ( h < r) break
        Y <- X[Z[1:r],]
        C <- apply(Y, 2, mean)
        S <- var(Y)
        if (det(S) > tol) {
            D <- mahalanobis(X, C, S)
            Z <- sort.list(D) }
    }
    #**  Step 3
    # ----------------------------------------------------------------
    #  **  Compute Di(Cb, Sb)
    repeat {
        Y <- X[Z[1:h],]
        C <- apply(Y, 2, mean)
        S <- var(Y)
        if (det(S) > tol) {
            D <- mahalanobis(X, C, S)
            Z <- sort.list(D)
            if (D[Z[h + 1]] >= (cf*qchisq(1-(alpha/n), p))) {
                out <- Z[(h + 1) : n]
                break }
            else { h <- h + 1
            if (n <= h) break }
        }
        else { h <- h + 1
        if (n <= h) break }
    }
    D <- sqrt(D/cf)
    dst <- cbind(id, mah, D)
    Outliers <- out
    Cb <- C;
    Sb <- S
    Distances <- dst
    result <- list(Xbar = Xbar, Covariance = Covariance, mah.out = mah.out, 
                   Outliers = Outliers, Cb = Cb, Sb = Sb, Distances = Distances )
    class( result ) <- "outlierHadi"
    return( result )
}

detect_outliers <- function(df) {
    x <- outlierHadi(as.matrix(df))
    mv_outliers <- sort(x$Outliers)
}