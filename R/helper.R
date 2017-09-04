# helper functions

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

possibly_kmeans <- purrr::possibly(kmeans, NA)

is.kmeans <- function(x) inherits(x, "kmeans")

is.prcr <- function(x) inherits(x, "prcr")

extract_hclust_output <- function(x){
    attributes(x)$hclust_output
}

extract_kmeans_output <- function(x){
    attributes(x)$kmeans_output
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

uv_outlier_detector <- function(x, na.rm = T, ...) {
    # need to figure out where this came from - from a SO question, can probably re-write
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    y
}

remove_uv_out_func <- function(data){
    x <- sapply(data, uv_outlier_detector)
    return(x)
}

remove_uv_main_func <- function(data, removed_obs_df, cases_to_keep){
    data_tmp <- remove_uv_out_func(data) # makes uv outliers na
    print(paste0("### Note: ", sum(is.na(data_tmp)), " cases with univariate outliers out of ", nrow(data_tmp), " cases removed, so ", nrow(data_tmp) - sum(is.na(data_tmp)), " used in subsequent analysis ###"))
    if(any(is.na(data_tmp))){
        x <- removed_obs_df[cases_to_keep, ]
        y <- !complete.cases(data_tmp)
        z <- x$row[y]
        removed_obs_df$reason_removed[z] <- "uniivariate_outlier"
    }
    data_out <- data_tmp[complete.cases(data_tmp), ]
    if(any(is.na(data_tmp))){
        data_out <- list(data_out, removed_obs_df, uv_outliers_boolean_vector = y)
    } else{
        data_out <- list(data_out, removed_obs_df, uv_outliers_boolean_vector = NULL)
    }
    return(data_out)
}

detect_outliers <- function(df) {
    outlierHadi(as.matrix(df))
    # need to add UV outlier detector
}

prepare_data <- function(df, ..., to_center, to_scale){
    if (!is.data.frame(df)) stop("df must be a data.frame (or tibble)")
    
    df <- tibble::as_tibble(df)
    
    if (rlang::dots_n(...) > 1) {
        df_ss <- dplyr::select(df, ...)
    } else {
        df_ss <- dplyr::select(df, everything())
    }
    
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
