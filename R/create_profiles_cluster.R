# functions.R

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
#' @export 

create_profiles_cluster <- function(df, 
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

#' Identifies potential outliers
#' @details * add an argument to `create_profiles()` to remove multivariate outliers based on Hadi's (1994) procedure
#' @param df data.frame (or tibble) with variables to be clustered; all variables must be complete cases
#' @param return_index Boolean (TRUE or FALSE) for whether to return only the row indices of the possible multivariate outliers; if FALSE, then all of the output from the function (including the indices) is returned
#' @return either the row indices of possible multivariate outliers or all of the output from the function, depending on the value of return_index
#' @export
#' 

detect_outliers <- function(df, return_index = TRUE) {
    x <- outlierHadi(as.matrix(df))
    if (return_index == TRUE) {
        print(sort(x$Outliers))
        mv_outliers <- sort(x$Outliers)
    } else {
        print(x)
        x
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