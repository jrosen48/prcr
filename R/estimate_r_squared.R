
#' Estimates R^2 (r-squared) values for a range of number of profiles
#' @details Returns ggplot2 plot of cluster centroids
#' @param df with two or more columns with continuous variables
#' @param ... unquoted variable names separated by commas
#' @param to_center (TRUE or FALSE) for whether to center the raw data with M = 0
#' @param to_scale Boolean (TRUE or FALSE) for whether to scale the raw data with SD = 1
#' @param distance_metric Distance metric to use for hierarchical clustering; "squared_euclidean" is default but more options are available (see ?hclust)
#' @param linkage Linkage method to use for hierarchical clustering; "complete" is default but more options are available (see ?dist)
#' @param lower_bound the smallest number of profiles in the range of number of profiles to explore; defaults to 2
#' @param upper_bound the largest number of profiles in the range of number of profiles to explore; defaults to 9
#' @param r_squared_table if TRUE (default), then a table, rather than a plot, is returned; defaults to FALSE
#' @return A list containing a ggplot2 object and a tibble for the R^2 values
#' @export

estimate_r_squared <- function(df,    
                               ...,
                               to_center = FALSE,
                               to_scale = FALSE,
                               distance_metric = "squared_euclidean",
                               linkage = "complete",
                               lower_bound = 2, 
                               upper_bound = 9,
                               r_squared_table = TRUE) {
    
    out <- data.frame(
        cluster = lower_bound:upper_bound,
        r_squared_value = rep(NA, (upper_bound - lower_bound) + 1)
    )
    
    for (i in lower_bound:upper_bound) {
        
        message("Clustering data for iteration ", i)
        
        out[(i - 1), "r_squared_value"] <- 
            suppressMessages(create_profiles_cluster(df,
                                                     ...,
                                                     n_profiles = i, 
                                                     to_center = to_center, 
                                                     to_scale = to_scale, 
                                                     distance_metric = distance_metric, 
                                                     linkage = linkage))[[5]]
    }
    
    out$r_squared_value <- round(out$r_squared_value, 3)
    
    out$cluster <- as.integer(out$cluster)
    
    p <- ggplot2::ggplot(out, ggplot2::aes_string(x = "cluster", y = "r_squared_value")) +
        ggplot2::geom_point() +
        ggplot2::geom_line()
    
    if (r_squared_table == TRUE) {
        suppressWarnings(return(out))
    } else {
        suppressWarnings(print(p))
        suppressWarnings(return(p))
    }
}