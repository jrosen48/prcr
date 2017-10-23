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
#' @return A list containing the prepared data, the output from the hierarchical and k-means cluster analysis, the r-squared value, raw clustered data, processed clustered data of cluster centroids, and a ggplot object.
#' @examples
#' d <- pisaUSA15
#' m3 <- create_profiles_cluster(d, 
#'                               broad_interest, enjoyment, instrumental_mot, self_efficacy,
#'                               n_profiles = 3)
#' summary(m3)
#' @export 

create_profiles_cluster <- function(df, 
                                    ...,
                                    n_profiles, 
                                    to_center = FALSE,
                                    to_scale = FALSE,
                                    distance_metric = "squared_euclidean",
                                    linkage = "complete") {
    args <- match.call()
    prepped_data <- p(df, ..., to_center = to_center, to_scale = to_scale)
    
    y <- cluster_observations(prepped_data, n_profiles, distance_metric, linkage)
    
    if (class(y[[4]]) == "kmeans") {
        z <- calculate_statistics(y, n_profiles, to_center = to_center, to_scale = to_scale)
        z[[11]] <- args
        invisible(z)
    } else {
        y[[5]] <- NA
        names(y)[[5]] <- "r_squared"
        y[[6]] <- args
        invisible(y)
    }
}