
#' Returns statistics from double-split cross validation
#' @details Performs double-split cross validation and returns Cohen's Kappa and percentage agreement statistics.
#' @param df with two or more columns with continuous variables
#' @param ... unquoted variable names separated by commas
#' @param to_center (TRUE or FALSE) for whether to center the raw data with M = 0
#' @param to_scale Boolean (TRUE or FALSE) for whether to scale the raw data with SD = 1
#' @param n_profiles the number of profiles in the solution to cross-validate; or, the character string "iterate" to perform cross-validation for 2 to 9 profile solutions
#' @param distance_metric Distance metric to use for hierarchical clustering; "squared_euclidean" is default but more options are available (see ?hclust)
#' @param linkage Linkage method to use for hierarchical clustering; "complete" is default but more options are available (see ?dist)
#' @param k the number of iterations
#' @param lower_bound if n_profiles = "iterate", then this is the smallest number of profiles in the range of number of profiles to explore; defaults to 2
#' @param upper_bound if n_profiles = "iterate", then this is the largest number of profiles in the range of number of profiles to explore; defaults to 9
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
    
    message("################################")
    
    if (n_profiles == "iterate") {
        
        out_list <- list()
        
        for (i in lower_bound:upper_bound){
            
            message(paste0("Clustering for n_profiles = ", i))
            
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
        
        mean_percentage_agree <- purrr::map_dbl(out_list, function(x) mean(x$percentage_agree, na.rm = T))
        
        out <- data.frame(profile, mean_kappa, mean_percentage_agree)
        
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
    
    message("################################")
    
}
