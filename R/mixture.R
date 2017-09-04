# mixture.R 

create_profiles_mclust <- function(df,
                                   n_profiles,
                                   model_name = NULL,
                                   to_return = "tibble"){
    
    if (model_name %in% c("constrained_variance", "constrained_variance_and_covariance", "freed_variance_and_covariance")) {
        
        if (model_name == "constrained_variance") {
            model_name <- "EEI"
        } else if (model_name == "constrained_variance_and_covariance") {
            model_name <- "EEE"
        } else if (model_name == "freed_variance_and_covariance") {
            model_name <- "VVV"
        } else if (model_name %in% c("E", "V", "EII", "VII", "EEI", "VEI", "EVI", "VVI", "EEE", "EVE", "VEE", "VVE", "EEV", "VEV", "EVV", "VVV", "X", "XII", "XXI", "XXX")) {
            model_name <- model_name
        } else {
            stop("Model name is not correctly specified: use 'constrained_variance', 'constrained_variance_and_covariance', 'freed_variance_and_covariance' or one of the model names specified from ?mclust::mclustModelNames()")
        }
        
    }
    
    x <- mclust::Mclust(df, G = n_profiles, modelNames = model_name)
    
    dff <- as.data.frame(dplyr::bind_cols(df, profile = x$classification)) # replace with tibble
    
    attributes(dff)$mclust_output <- x
    
    if (to_return == "tibble") {
        return(dff)
    } else if (to_return == "mclust") {
        return(attributes(dff)$mclust_output)
    }
    
}