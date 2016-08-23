# # Model-based clustering
# 
# library(mclust)
# 
create_profiles_lpa <- function(prepared_data, G = 9, cluster_names = NULL, print_status = T){
    
    if(is.null(cluster_names)){
        cluster_names <- paste0("Cluster ", 1:G)
    }
    variable_names <- attributes(prepared_data)$variable_names
    # Create profiles

    print(paste0("### Note. This function can take a long time."))
    out <- list()
    prepared_data_ss <- data.frame(matrix(unlist(prepared_data), ncol = length(prepared_data), byrow = F))

    tmp_BIC = mclust::mclustBIC(prepared_data_ss)
    out[[1]] <- tmp_BIC # fit indices
    model1 <- mclust::Mclust(data = prepared_data_ss, x = tmp_BIC, G)

    out[[2]] <- model1 # model output

    # Calculate stats

    out[[3]] <- out[[2]]$classification # assignment

    tmp_df <- as.data.frame(t(out[[2]]$parameters$mean))

    # if(is.null(variable_names)){
    #     variable_names <- paste0("Var", 1:G)
    #     row.names(tmp_df) <- variable_names
    # }

    tmp_df$Cluster <- paste0("Cluster ", 1:G, ": ", table(out[[2]]$classification), " Obs.")

    out[[4]] <- tmp_df # cluster centroid table
    #out[[5]] <- cluster_plot_function(out[[4]]) # cluster plot
    
    if(print_status == T){
        print("### Created the following output ... ")
        print("### 1. Fit indices ###")
        print("### 2. Model output ###")
        print("### 3. Assignments ###")
        print("### 4. Cluster centroid table ###")
    }
    
    return(out)

}

# 
# x <- create_profiles_lpa(prepared_data, G = 7)
# 
# plot(x[[1]])
# summary(x[[1]])
# 
# plot(x[[4]])
# 
# str(x[[4]])
# plot(x[[5]])
# 
# X = data.frame(matrix(unlist(prepared_data), ncol = length(prepared_data), byrow = F))
# 
# ICL = mclustICL(X)
# summary(ICL)
# 
# plot(ICL)
# 
# LRT = mclustBootstrapLRT(X, modelName = "VVV")
# LRT
