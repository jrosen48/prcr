# # Model-based clustering
# 
# library(mclust)
# 
create_profiles_lpa <- function(prepared_data, G = 9, cluster_names = NULL, print_status = T){

    if(is.null(cluster_names)){
        cluster_names <- paste0("Cluster ", G)
    }
    variable_names <- attributes(prepared_data)$variable_names
    # Create profiles

    print(paste0("### Note. This function can take a long time."))
    out <- list()
    prepared_data_ss <- data.frame(matrix(unlist(prepared_data), ncol = length(prepared_data), byrow = F))

    tmp_BIC = mclust::mclustBIC(prepared_data_ss, G = G)

    out[[1]] <- tmp_BIC # fit indices
    
    model1 <- mclust::Mclust(data = prepared_data_ss, x = tmp_BIC, G = G)

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
    
    attributes(out) <- list(df = prepared_data_ss)
    return(out)

}


explore_factors_lpa_test <- function(factor_data_frame, 
                                     factor_to_explore, 
                                     variable_to_find_proportion = NULL, 
                                     cluster_names = NULL,
                                     print_status = T,
                                     cluster_assignments,
                                     cases_to_keep,
                                     variable_names,
                                     cleaned_data){
    out <- list()
    factor_data_frame[] <- lapply(factor_data_frame, factor)
    data <- merge_assignments_and_factors(cluster_assignments, cases_to_keep, factor_data_frame)
    data_for_descriptive_stats <- data.frame(cleaned_data, data)
    dummy_coded_data <- dummmy_code_cluster_assignments(data)
    out[[1]] <- create_crosstab(data, factor_to_explore)
    out[[2]] <- create_raw_data(dummy_coded_data, factor_to_explore, variable_to_find_proportion)
    out[[3]] <- create_processed_data(out[[2]], factor_to_explore, variable_to_find_proportion)
    out[[4]] <- create_plot_to_explore_factors(out[[3]], factor_to_explore, cluster_names)
    out[[5]] <- find_n(out[[2]], factor_to_explore)
    out[[6]] <- create_compare_anova(out[[2]], variable_to_find_proportion, cluster_names, factor_to_explore)
    # out[[7]] <- create_compare_manova()
    
    if(print_status == T){
        print("### Created the following output ... ")
        print("### 1. Comparison table ###")
        print("### 2. Processed data: Raw ###")
        print("### 3. Processed data: Summary ###")
        print("### 4. ggplot2 object  ###")
        print("### 5. Number by factor  ###")
        print("### 6. ANOVA [[1]] and Tukey HSD [[2]] ###")
        # print("### 7. MANOVA ###")
    }
    attributes(out) <- list(cleaned_df = data_for_descriptive_stats, data_attr = data)
    invisible(out)
    
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
