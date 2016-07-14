# helper_functions.R

# For cluster_data function

centering_function <- function(data, method_of_centering, grouping_vector, to_standardize = F){
    center_this <- function(x){
        x - mean(x, na.rm = T)
    }
    scale_this <- function(x) {
        if (sd(x, na.rm = T) == 0){
            x - mean(x, na.rm = T)
        } else {
            (x - mean(x, na.rm = T)) / sd(x, na.rm = T)
        }
    }
    if (method_of_centering == "grand" & to_standardize == F) {
        out <- sapply(data, function(x) center_this(x))
        out <- as.data.frame(out)
    }
    if (method_of_centering == "group" & to_standardize == F) {
        out <- data %>%
            cbind(grouping_vector) %>%
            group_by(grouping_vector) %>%
            mutate_each(funs(center_this))
        out <- as.data.frame(out[, 1:ncol(data)])
    }
    if (method_of_centering == "grand" & to_standardize == T) {
        out <- sapply(data, function(x) scale_this(x))
        out <- as.data.frame(out)
    }

    if (method_of_centering == "group" & to_standardize == T) {
        out <- data %>%
            cbind(grouping_vector) %>%
            group_by(grouping_vector) %>%
            mutate_each(funs(scale_this))
        out <- as.data.frame(out[, 1:ncol(data)])
    }
    if (method_of_centering == "raw") {
        out <- as.data.frame(data)
    }
    return(out)
}

distance_function <- function(x, distance_metric = "squared_euclidean"){
    if (distance_metric != "squared_euclidean") {
        distance <- dist(x, method = distance_metric)
    } else {
        distance <- dist(x, method = "euclidean")
        distance <- distance ^ 2
    }
    return(distance)
}

hclust_to_kmeans_function <- function(data, out, n_clusters){
    # This function processes the output from the hierarchical clustering to be used as starting points for the kmeans clustering
    cut_hclust <- cutree(out, n_clusters) # cuts the results of the hierarchical cluster at the specified # of clusters
    clusters_list <- list()
    for (i in seq(n_clusters)){
        clusters_list[[i]] <- data[cut_hclust == i,]
    }
    ordered_clusters <- list()
    cluster_freqs <- list()
    for (i in seq(length(clusters_list))){
        ordered_clusters[[i]] <- colSums(as.matrix(clusters_list[[i]]) / nrow(clusters_list[[i]]))
        cluster_freqs[[i]] <- ordered_clusters[[i]]
    }
    return(cluster_freqs)
}

kmeans_function <- function(data, cluster_freqs){
    start <- data.frame(matrix(unlist(cluster_freqs), nrow=length(cluster_freqs[[1]]), byrow = T), stringsAsFactors = F)
    start <- as.matrix(start)
    start <- t(start)
    return(kmeans(data, start))
}

# For calculate_stats function

dissim_function <- function(hc){
    data.frame(row.names = paste0("Cluster", seq_along(hc$height)),
               height = hc$height,
               component_1 = paste0("Cluster", hc$merge[, 1]),
               component_2 = paste0("Cluster", hc$merge[, 2]),
               stringsAsFactors=FALSE)
}

cluster_freq_function <- function(data, n_clusters, kfit, variable_names){
    clusters <- list()
    for (i in 1:n_clusters){
        clusters[[i]] <- data[kfit$cluster == i, ]
    }
    clusters
    cluster_freqs <- list()
    for (i in seq(n_clusters)){
        cluster_freqs[[i]] <- colSums(clusters[[i]]) / nrow(clusters[[i]]) # Need to fix - will want to add group freqs
    }
    cluster_freqs <- as.data.frame(matrix(unlist(cluster_freqs), nrow = n_clusters, byrow = T))
    names(cluster_freqs) <- variable_names
    cluster_freqs$Cluster <- paste0("Cluster ", 1:n_clusters, ": ", table(kfit$cluster), " Obs.")
    return(cluster_freqs)
}

cluster_plot_function <- function(cluster_freqs, cluster_names){
    cluster_freqs_tmp <- tidyr::gather(cluster_freqs, Var, Value, -Cluster)
    if (is.null(cluster_names)){
        clusters_p <- ggplot2::ggplot(cluster_freqs_tmp, aes(x = Cluster, y = Value, fill = Var)) +
            geom_bar(stat = "identity", position = "dodge", color = "black") +
            scale_fill_brewer(type = "qual", palette = 1) +
            ylab("") +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            theme(legend.title=element_blank()) +
            theme(legend.position = "top") +
            theme(text=element_text(size = 12, family = "Times"))
    } else {
        clusters_p <- ggplot2::ggplot(cluster_freqs_tmp, aes(x = Cluster, y = Value, fill = Var)) +
            geom_bar(stat = "identity", position = "dodge", color = "black") +
            scale_fill_brewer(type = "qual", palette = 1) +
            ylab("") +
            xlab("") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            theme(legend.title=element_blank()) +
            theme(legend.position = "top") +
            theme(text=element_text(size = 12, family = "Times")) +
            scale_x_discrete(breaks = waiver(), labels = cluster_names)
    }
    return(clusters_p)
}

testing_the_tukey <- function(data){
    tukey_list <- list()
    for (i in 1:(ncol(data) - 2)){
        fit <- aov(data[, i] ~ as.character(data$cluster_assignment))
        tukey_list[[i]] <- TukeyHSD(fit)
    }
    names(tukey_list) <- names(data[, 1:3])
    return(tukey_list)
}

manova_function <- function(data, cluster_assignment, variable_names){
    out <- list()
    data$DV <- as.matrix(data)
    data <- cbind(data, cluster_assignment)
    mv_out <- manova(DV ~ cluster_assignment, data = data)
    out[[1]] <- summary(mv_out, test = "Pillai")
    out[[2]] <- summary.aov(mv_out)
    out[[3]] <- testing_the_tukey(data)
    names(out[[3]]) <- variable_names
    return(out)
}

# For explore_factors function

merge_assignments_and_factors <- function(cluster_assignments, cases_to_keep, factor_data_frame){
    factor_data_frame_ss <- factor_data_frame[cases_to_keep, ]
    data <- data.frame(cluster = cluster_assignments, factor_data_frame_ss)
    data$cluster <- as.factor(data$cluster)
    return(data)
}

create_crosstab <- function(data, factor_to_explore){
    if (length(factor_to_explore) == 1) {
        which_variable <- which(names(data) == factor_to_explore)
        out <- table(data$cluster, data[, which_variable])
    } else if (length(factor_to_explore) == 2) {
        which_variable_1 <- which(names(data) == factor_to_explore[1])
        which_variable_2 <- which(names(data) == factor_to_explore[2])
        out <- table(data$cluster, data[, which_variable_1], data[, which_variable_2])
    } else if (length(factor_to_explore) == 3) {
        which_variable_1 <- which(names(data) == factor_to_explore[1])
        which_variable_2 <- which(names(data) == factor_to_explore[2])
        which_variable_3 <- which(names(data) == factor_to_explore[3])
        out <- table(data$cluster, data[, which_variable_1], data[, which_variable_2], data[, which_variable_3])
    }
    return(out)
}

dummmy_code_cluster_assignments <- function(data){
    tmp <- as.data.frame(model.matrix( ~ cluster - 1, data = data))
    out <- data.frame(data[, 2:ncol(data)], tmp)
    return(out)
}

create_raw_data <- function(dummy_coded_data, factor_to_explore, variable_to_find_proportion){
    # this is for ANOVA and for subsequent processing
    
    for_one <- function(dummy_coded_data, factor_to_explore, variable_to_find_proportion){
        if (!is.null(variable_to_find_proportion)) {
            raw_data <- dummy_coded_data %>%
                select(matches(variable_to_find_proportion), matches(factor_to_explore), contains("cluster")) %>%
                group_by_(variable_to_find_proportion, factor_to_explore) %>%
                summarize_each(funs(mean)) %>%
                ungroup()
        } else {
            raw_data <- dummy_coded_data %>%
                select(matches(factor_to_explore), contains("cluster")) %>%
                ungroup()
        }
        return(raw_data)
    }
    
    for_two <- function(dummy_coded_data, factor_to_explore, variable_to_find_proportion){
        if (!is.null(variable_to_find_proportion)) {
            raw_data <- dummy_coded_data %>%
                select(matches(variable_to_find_proportion), matches(factor_to_explore[1]), matches(factor_to_explore[2]), contains("cluster")) %>%
                group_by_(variable_to_find_proportion, factor_to_explore) %>%
                summarize_each(funs(mean)) %>%
                ungroup()
        } else {
            raw_data <- dummy_coded_data %>%
                select(matches(factor_to_explore[1]), matches(factor_to_explore[2]), contains("cluster")) %>%
                ungroup()
        }
        return(raw_data)
    }
    
    for_three <- function(dummy_coded_data, factor_to_explore, variable_to_find_proportion){
        if (!is.null(variable_to_find_proportion)) {
            raw_data <- dummy_coded_data %>%
                select(matches(variable_to_find_proportion), matches(factor_to_explore[1]), matches(factor_to_explore[2]), matches(factor_to_explore[3]), contains("cluster")) %>%
                group_by_(variable_to_find_proportion, factor_to_explore) %>%
                summarize_each(funs(mean)) %>%
                ungroup()
        } else {
            raw_data <- dummy_coded_data %>%
                select(matches(factor_to_explore), matches(factor_to_explore[1]), matches(factor_to_explore[2]), matches(factor_to_explore[3]), contains("cluster")) %>%
                ungroup()
        }
        return(raw_data)
    }
    
    if (length(factor_to_explore) == 1) {
        out <- for_one(dummy_coded_data, factor_to_explore, variable_to_find_proportion)
    } else if (length(factor_to_explore) == 2) {
        out <- for_two(dummy_coded_data, factor_to_explore, variable_to_find_proportion)
    } else if (length(factor_to_explore) == 3) {
        out <- for_three(dummy_coded_data, factor_to_explore, variable_to_find_proportion)
    }

    return(out)
}

find_n <- function(raw_data, factor_to_explore){
    
    if (length(factor_to_explore) == 1) {
        out <- 
            raw_data %>%
            group_by_(factor_to_explore) %>%
            summarize(n = n())
    } else if (length(factor_to_explore) == 2) {
        out <- 
            raw_data %>%
            group_by_(factor_to_explore[1], factor_to_explore[2]) %>%
            summarize(n = n())    
    } else if (length(factor_to_explore) == 3) {
        out <- 
            raw_data %>%
            group_by_(factor_to_explore[1], factor_to_explore[2], factor_to_explore[3]) %>%
            summarize(n = n())
    }
    
}

create_processed_data <- function(raw_data, factor_to_explore, variable_to_find_proportion){
    # this is for summary table and plot
    
    for_one <- function(dummy_coded_data, factor_to_explore, variable_to_find_proportion){
        if (!is.null(variable_to_find_proportion)) {
            processed_data <- raw_data %>%
                select(-matches(variable_to_find_proportion)) %>%
                group_by_(factor_to_explore) %>%
                summarize_each(funs(mean))
        } else {
            processed_data <- raw_data %>%
                group_by_(factor_to_explore) %>%
                summarize_each(funs(mean))
        }
        return(processed_data)
    }
    
    for_two <- function(dummy_coded_data, factor_to_explore, variable_to_find_proportion){
        if (!is.null(variable_to_find_proportion)) {
            processed_data <- raw_data %>%
                select(-matches(variable_to_find_proportion)) %>%
                group_by_(factor_to_explore[1], factor_to_explore[2]) %>%
                summarize_each(funs(mean))
        } else {
            processed_data <- raw_data %>%
                group_by_(factor_to_explore[1], factor_to_explore[2]) %>%
                summarize_each(funs(mean))
        }
        return(processed_data)
    }
    
    for_three <- function(dummy_coded_data, factor_to_explore, variable_to_find_proportion){
        if (!is.null(variable_to_find_proportion)) {
            processed_data <- raw_data %>%
                select(-matches(variable_to_find_proportion)) %>%
                group_by_(factor_to_explore[1], factor_to_explore[2], factor_to_explore[3]) %>%
                summarize_each(funs(mean))
        } else {
            processed_data <- raw_data %>%
                group_by_(factor_to_explore[1], factor_to_explore[2], factor_to_explore[3]) %>%
                summarize_each(funs(mean))
        }
        return(processed_data)
    }
    
    if (length(factor_to_explore) == 1) {
        out <- for_one(dummy_coded_data, factor_to_explore, variable_to_find_proportion)
    } else if (length(factor_to_explore) == 2) {
        out <- for_two(dummy_coded_data, factor_to_explore, variable_to_find_proportion)
    } else if (length(factor_to_explore) == 3) {
        out <- for_three(dummy_coded_data, factor_to_explore, variable_to_find_proportion)
    }
    
    return(out)
    
}

create_plot_to_explore_factors <- function(processed_data, factor_to_explore, cluster_names){

    processed_data <- processed_data[complete.cases(processed_data), ]
    
    if (length(factor_to_explore) == 1) {
        to_plot <- tidyr::gather(processed_data, cluster, mean, -matches(factor_to_explore))
        out <- ggplot(to_plot, aes(y = mean, fill = cluster)) +
            aes_string(x = factor_to_explore) +
            geom_bar(stat = "identity", color = "black") +
            xlab("") +
            ylab("Proportion of Responses") +
            ggtitle("") +
            theme(legend.position = "top") +
            theme(legend.title = element_blank()) +
            theme(text=element_text(size = 12, family = "Times")) +
            theme(axis.text.x = element_text(angle = 90)) +
            theme(legend.position = "right") +
            theme(legend.title = element_blank()) +
            scale_fill_discrete(name = "Cluster", labels = cluster_names)
    } else if (length(factor_to_explore) == 2) {
        to_plot <- tidyr::gather(processed_data, cluster, mean, -matches(factor_to_explore[1]), -matches(factor_to_explore[2]))
        out <- ggplot(to_plot, aes(y = mean, fill = cluster)) +
            aes_string(x = factor_to_explore[1]) +
            facet_wrap(as.formula(paste("~", factor_to_explore[2]))) +
            geom_bar(stat = "identity", color = "black") +
            xlab("") +
            ylab("Proportion of Responses") +
            ggtitle("") +
            theme(legend.position = "top") +
            theme(legend.title = element_blank()) +
            theme(text=element_text(size = 12, family = "Times")) +
            theme(axis.text.x = element_text(angle = 90)) +
            theme(legend.position = "right") +
            theme(legend.title = element_blank()) +
            scale_fill_discrete(name = "Cluster", labels = cluster_names)
    } else if (length(factor_to_explore) == 3) {
        to_plot <- tidyr::gather(processed_data, cluster, mean, -matches(factor_to_explore[1]), -matches(factor_to_explore[2]), -matches(factor_to_explore[3]))
        out <- ggplot(to_plot, aes(y = mean, fill = cluster)) +
            aes_string(x = factor_to_explore[1]) +
            facet_wrap(as.formula(paste("~", factor_to_explore[2], " + ", factor_to_explore[3]))) +
            geom_bar(stat = "identity", color = "black") +
            xlab("") +
            ylab("Proportion of Responses") +
            ggtitle("") +
            theme(legend.position = "top") +
            theme(legend.title = element_blank()) +
            theme(text=element_text(size = 12, family = "Times")) +
            theme(axis.text.x = element_text(angle = 90)) +
            theme(legend.position = "right") +
            theme(legend.title = element_blank()) +
            scale_fill_discrete(name = "Cluster", labels = cluster_names)
        out
    }
    return(out)
}

# create_compare_manova <- function(processed_data, factor_to_explore){
#     factor_to_explore <- "race"
#     processed_data <- explored_factors[[2]]
#     df <- as.data.frame(processed_data)
#     df$DV <- as.matrix(cbind(df[, 3:ncol(df)]))
#     x <- paste0("DV ~ ", factor_to_explore, sep = "")
#     out <- manova(as.formula(x), data = df)
#     return(out)
# }

create_compare_anova <- function(processed_data, variable_to_find_proportion, cluster_names, factor_to_explore){
    df <- processed_data
    out <- list()
    out_tukey <- list()
    
    for_one <- function(processed_data, variable_to_find_proportion, cluster_names){
        if (!is.null(variable_to_find_proportion)){
            names(df)[2] <- "DV"
            for (i in 1:(ncol(processed_data) - 2)){
                x <- paste0("cluster", i, " ~ DV", sep = "")
                out[[i]] <- summary(aov(as.formula(x), data = df))
                out_tukey[[i]] <- TukeyHSD(aov(as.formula(x), data = df))
            }
        } else {
            names(df)[1] <- "DV"
            for (i in 1:(ncol(processed_data) - 1)){
                x <- paste0("cluster", i, " ~ DV", sep = "")
                out[[i]] <- summary(aov(as.formula(x), data = df))
                out_tukey[[i]] <- TukeyHSD(aov(as.formula(x), data = df))
            }
        }
        out <- list(out, out_tukey)
        names(out[[1]]) <- cluster_names
        names(out[[2]]) <- cluster_names
        return(out)
    }

    for_two <- function(processed_data, variable_to_find_proportion, cluster_names){
        if (!is.null(variable_to_find_proportion)){
            names(df)[2] <- "DV1"
            names(df)[3] <- "DV2"
            for (i in 1:(ncol(processed_data) - 3)){
                x <- paste0("cluster", i, " ~ DV1*DV2", sep = "")
                out[[i]] <- summary(aov(as.formula(x), data = df))
                out_tukey[[i]] <- TukeyHSD(aov(as.formula(x), data = df))
            }
        } else {
            names(df)[1] <- "DV1"
            names(df)[2] <- "DV2"
            for (i in 1:(ncol(processed_data) - 2)){
                x <- paste0("cluster", i, " ~ DV1*DV2", sep = "")
                out[[i]] <- summary(aov(as.formula(x), data = df))
                out_tukey[[i]] <- TukeyHSD(aov(as.formula(x), data = df))
            }
        }
        out <- list(out, out_tukey)
        names(out[[1]]) <- cluster_names
        names(out[[2]]) <- cluster_names
        return(out)
    }
    
    for_three <- function(processed_data, variable_to_find_proportion, cluster_names){
        if (!is.null(variable_to_find_proportion)){
            names(df)[2] <- "DV1"
            names(df)[3] <- "DV2"
            names(df)[4] <- "DV2"
            for (i in 1:(ncol(processed_data) - 4)){
                x <- paste0("cluster", i, " ~ DV1*DV2*DV3", sep = "")
                out[[i]] <- summary(aov(as.formula(x), data = df))
                out_tukey[[i]] <- TukeyHSD(aov(as.formula(x), data = df))
            }
        } else {
            names(df)[1] <- "DV"
            names(df)[2] <- "DV2"
            names(df)[3] <- "DV3"
            for (i in 1:(ncol(processed_data) - 3)){
                x <- paste0("cluster", i, " ~ DV1*DV2*DV3", sep = "")
                out[[i]] <- summary(aov(as.formula(x), data = df))
                out_tukey[[i]] <- TukeyHSD(aov(as.formula(x), data = df))
            }
        }
        out <- list(out, out_tukey)
        names(out[[1]]) <- cluster_names
        names(out[[2]]) <- cluster_names
        return(out)
    }
    
    if (length(factor_to_explore) == 1) {
        out <- for_one(processed_data, variable_to_find_proportion, cluster_names)
    } else if (length(factor_to_explore) == 2) {
        out <- for_two(processed_data, variable_to_find_proportion, cluster_names)
    } else if (length(factor_to_explore) == 3) {
        out <- for_three(processed_data, variable_to_find_proportion, cluster_names)
    }
    
    # names(out) <- cluster_names
    return(out)
}

# Outlier detection

uv_outlier_detector <- function(x, na.rm = T, ...) {
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    return(y)
}

remove_uv_out_func <- function(data){
    x <- sapply(data, uv_outlier_detector)
    return(x)
}

remove_mv_out_func <- function(data){
    mvout <- chemometrics::Moutlier(data, quantile = 0.99, plot = F)
    the_index <- which(mvout$md > mvout$cutoff)
    if (any(the_index) == T){
        return(the_index)
    }
}

# nrow(data_ss)
# data_ss <- data_ss[-the_index, ] # removes outliers!
# nrow(data_ss)
# 
# raw_data_matrix <- data_ss[, 1:3]
# # For cross-validation
# 
# splitting_halves <- function(x){
#     
# }
# 
# str(prepared_data)
# cross_validation_df$half <- sample(c(rep(1, 1939), rep(2, 1939)), 3878)
# 
# cross_validation_df_1 <- filter(cross_validation_df, half == 1)
# cross_validation_df_2 <- filter(cross_validation_df, half == 2)
# 
# cross_validation_df_1$half <- NULL
# cross_validation_df_2$half <- NULL
# 
# cv_1_out <- cluster_mat(cross_validation_df_1, n_clusters) # clustering
# cv_2_out <- cluster_mat(cross_validation_df_2, n_clusters) # clustering
# 
# cv_1_mat <- as.matrix(cross_validation_df_1)
# cv_2_mat <- as.matrix(cross_validation_df_2)
# 
# cv_1_out[[9]]
# cv_2_out[[9]]
# 
# # We need to find the cluster centroid in cv 1 to which each row in cv 2 is closest
# # We create a new assignment, cv_2_out_star, to compare with PA and kappa to cv_2_out[[2]]
# 
# Sys.setenv("PKG_CXXFLAGS"="-std=c++0x") # needed for the lambda functions in Rcpp
# library(imputation)
# 
# a <- as.matrix(cv_1_out[[9]]) # this is "A"
# # cv_1_mat # this is sample "A" - not needed
# b <- as.matrix(cv_2_mat) # this is raw data for "B"
# 
# a
# b
# 
# # # which row of a is closest to each row of b
# # # this creates "A*"
# # cv_2_out_star <- apply(b, 1, function(i, a) {
# #     which.min(imputation:::dist_q.matrix(rbind(i, a), ref= 1L, q=2))
# # }, a= a)
# #
# # cv_2_out[[2]] # this is "B"
# #
# # cv_2_out_star # this is "A*"
# #
# # table(cv_2_out_star, cv_2_out[[2]]) # this is "A*" compared to B in a table, but is not optimized
# #
# # ###
# # # Optimizing clusters
# # ###
# #
# # cv_1_mat <- as.data.frame(cv_1_mat)
# # cv_1_mat$cluster_A_star <- cv_1_out[[2]] # this adds the "A*" assignments to "A"
# #
# # tmp <- cv_1_mat %>% # this finds the "A*" centroids
# #     group_by(cluster_A_star) %>%
# #     summarize(behavioral = mean(behavioral_scale_ind),
# #               cognitive = mean(cognitive_scale_ind),
# #               affective = mean(affective_scale_ind)) %>%
# #     select(behavioral:affective) %>%
# #     as.data.frame()
# #
# # a <- as.matrix(tmp)
# # b <- as.matrix(cv_2_out[[9]])
# # b <- b[, 1:3]
# #
# # a # "A*" centroids
# # b # "B" centroids
# #
# # library(pdist)
# #
# # x <- pdist(a, b)
# # # If mypdist = pdist(X,Y), mypdist[i,j] is the distance between X[i,] and Y[j,].
# # y <- matrix(x@dist, nrow = nrow(b))
# # row.names(y) <- paste0("a_c", 1:nrow(b))
# # colnames(y) <- paste0("b_c", 1:nrow(b))
# # y
# #
# # out <- matrix(nrow = nrow(b), ncol = 2)
# # out
# # # not sure this is the best way to optimize
# # for (i in 1:nrow(b)){
# #     tmp_dim <- which(y == min(y), arr.ind = TRUE)
# #     out[i, 2] <- tmp_dim[1]
# #     out[i, 1] <- tmp_dim[2]
# #     y[tmp_dim[1], ] <- 1000
# #     y[, tmp_dim[2]] <- 1000
# # }
# #
# # out
# #
# # a # "A*" centroids
# # b # "B" centroids
# #
# # out
# #
# # # just have to recode one using 'out'
# #
# # table(tmp, cv_2_out[[2]])
# #
# # tmp <- cv_2_out_star
# #
# # # start here - need to find a programmatic way to recode them
# #
# # # for (i in 1:nrow(out)){
# # #     for (j in 1:length(tmp)){
# # #         tmp[j] <- ifelse(tmp[j] == out[i, 1], out[i, 2], tmp[j])
# # #     }
# # # }
# #
# # table(tmp)
# # out
# # table(tmp, cv_2_out[[2]])
# #
# # new_mat <- cbind(c, d)
# # library(irr)
# #
# # kappa2(new_mat)
# # agree(new_mat)
# #
# # table(cv_2_out_star_ss, cv_2_out[[2]]) # this is "A*" compared to B in a table, but is not optimized
# #
# # # we need to find out which A* centroid is most similar to which B centroid
# #
# # cv_2_out <- data.frame(cv_2_mat, cluster = cv_2_out_star)
# #
# # # Almost there; now we need to optimize the clusters
# #
# # cv_2_out_star
# 
# # Diagnostics

# mclust
# library(mclust)
# 
# X <- data_ss[, 1:3]
# X <- X[complete.cases(X), ]
# 
# BIC = mclustBIC(X)
# plot(BIC)
# BIC
# summary(BIC)
# 
# mod1 = Mclust(X, x = BIC)
# plot(mod1)
# str(mod1)
# summary(mod1, parameters = TRUE)
# 
# mod1$classification # assignment
# 
# ICL = mclustICL(X)
# summary(ICL)
# 
# plot(ICL)
# 
# LRT = mclustBootstrapLRT(X, modelName = "VVV")
# LRT
