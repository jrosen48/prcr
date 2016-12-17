# helper_functions.R

#' centers the data
#'
#' @export

centering_function <- function(data, method_of_centering, grouping_vector, to_standardize = F){
    center_this <- function(x){
        x - mean(x, na.rm = T)
    }
    
    scale_this <- function(x) {
        if (stats::sd(x, na.rm = T) == 0){
            x - mean(x, na.rm = T)
        } else {
            (x - mean(x)) / stats::sd(x)
        }
    }
    if (method_of_centering == "grand" & to_standardize == F) {
        out <- sapply(data, function(x) center_this(x))
        out <- as.data.frame(out)
    }
    if (method_of_centering == "group" & to_standardize == F) {
        out <- data %>%
            cbind(grouping_vector) %>%
            dplyr::group_by(grouping_vector) %>%
            dplyr::mutate_each(dplyr::funs(center_this))
        out <- as.data.frame(out[, 1:ncol(data)])
    }
    if (method_of_centering == "grand" & to_standardize == T) {
        out <- sapply(data, function(x) scale_this(x))
        out <- as.data.frame(out)
    }
    if (method_of_centering == "group" & to_standardize == T) {
        out <- data %>%
            cbind(grouping_vector) %>%
            dplyr::group_by(grouping_vector) %>%
            dplyr::mutate_each(dplyr::funs(scale_this))
        out <- as.data.frame(out[, 1:ncol(data)])
    }
    if (method_of_centering == "raw") {
        out <- as.data.frame(data)
    }
    return(out)
}

#' calculates distance
#'
#' @export

distance_function <- function(x, distance_metric = "squared_euclidean"){
    if (distance_metric != "squared_euclidean") {
        distance <- stats::dist(x, method = distance_metric)
    } else {
        distance <- stats::dist(x, method = "euclidean")
        distance <- distance ^ 2
    }
    return(distance)
}

#' converts hclust output to start values for kmeans function
#'
#' @export

hclust_to_kmeans_function <- function(data, out, n_clusters){
    # This function processes the output from the hierarchical clustering to be used as starting points for the kmeans clustering
    cut_hclust <- stats::cutree(out, n_clusters) # cuts the results of the hierarchical cluster at the specified # of clusters
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

#' carries out kmeans cluster analysis
#'
#' @export

kmeans_function <- function(data, cluster_freqs){
    start <- data.frame(matrix(unlist(cluster_freqs), nrow=length(cluster_freqs[[1]]), byrow = T), stringsAsFactors = F)
    start <- as.matrix(start)
    start <- t(start)
    return(stats::kmeans(data, start))
}

#' calculates dissimilarity matrix
#'
#' @export

dissim_function <- function(hc){
    data.frame(row.names = paste0("Cluster", seq_along(hc$height)),
               height = hc$height,
               component_1 = paste0("Cluster", hc$merge[, 1]),
               component_2 = paste0("Cluster", hc$merge[, 2]),
               stringsAsFactors=FALSE)
}

#' standardizes raw data
#'
#' @export

standardize_function <- function(data){
    standardized_data <- scale(data, center = F, scale = T)
    return(standardized_data)
}

#' calculates cluster centroids
#'
#' @export

cluster_freq_function <- function(data, n_clusters, kfit, variable_names){
    clusters <- list()
    for (i in 1:n_clusters){
        clusters[[i]] <- data[kfit$cluster == i, ]
    }
    cluster_freqs <- list()
    for (i in seq(n_clusters)){
        cluster_freqs[[i]] <- colSums(clusters[[i]]) / nrow(clusters[[i]]) # Need to fix - will want to add group freqs
    }
    cluster_freqs <- as.data.frame(matrix(unlist(cluster_freqs), nrow = n_clusters, byrow = T))
    names(cluster_freqs) <- variable_names
    cluster_freqs$Cluster <- paste0("Cluster ", 1:n_clusters, ": ", table(kfit$cluster), " Obs.")
    return(cluster_freqs)
}

#' creates plot of cluster centroids
#'
#' @export

cluster_plot_function <- function(cluster_freqs, font_size, fill_order){
    cluster_freqs_tmp <- tidyr::gather(cluster_freqs, Var, Value, -Cluster)
    if (!is.null(fill_order)){
        cluster_freqs_tmp$Var <- factor(cluster_freqs_tmp$Var, levels = fill_order)
        #cluster_freqs_tmp <- cluster_freqs_tmp[match(fill_order, cluster_freqs_tmp$Var), ]
    }
    if (is.null(cluster_names)){
        clusters_p <- ggplot2::ggplot(cluster_freqs_tmp, ggplot2::aes(x = Cluster, y = Value, fill = Var)) +
            ggplot2::geom_bar(stat = "identity", position = "dodge") +
            ylab("") +
            xlab("") +
            ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            ggplot2::theme(legend.position = "top") +
            ggplot2::theme(text=element_text(size = font_size, family = "Times"))
    } else {
        clusters_p <- ggplot2::ggplot(cluster_freqs_tmp, ggplot2::aes(x = Cluster, y = Value, fill = Var)) +
            ggplot2::geom_bar(stat = "identity", position = "dodge") +
            ylab("") +
            xlab("") +
            ggplot2::theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            ggplot2::theme(legend.position = "top") +
            ggplot2::theme(text=element_text(size = font_size, family = "Times"))
    }
    return(clusters_p)
}

#' carries out Tukey HSD test
#'
#' @export

testing_the_tukey <- function(data){
    tukey_list <- list()
    for (i in 1:(ncol(data) - 2)){
        fit <- aov(data[, i] ~ as.character(data$cluster_assignment))
        tukey_list[[i]] <- stats::TukeyHSD(fit)
    }
    names(tukey_list) <- names(data[, 1:3])
    return(tukey_list)
}

#' carries out MANOVA test
#'
#' @export

try_manova <- function(data, cluster_assignment, variable_names){
    
    manova_function <- function(data, cluster_assignment, variable_names){
        out <- list()
        data$DV <- as.matrix(data)
        data <- cbind(data, cluster_assignment)
        mv_out <- stats::manova(DV ~ cluster_assignment, data = data)
        out[[1]] <- summary(mv_out, test = "Wilks")
        out[[2]] <- summary.aov(mv_out)
        out[[3]] <- testing_the_tukey(data)
        names(out[[3]]) <- variable_names
        return(out)
    }
    
    out <- tryCatch(
        {
        manova_function(data, cluster_assignment, variable_names)  
        },
        error = function(cond){
            return(cond)
        }
    )
    return(out)
}

#' merge assignments with selected factors
#'
#' @export

merge_assignments_and_factors <- function(cluster_assignments, cases_to_keep, factor_data_frame){
    factor_data_frame_ss <- factor_data_frame[cases_to_keep, ]
    data <- data.frame(cluster = cluster_assignments, factor_data_frame_ss)
    return(data)
}

#' create crosstabs
#'
#' @export

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

#' dummy code cluster assignments
#'
#' @export

dummmy_code_cluster_assignments <- function(data){
    data$cluster <- as.factor(data$cluster)
    tmp <- as.data.frame(model.matrix( ~ cluster - 1, data = data))
    out <- data.frame(data[, 2:ncol(data)], tmp)
    return(out)
}

#' create raw data for ANOVA and subsequent processing
#'
#' @export

create_raw_data <- function(dummy_coded_data, factor_to_explore, variable_to_find_proportion){
    
    for_one <- function(dummy_coded_data, factor_to_explore, variable_to_find_proportion){
        if (!is.null(variable_to_find_proportion)) {
            raw_data <- dummy_coded_data %>%
                dplyr::select(dplyr::matches(variable_to_find_proportion), dplyr::matches(factor_to_explore), dplyr::contains("cluster")) %>%
                dplyr::group_by_(variable_to_find_proportion, factor_to_explore) %>%
                dplyr::summarize_each(dplyr::funs(mean)) %>%
                dplyr::ungroup()
        } else {
            raw_data <- dummy_coded_data %>%
                dplyr::select(dplyr::matches(factor_to_explore), dplyr::contains("cluster")) %>%
                dplyr::ungroup()
        }
        return(raw_data)
    }
    
    for_two <- function(dummy_coded_data, factor_to_explore, variable_to_find_proportion){
        if (!is.null(variable_to_find_proportion)) {
            raw_data <- dummy_coded_data %>%
                dplyr::select(dplyr::matches(variable_to_find_proportion), dplyr::matches(factor_to_explore[1]), dplyr::matches(factor_to_explore[2]), dplyr::contains("cluster")) %>%
                dplyr::group_by_(variable_to_find_proportion, factor_to_explore) %>%
                dplyr::summarize_each(dplyr::funs(mean)) %>%
                ungroup()
        } else {
            raw_data <- dummy_coded_data %>%
                dplyr::select(dplyr::matches(factor_to_explore[1]), dplyr::matches(factor_to_explore[2]), dplyr::contains("cluster")) %>%
                dplyr::ungroup()
        }
        return(raw_data)
    }
    
    for_three <- function(dummy_coded_data, factor_to_explore, variable_to_find_proportion){
        if (!is.null(variable_to_find_proportion)) {
            raw_data <- dummy_coded_data %>%
                dplyr::select(dplyr::matches(variable_to_find_proportion), dplyr::matches(factor_to_explore[1]), dplyr::matches(factor_to_explore[2]), dplyr::matches(factor_to_explore[3]), dplyr::contains("cluster")) %>%
                dplyr::group_by_(variable_to_find_proportion, factor_to_explore) %>%
                dplyr::summarize_each(dplyr::funs(mean)) %>%
                dplyr::ungroup()
        } else {
            raw_data <- dummy_coded_data %>%
                dplyr::select(dplyr::matches(factor_to_explore), dplyr::matches(factor_to_explore[1]), dplyr::matches(factor_to_explore[2]), dplyr::matches(factor_to_explore[3]), dplyr::contains("cluster")) %>%
                dplyr::ungroup()
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

#' finds n by condition 
#'
#' @export

find_n <- function(raw_data, factor_to_explore){
    
    if (length(factor_to_explore) == 1) {
        out <- 
            raw_data %>%
            dplyr::group_by_(factor_to_explore) %>%
            dplyr::summarize(n = n())
    } else if (length(factor_to_explore) == 2) {
        out <- 
            raw_data %>%
            dplyr::group_by_(factor_to_explore[1], factor_to_explore[2]) %>%
            dplyr::summarize(n = n())    
    } else if (length(factor_to_explore) == 3) {
        out <- 
            raw_data %>%
            dplyr::group_by_(factor_to_explore[1], factor_to_explore[2], factor_to_explore[3]) %>%
            dplyr::summarize(n = n())
    }
    
}

#' creates process data for summary table and plot
#'
#' @export

create_processed_data <- function(raw_data, factor_to_explore, variable_to_find_proportion){
    
    for_one <- function(dummy_coded_data, factor_to_explore, variable_to_find_proportion){
        if (!is.null(variable_to_find_proportion)) {
            processed_data <- raw_data %>%
                dplyr::select(-dplyr::matches(variable_to_find_proportion)) %>%
                dplyr::group_by_(factor_to_explore) %>%
                dplyr::summarize_each(dplyr::funs(mean))
        } else {
            processed_data <- raw_data %>%
                dplyr::group_by_(factor_to_explore) %>%
                dplyr::summarize_each(dplyr::funs(mean))
        }
        return(processed_data)
    }
    
    for_two <- function(dummy_coded_data, factor_to_explore, variable_to_find_proportion){
        if (!is.null(variable_to_find_proportion)) {
            processed_data <- raw_data %>%
                dplyr::select(-dplyr::matches(variable_to_find_proportion)) %>%
                dplyr::group_by_(factor_to_explore[1], factor_to_explore[2]) %>%
                dplyr::summarize_each(dplyr::funs(mean))
        } else {
            processed_data <- raw_data %>%
                dplyr::group_by_(factor_to_explore[1], factor_to_explore[2]) %>%
                dplyr::summarize_each(dplyr::funs(mean))
        }
        return(processed_data)
    }
    
    for_three <- function(dummy_coded_data, factor_to_explore, variable_to_find_proportion){
        if (!is.null(variable_to_find_proportion)) {
            processed_data <- raw_data %>%
                dplyr::select(-dplyr::matches(variable_to_find_proportion)) %>%
                dplyr::group_by_(factor_to_explore[1], factor_to_explore[2], factor_to_explore[3]) %>%
                dplyr::summarize_each(dplyr::funs(mean))
        } else {
            processed_data <- raw_data %>%
                dplyr::group_by_(factor_to_explore[1], factor_to_explore[2], factor_to_explore[3]) %>%
                dplyr::summarize_each(dplyr::funs(mean))
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

#' create plot to explore factors
#'
#' @export

create_plot_to_explore_factors <- function(processed_data, factor_to_explore, cluster_names){
    processed_data <- processed_data[complete.cases(processed_data), ]
    if (length(factor_to_explore) == 1) {
        to_plot <- tidyr::gather(processed_data, cluster, mean, -dplyr::matches(factor_to_explore))
        # to_plot <- to_plot[!is.na(dplyr::matches(factor_to_explore)), ]
        out <- ggplot(dplyr::arrange(to_plot, desc(cluster)), ggplot2::aes(y = mean, fill = cluster)) +
            ggplot2::aes_string(x = factor_to_explore) +
            ggplot2::geom_bar(stat = "identity", color = "black") +
            xlab("") +
            ylab("Proportion of Responses") +
            ggtitle("") +
            ggplot2::theme(legend.position = "top") +
            ggplot2::theme(legend.title = element_blank()) +
            ggplot2::theme(text=element_text(size = 12, family = "Times")) +
            ggplot2::theme(axis.text.x = element_text(angle = 45)) +
            ggplot2::theme(legend.position = "right") +
            ggplot2::theme(legend.title = element_blank()) +
            ggplot2::scale_fill_discrete(name = "Cluster", labels = cluster_names)
    } else if (length(factor_to_explore) == 2) {
        to_plot <- tidyr::gather(processed_data, cluster, mean, -dplyr::matches(factor_to_explore[1]), -dplyr::matches(factor_to_explore[2]))
        out <- ggplot2::ggplot(dplyr::arrange(to_plot, desc(cluster)), ggplot2::aes(y = mean, fill = cluster)) +
            ggplot2::aes_string(x = factor_to_explore[1]) +
            ggplot2::facet_wrap(as.formula(paste("~", factor_to_explore[2]))) +
            ggplot2::geom_bar(stat = "identity", color = "black") +
            ggplot2::xlab("") +
            ggplot2::ylab("Proportion of Responses") +
            ggplot2::ggtitle("") +
            ggplot2::theme(legend.position = "top") +
            ggplot2::theme(legend.title = element_blank()) +
            ggplot2::theme(text=element_text(size = 12, family = "Times")) +
            ggplot2::theme(axis.text.x = element_text(angle = 45)) +
            ggplot2::theme(legend.position = "right") +
            ggplot2::theme(legend.title = element_blank()) +
            ggplot2::scale_fill_discrete(name = "Cluster", labels = cluster_names)
    } else if (length(factor_to_explore) == 3) {
        to_plot <- tidyr::gather(processed_data, cluster, mean, -matches(factor_to_explore[1]), -matches(factor_to_explore[2]), -matches(factor_to_explore[3]))
        out <- ggplot2::ggplot(dplyr::arrange(to_plot, desc(cluster)), ggplot2::aes(y = mean, fill = cluster, order =)) +
            ggplot2::aes_string(x = factor_to_explore[1]) +
            ggplot2::facet_wrap(as.formula(paste("~", factor_to_explore[2], " + ", factor_to_explore[3]))) +
            ggplot2::geom_bar(stat = "identity", color = "black") +
            ggplot2::xlab("") +
            ggplot2::ylab("Proportion of Responses") +
            ggplot2::ggtitle("") +
            ggplot2::theme(legend.position = "top") +
            ggplot2::theme(legend.title = element_blank()) +
            ggplot2::theme(text=element_text(size = 12, family = "Times")) +
            ggplot2::theme(axis.text.x = element_text(angle = 45)) +
            ggplot2::theme(legend.position = "right") +
            ggplot2::theme(legend.title = element_blank()) +
            ggplot2::scale_fill_discrete(name = "Cluster", labels = cluster_names)
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

#' compares between levels of factor using ANOVA
#'
#' @export

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
                out_tukey[[i]] <- stats::TukeyHSD(aov(as.formula(x), data = df))
            }
        } else {
            names(df)[1] <- "DV"
            for (i in 1:(ncol(processed_data) - 1)){
                x <- paste0("cluster", i, " ~ DV", sep = "")
                out[[i]] <- summary(aov(as.formula(x), data = df))
                out_tukey[[i]] <- stats::TukeyHSD(aov(as.formula(x), data = df))
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
                out_tukey[[i]] <- stats::TukeyHSD(aov(as.formula(x), data = df))
            }
        } else {
            names(df)[1] <- "DV1"
            names(df)[2] <- "DV2"
            for (i in 1:(ncol(processed_data) - 2)){
                x <- paste0("cluster", i, " ~ DV1*DV2", sep = "")
                out[[i]] <- summary(aov(as.formula(x), data = df))
                out_tukey[[i]] <- stats::TukeyHSD(aov(as.formula(x), data = df))
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
                out_tukey[[i]] <- stats::TukeyHSD(aov(as.formula(x), data = df))
            }
        } else {
            names(df)[1] <- "DV"
            names(df)[2] <- "DV2"
            names(df)[3] <- "DV3"
            for (i in 1:(ncol(processed_data) - 3)){
                x <- paste0("cluster", i, " ~ DV1*DV2*DV3", sep = "")
                out[[i]] <- summary(aov(as.formula(x), data = df))
                out_tukey[[i]] <- stats::TukeyHSD(aov(as.formula(x), data = df))
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

#' removes incomplete cases
#'
#' @export

removed_obs_df_maker <- function(raw_data_matrix, cases_to_keep){
    removed_obs_df <- data.frame(row = row.names(raw_data_matrix), raw_data_matrix, stringsAsFactors = F)
    removed_obs_df$reason_removed <- NA
    removed_obs_df$reason_removed[!cases_to_keep] <- "incomplete_case"
    return(removed_obs_df)
}

#' detects univariate outliers
#'
#' @export

uv_outlier_detector <- function(x, na.rm = T, ...) {
    # need to figure out where this came from - from a SO question, can probably re-write
    qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
    H <- 1.5 * IQR(x, na.rm = na.rm)
    y <- x
    y[x < (qnt[1] - H)] <- NA
    y[x > (qnt[2] + H)] <- NA
    return(y)
}

#' remove uv outliers
#'
#' @export

remove_uv_out_func <- function(data){
    x <- sapply(data, uv_outlier_detector)
    return(x)
}

#' remove mv outliers
#'
#' @export

remove_mv_out_func <- function(data){
    mvout <- chemometrics::Moutlier(data, quantile = 0.99, plot = F)
    the_index <- which(mvout$md > mvout$cutoff)
    if (any(the_index) == T){
        return(the_index)
    } else{
        return(data)
    }
}

#' main function to remove mv outliers
#'
#' @export

remove_mv_main_func <- function(data, removed_obs_df, cases_to_keep, found_uv_outlier_bool = F, uv_outliers = NULL, print_status){
    out_tmp <- remove_mv_out_func(data)
    if(print_status == T){
        print(paste0("### Note: ", length(out_tmp), " cases with multivariate outliers out of ", nrow(data), " cases removed, so ", nrow(data) - length(out_tmp), " used in subsequent analysis ###"))
    }
    x <- removed_obs_df[cases_to_keep, ]
    if(found_uv_outlier_bool == T){ 
        y <- x[-uv_outliers, ]
        z <- as.numeric(y$row[out_tmp])
    } else{
        z <- as.numeric(x$row[out_tmp])
    }
    removed_obs_df$reason_removed[z] <- "multivariate_outlier"
    data_out <- data[-out_tmp, ] # this is the first list item (data with mv outliers removed), second is the cases to be output as an attribute returned from prepare_data()
    data_out <- list(data_out, removed_obs_df)
    return(data_out)
}

#' compare cluster fit index statistics
#'
#' @export

comparision_of_statistics_plot <- function(data, lower_num, upper_num){
    ggplot2::ggplot(data, aes(x = number_of_clusters, y = proportion_of_variance_explained)) +
        ggplot2::geom_point() +
        # scale_x_continuous(breaks = lower_num:upper_num) +
        ggplot2::ylab("Proportion of Variance Explained (R^2)") +
        ggplot2::xlab("Number of Clusters")  
}

#' split halves
#'
#' @export

splitting_halves <- function(x){
    x <- data.frame(matrix(unlist(x), ncol = length(x), byrow = F))
    if (nrow(x) %/% 2 == 0){
        y1 <- nrow(x) / 2
        y2 <- y1
    } else{
        y1 <- nrow(x) %/% 2
        y2 <- y1 + 1
    }
    y <- c(rep(1, y1), rep(0, y2))
    z <- rnorm(nrow(x))
    df <- dplyr::arrange(data.frame(y, z), z)
    y_shuffled <- df$y
    half_one <- as.data.frame(x[y_shuffled == 0, ])
    half_two <- as.data.frame(x[y_shuffled == 1, ])
    out <- list(half_one, half_two)
    return(out)
}

#' try to cluster halves
#'
#' @export

try_to_cluster_halves <- function(prepared_data, args){
    out <- tryCatch({
        create_profiles(prepared_data, args[[2]], args[[3]], args[[4]], print_status = F)
        },
        error = function(cond){
            #warning("Did not properly converge, trying again.")
            return(NA)
        },
        finally = {
    
        }
    )
    return(out)
}

#' clustering the halves functions
#'
#' @export

cluster_the_halves <- function(split_halves, args){
    df <- data.frame(matrix(unlist(prepared_data), ncol = length(prepared_data), byrow = F))
    
    clustered_half_one <- try_to_cluster_halves(split_halves[[1]], args)
    if(!is.character(clustered_half_one)){
        clustered_half_one <- calculate_stats(clustered_half_one, print_status = F)[[5]]
        print(paste0("### Proportion of variance explained (R^2) = ", round(clustered_half_one, 3)))
    } else{
        clustered_half_one<- NA
    }
    
    clustered_half_two <- try_to_cluster_halves(split_halves[[2]], args)
    if(!is.character(clustered_half_two)){
        clustered_half_two <- calculate_stats(clustered_half_two, print_status = F)[[5]]
        print(paste0("### Proportion of variance explained (R^2) = ", round(clustered_half_two, 3)))
    } else{
        clustered_half_two <- NA
    }
    
    out <- list(clustered_half_one, clustered_half_two)
    return(out)
}

#' calculate stats for halves
#'
#' @export

calculate_the_stats <- function(clustered_halves, variable_names){ #fix
    half_one_stats <- calculate_stats(clustered_halves[[1]], variable_names, print_status = F)
    half_two_stats <- calculate_stats(clustered_halves[[2]], variable_names, print_status = F)
    out <- list(half_one_stats, half_two_stats)
    return(out)
}

#' finding the nearest centroids
#'
#' @export

find_nearest_centroid <- function(split_halves, calculated_stats){
    a <- split_halves[[1]] # keep
    a_assign <- calculated_stats[[1]][[3]] # keep
    b_centroid <- calculated_stats[[2]][[7]][, 1:3] # keep
    z <- fields::rdist(a, b_centroid)
    a_assign_star <- apply(z, 1, function(x) which.min(x))
    return(a_assign_star)
}

#' calculate agreement and kappa
#'
#' @export

calculate_agreement <- function(a_assign_star, a_assign){
    out <- list()
    tab <- table(a_assign_star, a_assign[[1]])
    res <- lpSolve::lp.assign(-tab)
    l <- apply(res$solution > 0.5, 1, which)
    a_assign_star_recode <- l[a_assign_star]
    tmp_mat <- data.frame(a_assign_star_recode, a_assign)
    out[[1]] <- irr::kappa2(tmp_mat)
    out[[2]] <- irr::agree(tmp_mat)
    return(out)
}