# x <- data.frame(x)
# 
# names(x)
# 
# x$deviation <- x$X1 - x$X2
# 
# plot(x$deviation)
# 
# paste0(paste0(letters, 2, sep = ""), collapse = ",")
# 
# ?paste
# # ilp.R
# 
# # need 
# 
# str(survey_df)
# str(factor_df)
# str(statistics)
# 
# data_for_ilps <- explore_factors(statistics,
#                                  factor_df,
#                                  variable_to_find_proportion = "stud_ID",
#                                  factor_to_explore = "gender")
# 
# data_for_ilps[[4]]
# 
# data_for_ilps_ss <- data_for_ilps[[2]][, 3:8]
# 
# names(data_for_ilps_ss) <- c("Moderately Full", "Rational", "Full", "Reluctant", "Universally Low", "Pleasurable")
# 
# str(data_for_ilps_ss)
# 
# prepared_data <- prepare_data(data_for_ilps_ss, "raw", grouping_vector = NULL,
#                               to_standardize = F, remove_uv_outliers = F,
#                               remove_mv_outliers = F, print_status = T) # this won't work when no cases are removed
# 
# created_profiles <- create_profiles(prepared_data, 6, "squared_euclidean", "complete")
# 
# new_cluster_names = c("Moderately Full (n = 42)",
#                   "Rational (n = 30)",
#                   "Universally Low (n = 28)",
#                   "Reluctant (n = 52)",
#                   "Pleasurable (n = 66)",
#                   "Full (n = 26)")
# 
# statistics <- calculate_stats(created_profiles,
#                               to_standardize = F,
#                               plot_uncentered_data = F,
#                               cluster_names =  new_cluster_names,
#                               # fill_order = c("Universally Low", "Reluctant", "Pleasurable", "Rational", "Moderately Full", "Full"),
#                               font_size = 22,
#                               print_status = T)
# 
# statistics[[8]] +
#     scale_x_discrete(limits = rev(c("Full (n = 26)", "Moderately Full (n = 42)", "Rational (n = 30)",
#                                 "Pleasurable (n = 66)", "Reluctant (n = 52)", "Universally Low (n = 28)"))) +
#     xlab("Individual-Level Profile") +
#     ylab("Proportion of Momentary Responses for Individuals") +
#     guides(fill = guide_legend(title.position = "top", title.hjust = .5)) +
#     scale_fill_discrete("MEP") +
#     ggtitle("ILPs Constructed Through Six-Cluster Solution")
# 
# ggsave("ilp_cluster_analysis.png", width = 10, height = 7.5)
# 
# df <- data.frame(data_for_ilps_ss, cluster = statistics[[4]])
# 
# str(df)
# 
# df_p <- data_for_ilps_ss
# df_p$which_max <- factor(apply(data_for_ilps_ss, 1, which.max), levels = 1:6)
# df_p$max <- apply(data_for_ilps_ss, 1, max)
# 
# str(df_p)
# 
# to_p <-
#     df_p %>% 
#         select(-max) %>% 
#         group_by(which_max) %>% 
#         summarize_each(funs(mean)) %>% 
#         tidyr::gather(key, val, -which_max)
#                        
# ggplot(to_p, aes(x = which_max, y = val, fill = key)) +
#     geom_bar(stat = "identity", position = "dodge") +
#     ylab("Proportion of Momentary Responses for Individuals") +
#     xlab("Individual-Level Profile") +
#     theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
#     theme(legend.position = "top") +
#     theme(text=element_text(size = 16, family = "Times")) +
#     scale_fill_discrete("MEP") +
#     scale_x_discrete(labels = c("Moderately Full (n = 47)", "Rational (n = 27)", "Full (n = 25)",
#                                 "Reluctant (n = 47)", "Universally Low (n = 56)", "Pleasurable (n = 42)")) + 
#     ggtitle("ILPs Constructed on Basis of Most Frequent MEP")
# 
# ggsave("ilp_most_freq.png", width = 10, height = 7)
# 
# p_all_raw <- ggplot(df_p, aes(x = max, group = which_max, color = which_max)) +
#     geom_density(size = .75) +
#     scale_color_brewer(type = "qual", palette = 1) +
#     scale_color_discrete("", labels = c("1" = "Moderately Full", "2" = "Rational", "3" = "Universally Low", "4" = "Reluctant", "5"= "Pleasurable", "6" = "Full")) +
#     xlim(0, 1) +
#     xlab("Proportion of Momentary Responses") +
#     ylab("Density") +
#     ggtitle("Density of Proportion of Momentary Responses by Most Likely Momentary Response")
# 
# p_all_raw
# 
# ggsave("p_all_raw.png", width = 8.5, height = 6)
# 
# p_raw <- ggplot(df_p, aes(x = max)) +
#     geom_density(size = .75) +
#     xlim(0, 1) +
#     xlab("Proportion of Momentary Responses") +
#     ylab("Density") +
#     ggtitle("Density of Proportion of Momentary Responses for All Responses")
# 
# ggsave("p_raw.png", width = 8.5, height = 6)
# 
# str(df)
# 
# df$cluster_max <- apply(df[, 1:6], 1, max)
# df$cluster <- factor(df$cluster)
# 
# str(df)
# 
# p_all_cluster <- ggplot(df, aes(x = cluster_max, group = cluster, color = cluster)) +
#     geom_density(size = .75) +
#     scale_color_brewer(type = "qual", palette = 1) +
#     # scale_color_discrete("", labels = c("1" = "Moderately Full", "2" = "Rational", "3" = "Universally Low", "4" = "Reluctant", "5"= "Pleasurable", "6" = "Full")) +
#     xlim(0, 1) +
#     xlab("Proportion of Momentary Responses") +
#     ylab("Density") +
#     ggtitle("Density of Proportion of Momentary Responses by Most Likely Momentary Response")
# 
# p_all_cluster
# ggsave("p_all_cluster.png", width = 8.5, height = 6)
# #
# # # merging
# #
# # data_to_merge <- data_for_ilps[[2]]
# # data_to_merge$stud_ID <- as.character(data_to_merge$stud_ID)
# # data_to_merge <- dplyr::select(data_to_merge, stud_ID)
# #
# # factor_df$stud_ID <- as.character(factor_df$stud_ID)
# # factor_df_ss <- factor_df[!duplicated(factor_df$stud_ID), ]
# #
# # ilp_factor_df <- dplyr::left_join(data_to_merge, factor_df_ss, by = "stud_ID")
# #
# # ilp_factor_df_ss <- dplyr::left_join(data_to_merge, survey_df, by = "stud_ID")
# #
# # ilp_factor_df_ss$the_gender <- as.vector(ilp_factor_df_ss$gender)
# # ilp_factor_df_ss$the_futureplan <- as.vector(ilp_factor_df_ss$futureplan)
# # ilp_factor_df_ss$CumGPA_2010 <- round(as.numeric(as.vector(ilp_factor_df_ss$CumGPA_2010)))
# # ilp_factor_df_ss$the_acadexpect <- as.vector(ilp_factor_df_ss$acadexpect)
# # ilp_factor_df_ss$scigrades <- as.vector(ilp_factor_df_ss$scigrades)
# # ilp_factor_df_ss$the_lovestudy1 <- as.vector(ilp_factor_df_ss$lovestudy1)
# # ilp_factor_df_ss$the_expectwell <- as.vector(ilp_factor_df_ss$expectwell)
# # ilp_factor_df_ss$the_percom <- round(as.numeric(as.vector(ilp_factor_df_ss$percom)))
# # ilp_factor_df_ss$the_int_scale <- round((as.vector(ilp_factor_df_ss$interesfun1) + as.vector(ilp_factor_df_ss$interestjob1)) / 2)
# # ilp_factor_df_ss$the_interestsci <- as.vector(ilp_factor_df_ss$interestsci)
# # ilp_factor_df_ss$the_relevant1 <- as.vector(ilp_factor_df_ss$relevant1)
# #
# # ilp_factor_df_p <- ilp_factor_df_ss[!is.nan(ilp_factor_df_ss$the_int_scale), ]
# #
# # explored_factors <- explore_factors(statistics,
# #                                     ilp_factor_df,
# #                                     factor_to_explore = c("gender", "race"),
# #                                     cluster_names = new_cluster_names,
# #                                     print_status = T)
# #
# # explored_factors[[3]]
# # explored_factors[[4]] + theme(text=element_text(size = 17, family = "Times")) +
# #     theme(axis.text.x = element_text(angle = 45, hjust = 1.05))
# #
# #
# # ggsave("race_gender_11-29_16.png")
# # explored_factors[[6]][[1]]
# #
# # explored_factors <- explore_factors(statistics,
# #                                     ilp_factor_df,
# #                                     factor_to_explore = c("efficacy"),
# #                                     cluster_names = new_cluster_names,
# #                                     print_status = T)
# # #
# # explored_factors[[1]]
# # explored_factors[[3]]
# # explored_factors[[4]]
# # explored_factors[[6]][[1]]
# # explored_factors[[6]][[2]][3]
# #
# # str(explored_factors[[2]])
# #
# # new_cluster_names = c("Moderately Full",
# #                       "Rational",
# #                       "Universally Low",
# #                       "Reluctant",
# #                       "Pleasurable",
# #                       "Full")
# #
# # x <- cbind(explored_factors[[2]],
# #            gender = ilp_factor_df_ss$the_gender,
# #            gpa = ilp_factor_df_ss$CumGPA_2010,
# #            subject = ilp_factor_df_ss$subject,
# #            teach = as.factor(ilp_factor_df_ss$teacher_ID),
# #            percom = ilp_factor_df_ss$the_percom,
# #            int = ilp_factor_df_ss$the_int_scale,
# #            stud_ID = ilp_factor_df_ss$stud_ID,
# #            race = ilp_factor_df_ss$race)
# #
# # x$gender
# #
# # x$urm <- ifelse(x$race != 4 & x$race != 1, 1, 0)
# # x$hispanic <- ifelse(x$race == "Hispanic", 1, 0)
# # x$other <- ifelse(x$race != "Hispanic" & x$race != "White", 1, 0)
# # x$other <- ifelse(is.na(x$race), 1, x$other)
# 
# # new_cluster_names = c("Moderately Full",
# #                       "Rational",
# #                       "Universally Low",
# #                       "Reluctant",
# #                       "Pleasurable",
# #                       "Full")
# 
# # gender by race
# #
# # model1 <- glm(cluster1 ~ gender + urm + int + percom + gpa, family = binomial(link = "logit"), data = x)
# # arm::display(model1)
# # summary(model1)
# # model2 <- glm(cluster2 ~ gender + urm + int + percom + gpa, family = binomial(link = "logit"), data = x)
# # arm::display(model2)
# # summary(model2)
# # model3 <- glm(cluster3 ~ gender + urm + int + percom + gpa, family = binomial(link = "logit"), data = x)
# # arm::display(model3)
# # summary(model3)
# # model4 <- glm(cluster4 ~ gender + urm + int + percom + gpa, family = binomial(link = "logit"), data = x)
# # arm::display(model4)
# # summary(model4)
# # model5 <- glm(cluster5 ~ gender + urm + int + percom + gpa, family = binomial(link = "logit"), data = x)
# # arm::display(model5)
# # summary(model5)
# # model6 <- glm(cluster6 ~ gender + urm + int + percom + gpa, family = binomial(link = "logit"), data = x)
# # arm::display(model6)
# # summary(model6)
# # #
# # model1 <- lme4::glmer(cluster1 ~ int + percom + gender + white + hispanic + gpa +
# #                           (1 | teach),
# #                       family = "binomial",
# #                       data = x)
# # summary(model1)
# #
# # model2 <- lme4::glmer(cluster2 ~ int + percom + gender + white + hispanic + gpa +
# #                           (1 | teach),
# #                       family = "binomial",
# #                       data = x)
# # summary(model2)
# #
# # model3 <- lme4::glmer(cluster3 ~ int + percom + gender + white + hispanic + gpa +
# #                           (1 | teach),
# #                       family = "binomial",
# #                       data = x)
# # summary(model3)
# #
# # model4 <- lme4::glmer(cluster4 ~ int + percom + gender + white + hispanic + gpa +
# #                           (1 | teach),
# #                       family = "binomial",
# #                       data = x)
# # summary(model4)
# #
# # model5 <- lme4::glmer(cluster5 ~ int + percom + gender + white + hispanic + gpa +
# #                           (1 | teach),
# #                       family = "binomial",
# #                       control=lme4::glmerControl(optimizer="bobyqa",
# #                                                  optCtrl=list(maxfun=2e5)),
# #                       data = x)
# # summary(model5)
# #
# # model6 <- lme4::glmer(cluster6 ~ int + percom + gender + white + hispanic + gpa +
# #                           (1 | teach),
# #                       family = "binomial",
# #                       data = x)
# # summary(model6)