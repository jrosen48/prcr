# command 12-7

# command 11-29.R

#############################################
### 1. Setting up, loading data #############
#############################################

remove(list = ls())

setwd("~/Dropbox/research/prcr")
library(ggplot2)
library(multcomp)
devtools::install_github("jrosen48/prcr")
#devtools::load_all()
library(prcr)
library(magrittr)
library(tidyverse)
library(MASS)

engagement_df <- read.csv("~/Dropbox/research/scimo/raw_data_matrix.csv")
names(engagement_df) <- c("Behavioral Scale", "Cognitive Scale", "Affective Scale")

factor_df <- read.csv("~/Dropbox/research/scimo/factor_data_frame.csv")

factor_df <- dplyr::rename(factor_df, ip = instructional_practice)

survey_df <- haven::read_sav("~/documents/myscimo/scimo_ss.sav")
survey_df <- dplyr::rename(survey_df, stud_ID = Stud_ID)

esm_df <- haven::read_sav("~/documents/myscimo/scimo_esm.sav")

esm_df$ch_who[is.na(esm_df$ch_who)] <- 0
esm_df$ch_howdo[is.na(esm_df$ch_howdo)] <- 0
esm_df$ch_mat[is.na(esm_df$ch_mat)] <- 0
esm_df$ch_time[is.na(esm_df$ch_time)] <- 0
esm_df$ch_doing[is.na(esm_df$ch_doing)] <- 0
esm_df$ch_topic[is.na(esm_df$ch_topic)] <- 0
esm_df$ch_defin[is.na(esm_df$ch_defin)] <- 0
esm_df$ch_other[is.na(esm_df$ch_other)] <- 0
esm_df$ch_none[is.na(esm_df$ch_none)] <- 0

esm_df$ch_none <- ifelse(((esm_df$ch_who == 0 & esm_df$ch_howdo == 0 & esm_df$ch_mat == 0 & esm_df$ch_time == 0 & esm_df$ch_doing == 0 &
                               esm_df$ch_defin == 0 & esm_df$ch_topic == 0 & esm_df$ch_other == 0) & esm_df$ch_none == 0), 1, esm_df$ch_none)

esm_df$ch_none <- ifelse(((esm_df$ch_who == 1 | esm_df$ch_howdo == 1 | esm_df$ch_mat == 1 | esm_df$ch_time == 1 | esm_df$ch_doing |
                               esm_df$ch_defin == 1 | esm_df$ch_topic == 1 | esm_df$ch_other == 1) & esm_df$ch_none == 1), 0, esm_df$ch_none)

esm_df$ch_doing_defining_topic <- ifelse(esm_df$ch_defin == 1 | esm_df$ch_topic == 1 | esm_df$ch_doing, 1, 0)

which(esm_df$stud_ID == "AXR050594")
to_remove <- esm_df$stud_ID == "AXR050594" & esm_df$month == 10 & esm_df$day == 20 & esm_df$year == 8 & esm_df$signal == 2
esm_df <- esm_df[-to_remove, ]

#############################################
### 2. Cluster analysis #####################
#############################################

names(engagement_df) <- c("Behavioral Engagement ", "Cognitive Engagement ", "Affective Engagement ")

prepared_data <- prepare_data(engagement_df, "grand", grouping_vector = NULL, 
                              to_standardize = T, remove_uv_outliers = T,
                              remove_mv_outliers = T, print_status = T) # this won't work when no cases are removed

created_profiles <- create_profiles(prepared_data, 6, "squared_euclidean", "complete")

cluster_names = c("Full (n = 428)",
                  "Moderately Full (n = 694)",
                  "Pleasurable (n = 751)",
                  "Rational (n = 497)",
                  "Reluctant (n = 722)",
                  "Universally Low (n = 871)")

statistics <- calculate_stats(created_profiles, to_standardize = F, 
                              cluster_names = rev(cluster_names), plot_uncentered_data = T,
                              the_order = c(5, 3, 6, 2, 1, 4),
                              font_size = 17) 

p1 <- statistics[[8]] + 
    ylab("Z-score") + 
    theme(legend.position = "none") +
    theme(axis.title.y=element_text(margin=margin(0,20,0,0)))

p1

p2 <- statistics[[9]] + ylab("Raw Score") + 
    theme(legend.title=element_blank()) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())

# cowplot::plot_grid(p2, p1, nrow = 2, align = "v", rel_heights = c(1, 1.25))

p1

ggsave("mep_12-7-16.png", width = 9, height = 7)

######################################################################
### 3. IP ###################################
###################

esm_df$ip <- as.character(factor_df$ip)

esm_df$ip <- ifelse((esm_df$ip == "Discussion" | esm_df$ip == "Non-instructional" |
                         esm_df$ip == "Presentation" | esm_df$ip == "Video" | esm_df$ip == "Group Work"), "Other", esm_df$ip)

explored_factors <- explore_factors(statistics, 
                                    esm_df, 
                                    factor_to_explore = c("ip"), # first is x, second is faceted
                                    variable_to_find_proportion = NULL, 
                                    cluster_names = cluster_names,
                                    print_status = T)

# explored_factors[[2]] <- dplyr::filter(explored_factors[[2]], !is.na(ip))

explored_factors[[2]]$ip <- as.character(explored_factors[[2]]$ip)

explored_factors[[2]]$ip[explored_factors[[2]]$ip == "Individual Work"] <- "Individual Work (n = 651)"
explored_factors[[2]]$ip[explored_factors[[2]]$ip == "Laboratory"] <- "Laboratory (n = 991)"
explored_factors[[2]]$ip[explored_factors[[2]]$ip == "Lecture"] <- "Lecture (n = 542)"
explored_factors[[2]]$ip[explored_factors[[2]]$ip == "Other"] <- "Other (n = 1091)"
explored_factors[[2]]$ip[explored_factors[[2]]$ip == "Quiz and Test"] <- "Quiz and Test (n = 639)"

explored_factors[[2]]$ip <- factor(explored_factors[[2]]$ip,
                                   levels = c("Laboratory (n = 991)",
                                              "Individual Work (n = 651)",
                                              "Lecture (n = 542)",
                                              "Quiz and Test (n = 639)",
                                              "Other (n = 1091)"))

explored_factors[[2]] <- dplyr::select(explored_factors[[2]], 
                                       ip,
                                       `Full` = cluster3,
                                       `Moderately Full` = cluster1,
                                       `Rational` = cluster2,
                                       `Pleasurable` = cluster6,
                                       `Reluctant` = cluster4,
                                       `Universally Low` = cluster5)

out <- explored_factors[[2]] %>% 
    tidyr::gather(key, val, -ip) %>% 
    dplyr::group_by(key, ip) %>% 
    dplyr::filter(!is.na(ip)) %>% 
    dplyr::summarize(sum = sum(val))
out
# tab <- out %>% dplyr::select(key, ip, sum) %>% tidyr::spread(ip, sum) %>% as.data.frame()
# tab
# tab <- tab[, 2:6]
# tab
# tab <- as.table(as.matrix(tab))
# names(dimnames(tab)) = c("cluster", "ip")
# m.sat = loglm(~cluster + ip, tab)
# resid(m.sat)

out1 <- out %>% tidyr::spread(ip, sum)
out1
y <- chisq.test(out1[, 2:6])
y
y <- as.data.frame(y$stdres)
row.names(y) <- out1$key
y

the_vec <- as.vector(as.matrix(t(y)))

out$label <- ifelse(the_vec > 1.96, "+",
                    ifelse(the_vec < -1.96, "-", ""))

out

out <- out %>% dplyr::filter(ip != "Other (n = 1091)")

ggplot(out, aes(x = ip, y = sum, fill = key, label = label)) +
    geom_bar(stat = "identity", position = "stack") +
    geom_text(position = position_stack(.5)) +
    theme(legend.position = "right") +
    theme(legend.title = element_blank()) +
    theme(text=element_text(size = 16.5, family = "Times")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1.05)) +
    ylab("Number of Responses") +
    xlab(NULL) +
    theme(panel.background = element_blank()) +
    theme(panel.grid.major.y=element_line(color="black", size=.25)) +
    theme(panel.grid.minor.y=element_line(color="black", size=.15)) +
    theme(panel.grid.major.x=element_blank()) +
    theme(axis.ticks.y = element_blank()) +
    guides(fill = guide_legend(reverse = T))

ggsave("ip_12-7-16.png", width = 8.5, height = 6.5)

#############################################
### 6. Choice (All IP) ######################
#############################################

filter <- attributes(statistics)$cases_to_keep

esm_df$ch_defining_topic <- ifelse(esm_df$ch_defin == 1 | esm_df$ch_topic == 1, 1, 0)
esm_df$ch_doing_defining_topic <- ifelse(esm_df$ch_defin == 1 | esm_df$ch_topic == 1 | esm_df$ch_doing, 1, 0)

aov_df <- data.frame(who = esm_df$ch_who[filter], mat = esm_df$ch_mat[filter],
                     time = esm_df$ch_time[filter], howdo = esm_df$ch_howdo[filter],
                     doing = esm_df$ch_doing[filter], define = esm_df$ch_defin[filter],
                     topic = esm_df$ch_topic[filter], other = esm_df$ch_other[filter], 
                     defining_topic = esm_df$ch_defining_topic[filter], doing_defining_topic = esm_df$ch_doing_defining_topic[filter],
                     none = esm_df$ch_none[filter],
                     cluster = statistics[[4]], ip = factor_df$ip[filter],
                     control = esm_df$control[filter],
                     challenge = esm_df$chall[filter],
                     value = esm_df$imp_y[filter],
                     posaffect = esm_df$posaffect[filter])

df <-
    aov_df %>% 
    dplyr::select(who, mat, time, howdo, other, doing_defining_topic, none, cluster) 

df$cluster1 <- ifelse(df$cluster == 1, 1, 0)
df$cluster2 <- ifelse(df$cluster == 2, 1, 0)
df$cluster3 <- ifelse(df$cluster == 3, 1, 0)
df$cluster4 <- ifelse(df$cluster == 4, 1, 0)
df$cluster5 <- ifelse(df$cluster == 5, 1, 0)
df$cluster6 <- ifelse(df$cluster == 6, 1, 0)

# `Full` = cluster3,
# `Moderately Full` = cluster1,
# `Rational` = cluster2,
# `Pleasurable` = cluster6,
# `Reluctant` = cluster4,
# `Universally Low` = cluster5

model1 <- glm(cluster1 ~ who + mat + time + howdo + doing_defining_topic + other, family = binomial(link = "logit"), data = df)
arm::display(model1)
jmRtools::convert_log_odds(coef(model1))
summary(model1)

model2 <- glm(cluster2 ~ who + mat + time + howdo + doing_defining_topic + other, family = binomial(link = "logit"), data = df)
arm::display(model2)
jmRtools::convert_log_odds(coef(model2))
summary(model2)

model3 <- glm(cluster3 ~ who + mat + time + howdo + doing_defining_topic + other, family = binomial(link = "logit"), data = df)
arm::display(model3)
jmRtools::convert_log_odds(coef(model3))
summary(model3)

model4 <- glm(cluster4 ~ who + mat + time + howdo + doing_defining_topic + other, family = binomial(link = "logit"), data = df)
arm::display(model4)
jmRtools::convert_log_odds(coef(model4))
summary(model4)

model5 <- glm(cluster5 ~ who + mat + time + howdo + doing_defining_topic + other, family = binomial(link = "logit"), data = df)
arm::display(model5)
jmRtools::convert_log_odds(coef(model5))
summary(model5)

model6 <- glm(cluster6 ~ who + mat + time + howdo + doing_defining_topic + other, family = binomial(link = "logit"), data = df)
arm::display(model6)
jmRtools::convert_log_odds(coef(model6))
summary(model6)

#############################################
### 6. All vs. No Choice (All IP) ###########
#############################################

filter <- attributes(statistics)$cases_to_keep

esm_df$ch_defining_topic <- ifelse(esm_df$ch_defin == 1 | esm_df$ch_topic == 1, 1, 0)
esm_df$ch_doing_defining_topic <- ifelse(esm_df$ch_defin == 1 | esm_df$ch_topic == 1 | esm_df$ch_doing, 1, 0)

aov_df <- data.frame(who = esm_df$ch_who[filter], mat = esm_df$ch_mat[filter],
                     time = esm_df$ch_time[filter], howdo = esm_df$ch_howdo[filter],
                     doing = esm_df$ch_doing[filter], define = esm_df$ch_defin[filter],
                     topic = esm_df$ch_topic[filter], other = esm_df$ch_other[filter], 
                     defining_topic = esm_df$ch_defining_topic[filter], doing_defining_topic = esm_df$ch_doing_defining_topic[filter],
                     none = esm_df$ch_none[filter],
                     cluster = statistics[[4]], ip = factor_df$ip[filter],
                     control = esm_df$control[filter],
                     challenge = esm_df$chall[filter],
                     value = esm_df$imp_y[filter],
                     posaffect = esm_df$posaffect[filter])

plot <-
    aov_df %>% 
    dplyr::select(who, mat, time, howdo, other, doing_defining_topic, none, cluster) %>% 
    dplyr::mutate(any = ifelse(who == 1 | mat == 1 | howdo == 1 | other == 1 | doing_defining_topic == 1, 1, 0)) %>% 
    group_by(cluster) %>% 
    summarize(any = sum(any),
              none = sum(none))
    
plot

plot$cluster <- factor(plot$cluster)
levels(plot$cluster) <- c("Moderately Full", "Rational", "Full", "Reluctant", "Universally Low", "Pleasurable")
plot$cluster <- factor(plot$cluster, levels = c("Full", "Moderately Full", "Rational", "Pleasurable", "Reluctant", "Universally Low"))

to_plot <- plot %>% tidyr::gather(key, val, -cluster)
to_plot$key <- factor(to_plot$key)
to_plot$key <- forcats::fct_recode(to_plot$key,
                               c("Any Choice" = "any",
                               "No Choice" = "none"
                               ))

to_plot <- dplyr::arrange(to_plot, cluster)
to_plot
plot_o <- to_plot %>% tidyr::spread(key, val)
plot_o
y <- chisq.test(plot_o[, 2:3])
y
y <- as.data.frame(y$stdres)
clipr::write_clip(y)
row.names(y) <- plot_o$cluster

the_vec <- as.vector(as.matrix(t(y)))

to_plot$label <- ifelse(the_vec > 1.96, "+",
                        ifelse(the_vec < -1.96, "-", ""))

to_plot %>% group_by(cluster) %>% summarize(sum = sum(val))

ggplot(to_plot, aes(x = key, y = val, fill = cluster, label = label)) +
    geom_bar(stat = "identity") +
    geom_text(position = position_stack(.5)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1.05)) +
    theme(text=element_text(size = 16, family = "Times")) +
    theme(legend.position = "right") +
    scale_fill_discrete("") +
    ylab("Number of Responses") +
    xlab(NULL) +
    theme(panel.background = element_blank()) +
    theme(panel.grid.major.y=element_line(color="black", size=.25)) +
    theme(panel.grid.minor.y=element_line(color="black", size=.15)) +
    theme(panel.grid.major.x=element_blank()) +
    theme(axis.ticks.y = element_blank()) +    guides(fill = guide_legend(reverse = T))

ggsave("choices_12-7-16.png", height = 5, width = 6)


#############################################
### 6.5 Choice (any vs. none) ###############
#############################################

# for cowplot

filter <- attributes(statistics)$cases_to_keep

esm_df$ch_defining_topic <- ifelse(esm_df$ch_defin == 1 | esm_df$ch_topic == 1, 1, 0)
esm_df$ch_doing_defining_topic <- ifelse(esm_df$ch_defin == 1 | esm_df$ch_topic == 1 | esm_df$ch_doing, 1, 0)

aov_df <- data.frame(who = esm_df$ch_who[filter], mat = esm_df$ch_mat[filter],
                     time = esm_df$ch_time[filter], howdo = esm_df$ch_howdo[filter],
                     doing = esm_df$ch_doing[filter], define = esm_df$ch_defin[filter],
                     topic = esm_df$ch_topic[filter], other = esm_df$ch_other[filter], 
                     framing = esm_df$ch_doing_defining_topic[filter],
                     none = esm_df$ch_none[filter],
                     cluster = statistics[[4]], ip = factor_df$ip[filter],
                     subject = esm_df$subject[filter])

aov_df$ip <- as.character(aov_df$ip)

table(aov_df$ip)

aov_df$ip <- ifelse((aov_df$ip == "Discussion" | aov_df$ip == "Non-instructional" |
                         aov_df$ip == "Presentation" | aov_df$ip == "Video" | aov_df$ip == "Group Work"), "Other", aov_df$ip)


# aov_df <- filter(aov_df, ip == "Individual Work")

str(aov_df)

aov_df$subject[aov_df$subject == 1] <- "Integrated Science"
aov_df$subject[aov_df$subject == 2] <- "Biology"
aov_df$subject[aov_df$subject == 3] <- "Chemistry"
aov_df$subject[aov_df$subject == 4] <- "Physics"

# aov_df <- filter(aov_df, subject %in% c("Integrated Science", "Biology"))

aov_df$subject <- as.vector(aov_df$subject)

# aov_df$subject <- factor(explored_factors[[2]]$subject, 
#                                         levels = c("Integrated Science", "Biology", "Chemistry", "Physics"))

aov_df$any_choice <- ifelse(aov_df$framing == 1 | aov_df$mat == 1 | aov_df$howdo == 1 | aov_df$time == 1 |
                                aov_df$who == 1 | aov_df$other == 1, 1, 0)

str(aov_df)

plot <-
    aov_df %>% 
    dplyr::select(any_choice, none, cluster, ip) %>% 
    group_by(cluster, ip) %>% 
    summarize(Any = sum(any_choice),
              None = sum(none)) %>% 
    arrange(ip, cluster)

plot %>% group_by(ip) %>% summarize(sum = sum(Any + None))

plot <- plot[!is.na(plot$ip), ]

plot

y <- chisq.test(plot[1:6, 3:4])
y <- as.data.frame(y$stdres)
y
the_vec1 <- as.vector(as.matrix(t(y)))

y <- chisq.test(plot[7:12, 3:4])
y <- as.data.frame(y$stdres)
y
the_vec2 <- as.vector(as.matrix(t(y)))

y <- chisq.test(plot[13:18, 3:4])
y
y <- as.data.frame(y$stdres)
y$observed
y
the_vec3 <- as.vector(as.matrix(t(y)))

y <- chisq.test(plot[19:24, 3:4])
y <- as.data.frame(y$stdres)
the_vec4 <- as.vector(as.matrix(t(y)))

y <- chisq.test(plot[25:30, 3:4])
y <- as.data.frame(y$stdres)
the_vec5 <- as.vector(as.matrix(t(y)))

the_vec <- c(the_vec1, the_vec2, the_vec3, the_vec4, the_vec5)

plot$cluster <- factor(plot$cluster)
levels(plot$cluster) <- c("Moderately Full", "Rational", "Full", "Reluctant", "Universally Low", "Pleasurable")

to_plot <- 
    plot %>% 
    select(cluster:None) %>% 
    tidyr::gather(key, val, -cluster, -ip) %>% 
    arrange(ip, cluster)

to_plot

to_plot %>% group_by(ip) %>% summarize(sum = sum(val))

to_plot

to_plot$cluster <- factor(to_plot$cluster, levels = c("Full", "Moderately Full", "Rational", "Pleasurable", "Reluctant", "Universally Low"))

View(plot)

p1 <- plot[1:6, 3:4]
p2 <- plot[7:12, 3:4]
p3 <- plot[13:18, 3:4]
p4 <- plot[19:24, 3:4]
p5 <- plot[25:30, 3:4]

the_list <- list(p1, p2, p3, p4, p5)
tothe_list 
toString(the_list)

arr <- abind::abind(the_list, along=3)

arr



str(arr)

tab <- as.table(arr)
tab
names(dimnames(tab)) = c("cluster", "choice", "ip")
dimnames(tab)[[1]] = c("Moderately Full", "Rational", "Full", "Reluctant", "Universally Low", "Pleasurable")
dimnames(tab)[[2]] = c("any", "none")
dimnames(tab)[[3]] = c("Individual Work", "Laboratory", "Lecture", "Other", "Quiz and Test")
m.sat = loglm(~cluster + choice + ip, tab)
m.sat
x <- as.data.frame(resid(m.sat))
clipr::write_clip(tidyr::spread(x, cluster, Freq) %>% arrange(ip))


to_plot

to_plot$label <- ifelse(the_vec > 1.96, "+",
                        ifelse(the_vec < -1.96, "-", ""))

to_plot %>% group_by(ip) %>% summarize(sum = sum(val))

to_plot$key <- factor(to_plot$key, c("Any", "None"))

to_plot %>% group_by(ip, key) %>% summarize(sum = sum(val))

to_plot %>% group_by(key) %>% summarize(sum = sum(val))

to_plot %>% group_by(cluster, key) %>% summarize(sum = sum(val))

to_plot$key <- forcats::fct_recode(to_plot$key,
                                   "Any Choice" = "Any",
                                   "No Choice" = "None")

to_plot <- dplyr::arrange(to_plot, desc(ip), key)

View(to_plot)

ggplot(to_plot, aes(x = key, y = val, fill = cluster, label = label)) +
    geom_bar(stat = "identity") +
    facet_grid( ~ ip) +
    geom_text(position = position_stack(.5)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1.05)) +
    theme(text=element_text(size = 18, family = "Times")) +
    theme(legend.position = "right") +
    scale_fill_discrete("") +
    ylab("Number of Responses") +
    xlab(NULL) +
    theme(panel.background = element_blank()) +
    theme(panel.grid.major.y=element_line(color="black", size=.25)) +
    theme(panel.grid.minor.y=element_line(color="black", size=.15)) +
    theme(panel.grid.major.x=element_blank()) +
    theme(axis.ticks.y = element_blank()) +
    theme(axis.ticks.x = element_blank()) +
    guides(fill = guide_legend(reverse = T))

ggsave("choice_ip.png", width = 10, height = 6)
