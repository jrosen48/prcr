# command_and_control.R

###############################
### 1. SETTING UP #############
###############################
source("primary_functions.R")
#library(foreign)
library(dplyr)

set.seed("03112016")
remove(list = ls())
setwd("~/dropbox/research/SciMo/prcr/R")
data <- tbl_df(foreign::read.spss("~/documents/myscimo/SciMo ESM.12.15.2010.sav", to.data.frame = T))
# data <- haven::read_spss("~/documents/myscimo/SciMo ESM.12.15.2010.sav")

# Removing the errant case

data$stud_ID <- stringr::str_trim(as.character(data$stud_ID))
to_remove <- data$stud_ID == "AXR050594" & data$month == 10 & data$day == 20 & data$year == 8 & data$signal == "2-2nd signal" # removing 10/20/8, ID = "AXR050594", second beeper
nrow(data)
data <- data[!to_remove, ]
nrow(data) # - 1 case

# Recoding instructional practices

data$instructional_practice_re <- car::recode(data$instructional_practice, # are these correct?
                                              "c('1 Lecture') = 'Lecture';
                                              c('2 Seatwk-Ind', '3 Review-Ind') = 'Individual Work';
                                              c('4 Seatwk-Grp', '5 Review-Grp') = 'Group Work';
                                              c('6 QuizPrep', '8 TestTaking', '7 QuizRev') = 'Quiz and Test';
                                              c('9 Discussion') = 'Discussion';
                                              c('11 PresElab', '10 Presentation') = 'Presentation';
                                              c('12 Video-Watch', '13 Video-Elab') = 'Video';
                                              c('15 Labwork', '14 Lab-Prep', '16 Lab-Review') = 'Laboratory';
                                              c('17 NonInstTime') = 'Non-instructional';
                                              c('18 OffTask') = 'Off-task'")

###############################
### 2. CREATING SCALES ########
###############################

cognitive <- data.frame(cbind(data$imp_y, data$imp_fut), stringsAsFactors = F)
affective <- data.frame(cbind(data$enjoy, data$interest), stringsAsFactors = F)
behavioral <- data.frame(cbind(data$conc, data$hardwk), stringsAsFactors = F)

data <- data.frame(data,
                   behavioral_scale = (behavioral$X1 + behavioral$X2) / 2,
                   cognitive_scale = (cognitive$X1 + cognitive$X2) / 2,
                   affective_scale = (affective$X1 + affective$X2) / 2, stringsAsFactors = F) # change these to mean... and find a way to keep just one if there's one of two

data_ss <-
    data %>%
    mutate(behavioral_scale = (behavioral$X1 + behavioral$X2) / 2,
           cognitive_scale = (cognitive$X1 + cognitive$X2) / 2,
           affective_scale = (affective$X1 + affective$X2) / 2) %>%
    select(behavioral_scale,
           cognitive_scale,
           affective_scale,
           stud_ID,
           teacher_ID,
           subject,
           gender,
           race,
           instructional_practice_re,
           age)

raw_data_matrix <- data_ss[, 1:3]
grouping_vector <- data_ss[, 4]
factor_data_frame <- dplyr::select(data_ss, stud_ID:age)

str(factor_data_frame)

###############################
### 3. COMMAND AND CONTROL ####
###############################

source("primary_functions.R")

prepared_data <- prepare_data(raw_data = raw_data_matrix,
                              method_of_centering = "grand", # can be group (Mg = 0), grand (M = 0), or raw
                              grouping_vector = grouping_vector, # take a grouping vector of the same length as the # of rows in raw_data_matrix
                              to_standardize = T # standardizes variables to have SD = 1
                              )

output <- cluster_data(prepared_data = prepared_data, # from the prepare_data function
                       n_clusters = 8,
                       distance_metric = "squared_euclidean", # can be euclidean, squared euclidean, manhattan, minkowski, and others (see ?dist())
                       linkage = "complete" # can be single, complete, average, centroid, and others (see ?hclust())
                       )

statistics <- calculate_stats(clustering_output = output,
                              names_of_variables = c("Behavioral Engagement", "Cognitive Engagement", "Affective Engagement")
                              )

cross_validation <- cross_validate()

explored_factors <- explore_factors(cluster_assignments = statistics[[5]],
                                    cases_to_keep = attributes(statistics)$cases_to_keep,
                                    factor_data_frame = factor_data_frame,
                                    factor_to_explore = "race",
                                    variable_to_find_proportion = "stud_ID", # can be thought of as unit of analysis - if NULL, factor to explore will be used
                                    cluster_names = NULL
                                    )

# Need to work on this guy a bit more

# comparison_of_statistics <- compare_cluster_statistics(args = attributes(output)$args_attr,
#                                                        vars_to_vary = "n_clusters"
#                                                        )

# Need to write this and abstract the part with the tables from Breckenridge
# look at double split cross validation randomly and by spring / fall
