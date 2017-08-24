setwd("~/Box Sync/CURRENT FILES/Projects/170510 Engagement Profiles/Data_Analysis/")
load(file="170518_Eng_prof.Rdata")
#load(file="170209.RData")
library("prcr")
library("plyr")
library("dplyr")
#outliers
#ENG_OUT<-outlierHadi(as.matrix(ENG_INPUT_NO_CEG_VAL))
#ENG_OUT$Outliers
#ENG_OUT$mah.out
#therefore no outliers
#how many clusters should we create?
#n_clust_noVAL_cent<-plot_r_squared(df=ENG_INPUT_NO_CEG_VAL,DBEG_prof, DPE_prof, DCEG_prof, to_center = TRUE, to_scale=T, lower_bound = 3, upper_bound = 7, r_squared_table = TRUE)
#plot_NO_VAL<-plot_r_squared(df=ENG_INPUT_NO_CEG_VAL,DBEG_prof, DPE_prof, DCEG_prof, to_center = TRUE, lower_bound = 3, upper_bound = 7, r_squared_table = F)

#create clusters
five_cluster<-create_profiles(df=ENG_INPUT_NO_CEG_VAL,DBEG_prof, DPE_prof, DCEG_prof, to_center=TRUE, to_scale=T, n_profiles=5)
six_cluster<-create_profiles(df=ENG_INPUT_NO_CEG_VAL,DBEG_prof, DPE_prof, DCEG_prof, to_center=TRUE, to_scale=T, n_profiles=6)
seven_cluster<-create_profiles(df=ENG_INPUT_NO_CEG_VAL,DBEG_prof, DPE_prof, DCEG_prof, to_center=TRUE, to_scale=T, n_profiles=7)

RAW_6_data<-six_cluster$.data
RAW_6_data$cluster.f<-factor(RAW_6_data$cluster)

RAW_5_data<-five_cluster$.data
RAW_5_data$cluster.f<-factor(RAW_5_data$cluster)

RAW_7_data<-seven_cluster$.data
RAW_7_data$cluster.f<-factor(RAW_7_data$cluster)


#R2 for five profiles
DBEG_raw_5_anova<-lm(formula = DBEG_prof ~ cluster.f, data =RAW_5_data)
DCEG_raw_5_anova<-lm(formula = DCEG_prof ~ cluster.f, data =RAW_5_data)
DPE_raw_5_anova<-lm(formula = DPE_prof ~ cluster.f, data =RAW_5_data)
DBEG_raw_6_anova<-lm(formula = DBEG_prof ~ cluster.f, data = RAW_6_data)
DCEG_raw_6_anova<-lm(formula = DCEG_prof ~ cluster.f, data = RAW_6_data)
DPE_raw_6_anova<-lm(formula = DPE_prof ~ cluster.f, data = RAW_6_data)
DBEG_raw_7_anova<-lm(formula = DBEG_prof ~ cluster.f, data = RAW_7_data)
DCEG_raw_7_anova<-lm(formula = DCEG_prof ~ cluster.f, data = RAW_7_data)
DPE_raw_7_anova<-lm(formula = DPE_prof ~ cluster.f, data = RAW_7_data)

DBEG_5_R<-summary(DBEG_raw_5_anova)$r.squared
DCEG_5_R<-summary(DBEG_raw_5_anova)$r.squared
DPE_5_R<-summary(DPE_raw_5_anova)$r.squared
DPE_6_R<-summary(DPE_raw_6_anova)$r.squared
DCEG_6_R<-summary(DBEG_raw_6_anova)$r.squared
DBEG_6_R<-summary(DBEG_raw_6_anova)$r.squared
DBEG_7_R<-summary(DBEG_raw_7_anova)$r.squared
DCEG_7_R<-summary(DBEG_raw_7_anova)$r.squared
DPE_7_R<-summary(DPE_raw_7_anova)$r.squared
R_sq<-matrix(c(DBEG_5_R, DBEG_6_R, DBEG_7_R,
        DCEG_5_R, DCEG_6_R, DCEG_7_R,
        DPE_5_R, DPE_6_R, DPE_7_R), nrow=3, ncol = 3)

#not working 
RAW_5_data$mv_response <- cbind(RAW_5_data$DBEG_prof, RAW_5_data$DCEG_prof, RAW_5_data$DPE_prof)
m <- manova(mv_response ~ cluster.f, RAW_5_data) # for use later on
summary(m)
etasq(m)
#ALL3<-merge(ALL2, clustered_data, by=c("SID", "ERID", "TID"), all.y = T)
#ALL3[is.na(ALL3)] <- "."

#write.csv(ALL3, "170705_clustered_merged_data.csv")
