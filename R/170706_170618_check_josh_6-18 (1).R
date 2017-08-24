
#install.packages("forcats")
library("forcats")
library("prcr")
library("dplyr")
#library("micEcon")

X <- forcats::gss_cat
X$rincome <- as.numeric(stringr::str_extract(X$rincome, "\\(?[0-9,.]+\\)?"))
X <- dplyr::select(X, rincome, age, tvhours)
#excluded<-X[c(complete.cases(X)==F),]
X<-X[complete.cases(X),]
X$age <- as.numeric(X$age)
X$tvhours <- as.numeric(X$tvhours)
JR_OUT<-outlierHadi(as.matrix(X[,2:3]))

### Josh comments

JR_OUT$Outliers # these should be the row indices for the outliers

X_no_mv_outliers <- X[-JR_OUT$Outliers, ] # this is the data frame without them
X_no_mv_outliers # just printing to check how it looks

# is the difference in the number of observations equal to the number of outliers?
nrow(X) - nrow(X_no_mv_outliers) # 67
length(JR_OUT$Outliers) # 67 - looks good

the_mv_outliers <- X[JR_OUT$Outliers, ] # we can also look at a data frame just with the outliers, to inspect them
the_mv_outliers # also has 67 rows


### RRS comments
# so what is mah.out
#length(JR_OUT$mah.out)

#test<-JR_OUT$mah.out %in% JR_OUT$Outliers 
# are the mah.out in the longer list of Outliers? 
###YES
#JR_OUT$mah.out #print
#JR_OUT$Outliers #print
#so mah.out is a subset of the outliers that were identified by the mahalanobis distance.

#also may want to make a note in the function description that Cb refers to the means of variables including outliers.
#Likewise with Sb variance

#finally, we should try to figure out why line 37 in Outlier Hadi function code needed to be changed from the original
# (see bottom of page on http://www1.aucegypt.edu/faculty/hadi/research.html)
#Is it because the original code was for S?
