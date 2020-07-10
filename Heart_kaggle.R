
# content -----------------------------------------------------------------

# This database contains 76 attributes, but all published experiments refer to using a subset of 14 of them. In particular, the Cleveland database is the only one that has been used by ML researchers to
# 
# # 
# # this date. The "goal" field refers to the presence of heart disease in the patient. It is integer valued from 0 (no presence) to 4.
# 
# Content
# 
# Attribute Information:
#   
#   age
# sex
# chest pain type (4 values)
# resting blood pressure
# serum cholestoral in mg/dl
# fasting blood sugar > 120 mg/dl
# resting electrocardiographic results (values 0,1,2)
# maximum heart rate achieved
# exercise induced angina
# oldpeak = ST depression induced by exercise relative to rest
# the slope of the peak exercise ST segment
# number of major vessels (0-3) colored by flourosopy
# thal: 3 = normal; 6 = fixed defect; 7 = reversable defect
#loading the DataSet

library(readr)
library(caTools)
library(CatEncoders)
library(ggplot2)
library(dplyr)
install.packages('mice',dependencies = TRUE)
install.packages('dlookr',dependencies = TRUE)
library(mice)
library(dlookr)
heart <- read.csv("D:/DATA SC WITH R/heart.csv", header = T)
head(heart)
View(heart)
str(heart)
summary(heart)
# ï..age           sex               cp           trestbps          chol      
# Min.   :29.00   Min.   :0.0000   Min.   :0.000   Min.   : 94.0   Min.   :126.0  
# 1st Qu.:47.50   1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:120.0   1st Qu.:211.0  
# Median :55.00   Median :1.0000   Median :1.000   Median :130.0   Median :240.0  
# Mean   :54.37   Mean   :0.6832   Mean   :0.967   Mean   :131.6   Mean   :246.3  
# 3rd Qu.:61.00   3rd Qu.:1.0000   3rd Qu.:2.000   3rd Qu.:140.0   3rd Qu.:274.5  
# Max.   :77.00   Max.   :1.0000   Max.   :3.000   Max.   :200.0   Max.   :564.0  
# fbs            restecg          thalach          exang           oldpeak    
# Min.   :0.0000   Min.   :0.0000   Min.   : 71.0   Min.   :0.0000   Min.   :0.00  
# 1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:133.5   1st Qu.:0.0000   1st Qu.:0.00  
# Median :0.0000   Median :1.0000   Median :153.0   Median :0.0000   Median :0.80  
# Mean   :0.1485   Mean   :0.5281   Mean   :149.6   Mean   :0.3267   Mean   :1.04  
# 3rd Qu.:0.0000   3rd Qu.:1.0000   3rd Qu.:166.0   3rd Qu.:1.0000   3rd Qu.:1.60  
# Max.   :1.0000   Max.   :2.0000   Max.   :202.0   Max.   :1.0000   Max.   :6.20  
# slope             ca              thal           target      
# Min.   :0.000   Min.   :0.0000   Min.   :0.000   Min.   :0.0000  
# 1st Qu.:1.000   1st Qu.:0.0000   1st Qu.:2.000   1st Qu.:0.0000  
# Median :1.000   Median :0.0000   Median :2.000   Median :1.0000  
# Mean   :1.399   Mean   :0.7294   Mean   :2.314   Mean   :0.5446  
# 3rd Qu.:2.000   3rd Qu.:1.0000   3rd Qu.:3.000   3rd Qu.:1.0000  
# Max.   :2.000   Max.   :4.0000   Max.   :3.000   Max.   :1.0000  

#EDA
colnames(heart)
#Checking For Null values
sum(is.na(heart))
colSums(is.na(heart))
#There are no null values
#Checking For OUtliers
heart$ï..age.
bp=boxplot(heart$ï..age,heart$trestbps,heart$chol,heart$thalach)
bp$stats
#trestbps,chol,thalatch have outiers
# [,1] [,2]  [,3]  [,4]
# [1,] 29.0   94 126.0  88.0
# [2,] 47.5  120 211.0 133.5
# [3,] 55.0  130 240.0 153.0
# [4,] 61.0  140 274.5 166.0
# [5,] 77.0  170 360.0 202.0
# attr(,"class")
#Checking The Skewness Data Is not skewed
library(e1071)   
heart$trestbps=as.numeric(heart$trestbps)
heart$chol=as.numeric(heart$chol)
heart$thalach=as.numeric(heart$thalach)
skewness((heart$trestbps))
#0.706717
skewness(heart$chol)
#1.132105
skewness(heart$thalach)
#-0.5321005
skewness(heart$ï..age)
#-0.2004632
#dealing with outliers
#trestbps
heart$trestbps<-imputate_outlier(heart,trestbps, method = 'capping')
boxplot(heart$trestbps)
#chol
heart$chol<-imputate_outlier(heart,chol, method = 'capping')
boxplot(heart$chol)
#thalach
heart$thalach<-imputate_outlier(heart,thalach, method = 'capping')
boxplot(heart$thalach)
##########################################################
head(heart)
#Relation Of variables with taret variable
#Sex
sex_1<-heart %>% group_by(sex) %>% summarise(target=mean(target))
sex_1
# sex target
# <int>  <dbl>
#   1     0  0.75 
# 2     1  0.449
#cp
cp_1<-heart %>% group_by(cp) %>% summarise(target=mean(target))
cp_1
# cp target
# <int>  <dbl>
#   1     0  0.273
# 2     1  0.82 
# 3     2  0.793
# 4     3  0.696
fbs_1<-heart %>% group_by(fbs) %>% summarise(target=mean(target))
fbs_1
# fbs target
# <int>  <dbl>
#   1     0  0.550
# 2     1  0.511
# we can drop fbs column from our model
exang_1<-heart %>% group_by(exang) %>% summarise(target=mean(target))
exang_1
# > exang_1
# # A tibble: 2 x 2
# # exang target
# # <int>  <dbl>
# #   1     0  0.696
# # 2     1  0.232
slope1<-heart %>% group_by(slope) %>% summarise(target=mean(target))
slope1
slope1
# A tibble: 3 x 2
# slope target
# <int>  <dbl>
#   1     0  0.429
# 2     1  0.35 
# 3     2  0.754
ca_1<-heart%>%group_by(ca) %>%summarise(target=mean(target))
# A tibble: 5 x 2
# ca target
# <int>  <dbl>
#   1     0  0.743
# 2     1  0.323
# 3     2  0.184
# 4     3  0.15 
# 5     4  0.8  
thal_1<-heart %>% group_by(thal) %>% summarise(target=mean(target))
thal_1
# # A tibble: 4 x 2
# thal target
# <int>  <dbl>
#   1     0  0.5  
# 2     1  0.333
# 3     2  0.783
# 4     3  0.239
head(heart)
###########building A model#############################
heart_2<-select(heart,-6)
heart_2
#train & Test
set.seed(9) # this wil give the same sample everytime
split = sample.split(Y = heart_2$target, SplitRatio = 0.8)
sum(split)
sum(!split)
train=heart_2[split,]
test=heart_2[!split,]

########Model#################

model <- glm(target ~ ., # glm - Generalised Linear Models; Dependent ~ Independent
             family = binomial(link = 'logit'), # Assumption of Logistic Regression
             data = train)
#######Evaluate#################
result <- predict(model, test)
result[1:5]
result <- predict(model, test, type = 'response') # predicting prob
result[1:10]
result <- ifelse(result > 0.5, 1, 0) # Threshold is 0.5
result[1:10]
accuracy = mean(result == test$target)
accuracy
#[1] 0.852459
###############################
library(ROCR)

# pred <- prediction(model$fitted.values[1:5], train$Survived[1:5])
# pred

pred <- prediction(model$fitted.values, train$target)

head(train)

perf <- performance(pred,"tpr","fpr") # tpr - Sensitivity; fpr = 1 - TNR = Specificity
perf

plot(perf, avg = "threshold", colorize=T, print.cutoffs.at = seq(0,1,.1), text.adj=c(-.2, 1.7),lwd=3, main="ROC Curve")

abline(0,1,lty = 10)

AUCLog2 <- performance(pred, measure = "auc")@y.values[[1]] # subsetting the list
cat("AUC: ", AUCLog2)
#0.9337466

