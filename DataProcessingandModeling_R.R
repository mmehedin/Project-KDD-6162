#July 31, 2018 KDD Class UNC Charlotte
#Team Members:
# Mihai Mehedint 
# Shurya badam
# Surya pavan malireddy
# Karhikeya vayuputra chittuluri

#load libraries
if(!require(missForest)) install.packages("missForest",repos = "http://cran.us.r-project.org")
if(!require(VIM)) install.packages("VIM",repos = "http://cran.us.r-project.org")
if(!require(mice)) install.packages("mice",repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot",repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales",repos = "http://cran.us.r-project.org")
if(!require(tidyr)) install.packages("tidyr",repos = "http://cran.us.r-project.org")
library(missForest)
library(VIM)
library(ggplot2)
library(plyr)
library(e1071)
library(mice)
library(corrplot)
library(tidyr)
library(scales)

#load data files
setwd("~/Documents/UNCC/ITCS6162_KDD/DataProcessingandModeling")
#read all CSVs
filenames <- list.files(path = "./all/", full.names=TRUE)
#all.csv <- lapply(filenames,function(i){
 # read.csv(i, header=FALSE, skip=4)
#})
#loan.data <- read.csv(file="application_train.csv", header=TRUE, sep=",")
loan.data.all <- read.csv(file="./all/application_train_all.csv", header=TRUE, sep=",")
ccard <- read.csv(file="all/credit_card_balance.csv", header = TRUE, sep=",")

previous.application <- read.csv(file="all/previous_application.csv", header = TRUE, sep=",")
POS.CASH.balance <- read.csv(file="all/POS_CASH_balance.csv", header = TRUE, sep=",")
installments.payments <- read.csv(file="all/installments_payments.csv", header = TRUE, sep=",")
bureau <- read.csv(file="all/bureau.csv", header = TRUE, sep=",")
bureau.balance <- read.csv(file="all/bureau_balance.csv", header = TRUE, sep=",")
#attach(loan.data)
attach(loan.data.all)
attach(ccard)
source("outlierscript.R")
total1 <- merge(loan.data.all, ccard, by=c("SK_ID_CURR"))
write.csv(total1, 'all/total1.csv', row.names=FALSE)
total1 <- read.csv(file="all/total1.csv", header = TRUE, sep=",")

prev1 <- merge(previous.application, POS.CASH.balance, by=c("SK_ID_PREV"))
colnames(prev1)[2] <- 'SK_ID_CURR'
write.csv(prev1, 'all/prev1.csv', row.names=FALSE)
prev1 <- na.omit(prev1)


prev1 <- read.csv(file="all/prev1.csv", header = TRUE, sep=",")
prev2 <- merge(prev1, installments.payments, by=c("SK_ID_PREV"))
prev2 <- prev2[, !(names(prev2)=='SK_ID_CURR.y')]
prev2 <- prev2[, !(names(prev2)=='NAME_CONTRACT_STATUS.y')]
colnames(prev2)[2] <- 'SK_ID_CURR'
colnames(prev2)[17] <- 'NAME_CONTRACT_STATUS'
write.csv(prev2, 'all/prev2.csv', row.names=FALSE)
prev2 <- read.csv(file="all/prev2.csv", header = TRUE, sep=",")
#prev3 <- merge(prev2, ccard, by=c("SK_ID_PREV"))
#write.csv(prev3, 'all/prev3.csv', row.names=FALSE)

total1 <- read.csv(file="all/total1.csv", header = TRUE, sep=",")
total2 <- merge(prev2, total1, by=c("SK_ID_CURR"))
total2 <- total2[, !(names(total2)=='NAME_CONTRACT_STATUS.y')]
colnames(total2)[17] <- 'NAME_CONTRACT_STATUS'
total2 <- total2[, !(names(total2)=='NAME_CONTRACT_TYPE.y')]
colnames(total2)[3] <- 'NAME_CONTRACT_TYPE'
write.csv(total2, 'all/total2.csv', row.names=FALSE)

bureau.merged <- merge(bureau.balance, bureau, by=c("SK_ID_BUREAU")) 
write.csv(bureau.merged, 'all/bureau.merged.csv', row.names=FALSE)

total2 <- read.csv(file="all/total2.csv", header = TRUE, sep=",")
total2<-na.omit(total2)
head(table(total2$SK_ID_CURR),10)
bureau.merged <- read.csv(file="all/bureau.merged.csv", header = TRUE, sep=",")
bureau.merged <- na.omit(bureau.merged)
total3 <- merge(total2, bureau.merged, by=c("SK_ID_CURR"))
write.csv(total3, 'all/total3.csv', row.names=FALSE)

total3 <- read.csv(file="all/total3.csv", header = TRUE, sep=",")

#use summary to get information on attributes
summary(loan.data)
names(loan.data)
#Missing Values - analyze missing values in the data
sapply(loan.data, function(x) sum(is.na(x)))

missing<- loan.data[!complete.cases(loan.data),]; missing
complete.cases(loan.data) != is.na(loan.data)

sum(table(which(loan.data == ''))) #find how many missing values are there

sum(sapply(loan.data, function(x) sum(is.na(x))))

#using mice:
library(missForest)
library(VIM)
mice_plot <- aggr(loan.data, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(loan.data), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

#Box plot

library(plyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("e1071")
library(e1071)
#install.packages("mice")
library(mice)
amt_income <- loan.data$AMT_INCOME_TOTAL
amt_credit <- loan.data$AMT_CREDIT
hist(amt_income) #histogram
hist(amt_credit)
boxplot(loan.data$AMT_INCOME_TOTAL,data=loan.data, main="Income Data", 
        xlab="Income", ylab="Income")
boxplot(loan.data$AMT_CREDIT,data=loan.data, main="Credit Data", 
        xlab="Credit", ylab="Credit")
#display amt credit
bp <- ggplot(loan.data, aes(x=AMT_CREDIT, y=AMT_INCOME_TOTAL, group=loan.data$CNT_CHILDREN)) + 
  geom_boxplot(fill="red", alpha=0.2) + 
  xlab("AMT CREDIT")+ylab("AMT INCOME")
bp <- bp + scale_x_continuous(trans='log2') +
  scale_y_continuous(trans='log2')
bp


ggplot(loan.data, aes(x = as.factor(loan.data$CNT_CHILDREN), fill = CNT_CHILDREN==1, color=loan.data$AMT_INCOME_TOTAL)) +
  geom_density(alpha = .3)

ggplot(loan.data[loan.data$AMT_INCOME_TOTAL<100000,], aes(x = AMT_INCOME_TOTAL/10000, fill = as.factor(CNT_CHILDREN))) +
  geom_density(alpha = .3)

ggplot(loan.data, aes(x = as.factor(loan.data$CNT_CHILDREN), fill = AMT_CREDIT)) +
  geom_density(alpha = .3)

ggplot(loan.data, aes(x = as.factor(loan.data$CNT_CHILDREN), fill = AMT_CREDIT)) +
  geom_density(alpha = .3)

#display amt income
#using CNT Children since it has discrete integer values to deal with continuous values and separate the groups
bp <- ggplot(loan.data, aes(x="", y=AMT_CREDIT, group=loan.data$CNT_CHILDREN)) + 
  geom_boxplot(fill="blue", alpha=0.2) + 
  xlab("CNT CHILDREN")+ylab("AMT CREDIT")
bp <- bp + scale_y_continuous(trans='log2')
bp


par(mar=c(1,1,1,1))
outlierKD(loan.data, AMT_INCOME_TOTAL) 


outlierKD(loan.data, AMT_CREDIT)


#flag variables
#loan.data["NAME_FAMILY_STATUS"] <- 0
loan.data$NAME_FAMILY_STATUS <- loan.data$NAME_FAMILY_STATUS > 0
loan.data$NAME_FAMILY_STATUS

loan.data$flag_s_not_m <- 0
loan.data$flag_m <- 0
loan.data$flag_c_m <- 0
loan.data$flag_w <- 0
loan.data$flag_s <- 0

#flag family status
loan.data$flag_s_not_m[loan.data$NAME_FAMILY_STATUS == 'Single / not married'] <- 1
#check field
loan.data$flag_s_not_m
#continue for all other fields
loan.data$flag_m[loan.data$NAME_FAMILY_STATUS == 'Married'] <- 1
#check field
loan.data$flag_m
loan.data$flag_c_m[loan.data$NAME_FAMILY_STATUS == 'Civil Marriage'] <- 1
#check field
loan.data$flag_c_m
loan.data$flag_w[loan.data$NAME_FAMILY_STATUS == 'Widow'] <- 1
#check field
loan.data$flag_w
loan.data$flag_s[loan.data$NAME_FAMILY_STATUS == 'Separated'] <- 1
#check field
loan.data$flag_s

count(loan.data, 'flag_s_not_m')
count(loan.data, 'flag_m')
count(loan.data, 'flag_c_m')
count(loan.data, 'flag_w')
count(loan.data, 'flag_s')

#Z-Score Standardization.
loan.data$zscore.income <- (loan.data$AMT_INCOME_TOTAL - mean(loan.data$AMT_INCOME_TOTAL))/sd(loan.data$AMT_INCOME_TOTAL)
loan.data$zscore.income
loan.data$zscore.credit <- (loan.data$AMT_CREDIT - mean(loan.data$AMT_CREDIT))/sd(loan.data$AMT_CREDIT)
loan.data$zscore.credit

income_skew <- (3*(mean(loan.data$AMT_INCOME_TOTAL)-median(loan.data$AMT_INCOME_TOTAL))) / sd(loan.data$AMT_INCOME_TOTAL)
income_skew

credit_skew <- (3*(mean(loan.data$AMT_CREDIT)-median(loan.data$AMT_CREDIT))) / sd(loan.data$AMT_CREDIT)
credit_skew

skewincome = loan.data$zscore.income     
skewa = skewness(skewincome)                #skewness fc.
skewa
skewcredit = loan.data$zscore.credit     
skewa = skewness(skewcredit)                #skewness fc
skewa

outlierKD(loan.data, zscore.income)

loan.data$zscore.credit <- (loan.data$AMT_CREDIT - mean(loan.data$AMT_CREDIT))/sd(loan.data$AMT_CREDIT)
loan.data$zscore.credit

skewcredit = loan.data$zscore.credit     
skewa = skewness(skewcredit)                #skewness fc
skewa

#a more manual approach
amt_credit_sd <- sd(loan.data$AMT_CREDIT)
amt_credit_mean <- mean(loan.data$AMT_CREDIT)
amt_credit_sd
amt_credit_mean
zscore.amt_credit <- (loan.data$AMT_CREDIT - amt_credit_mean) / amt_credit_sd

outlierKD(loan.data, zscore.credit) 


#Remove outliers 
clean.data <-  loan.data[! (zscore.amt_credit > 3) ,]
# and use invert square, we get normal distribution
invert_sq_amt_credit = 1 / sqrt(clean.data$AMT_CREDIT)
qqnorm(invert_sq_amt_credit,
       datax = TRUE,
       col = "red",
       ylim = c(0.0007441199, 0.004714045),
       main = "Normal Q-Q Plot of Cleaned Inverted amount credit")
qqline(invert_sq_amt_credit,
       col = "blue",
       datax = TRUE)

# Use binning to discretize AMT_TOTAL_INCOME
# into five bins named A through E with A being the lowest
# and E being the highest - name the new attribute CAT_AMT_TOTAL_INCOME

loan.data$CAT_AMT_TOTAL_INCOME[loan.data$AMT_INCOME_TOTAL > 0 & loan.data$AMT_INCOME_TOTAL <= 100000 ] <- "A"
loan.data$CAT_AMT_TOTAL_INCOME[loan.data$AMT_INCOME_TOTAL > 100000 & loan.data$AMT_INCOME_TOTAL <= 150000 ] <- "B"
loan.data$CAT_AMT_TOTAL_INCOME[loan.data$AMT_INCOME_TOTAL > 150000 & loan.data$AMT_INCOME_TOTAL <= 200000 ] <- "C"
loan.data$CAT_AMT_TOTAL_INCOME[loan.data$AMT_INCOME_TOTAL > 200000 & loan.data$AMT_INCOME_TOTAL <= 250000 ] <- "D"
loan.data$CAT_AMT_TOTAL_INCOME[loan.data$AMT_INCOME_TOTAL > 250000 ] <- "E"



#Use MICE to impute appropriate values for
#the missing values in CNT_CHILDREN (note:  the actual value in 
#each case was 0)
#install.packages("mice")
#library(mice)
md.pattern(loan.data)
dataframe_children <- as.data.frame(CNT_CHILDREN)
#note maxit is low due to time for processing can be higher
imputed_Data <- mice(loan.data, m=1, maxit = 2, method = 'cart', seed = 500)
imputed_Data$imp$CNT_CHILDREN
summary(imputed_Data)
densityplot(imputed_Data)
completedData <- complete(imputed_Data,1)
sapply(completedData, function(x) sum(is.na(x)))


#Contingency table
table(loan.data$NAME_FAMILY_STATUS,loan.data$NAME_HOUSING_TYPE)

count(loan.data,vars = c("NAME_FAMILY_STATUS","NAME_HOUSING_TYPE"))

#PCA
classes <- sapply(loan.data.all, class)
loan.data.numeric <- loan.data.all[,classes=="numeric"]
#loan.data.complete <- complete.cases(loan.data.all[,classes=="numeric"])
loan.data.complete <- na.omit(loan.data.numeric) #!is.na(loan.data.numeric))
m.pca <- apply(loan.data.complete, 2, mean)
s.pca <- apply(loan.data.complete, 2, sd)
z.pca <- scale(loan.data.complete, m.pca, s.pca)


#we observe 65 principal components with 26 PCs over 85% contributing to the 
#variability we see in the sample
l.pca <- prcomp(z.pca, center = TRUE, scale. = TRUE)
summary(l.pca)
head(l.pca)
str(l.pca)
distance <- dist(z.pca)
str(distance)

#K-means clustering
#standardize numeric values
classes <- sapply(loan.data.all, class)
num.loan <- loan.data.all[,classes=="numeric"]
num.loan$SK_ID_CURR <- loan.data.all$SK_ID_CURR
num.loan$TARGET <- loan.data.all$TARGET
head(num.loan,1)
df <- na.omit(num.loan)
df2.scale<-na.omit(num.loan)
m <- apply(df2.scale[,-c(66, 67)], 2, mean)
s <- apply(df2.scale[,-c(66, 67)], 2, sd)
#classes <- sapply(df, class)
#z<- scale(df[,classes=="numeric"], m, s)
df2.scale[,-c(66,67)] <- lapply(df2.scale[,-c(66, 67)], function(x) c(scale(x), m, s))
z<- scale(df[,-c(66, 67)], m, s)
head(df2.scale,1)

distance.euclidian <- dist(df2.scale[,-c(66,67)], method="euclidian")
#install.packages("corrplot")
library("corrplot")
str(distance.euclidian)
rand.sample.vect<-sample(1:100, 10)
#corrplot(as.matrix(distance.euclidian), is.corr = FALSE, method="color")
#identifying correlation between individual samples
round(as.matrix(distance.euclidian)[1:6, 1:6], 1)

resp.correlation <- cor(t(z), method="pearson")
distance.correlation <- as.dist(1-resp.correlation)
round(as.matrix(distance.correlation)[1:6, 1:6], 1)



# Scree Plot for k-means and selecting the number of clusters
#there are 3 obvious clusters
wss <- (nrow(df2.scale[,-c(66,67)])-1)*sum(apply(df2.scale[,-c(66,67)],2,var))
for (i in 2:20) wss[i] <- sum(kmeans(df2.scale[,-c(66,67)], centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

# K-means clustering
kc<-kmeans(df2.scale[,-c(66,67)],3)

#we see that the ratio between_SS values and total_SS values is: 25.4%
kc

kc$cluster
kc$centers

#Determine which Id belongs to which cluster and add this data
# to the df2.scale in form of cluster number:1, 2, or 3 per each id
out <- cbind(df2.scale, Credit.Cluster.Num = kc$cluster)
head(out,1)
plot(out[,c("AMT_CREDIT","Credit.Cluster.Num")])
plot(out[,"Credit.Cluster.Num"], (out[, "AMT_INCOME_TOTAL"]))

#there are 0 unavailable values for credit clusters since we removed the NA info at the begining
sum(is.na(out$Credit.Cluster.Num))

#there are 0 duplicate credit records
sum(duplicated(out$SK_ID_CURR))

#adding the credit cluster data to the training data too:
loan.data.all.wClusterCredit <- cbind(loan.data.all, Credit.Cluster.Num = NA) #create column
for (row in 1:nrow(out)) {
  SK_ID <- out[row, "SK_ID_CURR"]
  SK_ID
  #update column
  loan.data.all.wClusterCredit[loan.data.all.wClusterCredit$SK_ID_CURR==SK_ID, "Credit.Cluster.Num"] <- out[out$SK_ID_CURR==SK_ID, "Credit.Cluster.Num"]
}
head(loan.data.all.wClusterCredit)

#loan.data.all.wClusterCredit["SK_ID_CURR"==100083, "Credit.Cluster.Num"] <- out["SK_ID_CURR"==100083, "Credit.Cluster.Num"]




#CLusters for training only:
#K-means clustering
#standardize numeric values
#df.no.na.col <- sapply(names(loan.data.all), function(x){if(sum(is.na(loan.data.all[,x]))>100){drop(loan.data.all, rm(,x))}})

#df.no.na.col <- sapply(names(loan.data.all), function(x){(colSums(is.na(loan.data.all[,x]))>100)})

#loan.data.all <- na.omit(loan.data.all[df.no.na.col])
#loan.data.all2<-na.omit(loan.data.all[df.no.na.col])
setwd("~/Documents/UNCC/ITCS6162_KDD/DataProcessingandModeling")

#loan.data <- read.csv(file="application_train.csv", header=TRUE, sep=",")
loan.data.all <- read.csv(file="./all/application_train_all.csv", header=TRUE, sep=",")
loan.data.all2.no.na <-na.omit(loan.data.all[ , colSums(is.na(loan.data.all)) <100])
classes <- sapply(loan.data.all, class)
num.loan <- loan.data.all[,classes=="numeric"]
num.loan$SK_ID_CURR <- loan.data.all$SK_ID_CURR
num.loan$TARGET <- loan.data.all$TARGET
head(num.loan,1)
df <- na.omit(num.loan)
df2.scale<-na.omit(num.loan)
m <- apply(df2.scale[,-c(66, 67)], 2, mean)
s <- apply(df2.scale[,-c(66, 67)], 2, sd)
#classes <- sapply(df, class)
#z<- scale(df[,classes=="numeric"], m, s)
df2.scale[,-c(66,67)] <- lapply(df2.scale[,-c(66, 67)], function(x) c(scale(x), m, s))
z<- scale(df[,-c(66, 67)], m, s)
head(df2.scale,1)

distance.euclidian <- dist(df2.scale[,-c(66,67)], method="euclidian")
#install.packages("corrplot")
library("corrplot")
str(distance.euclidian)



# Scree Plot for k-means and selecting the number of clusters
#there are 3 obvious clusters
wss <- (nrow(df2.scale[,-c(66,67)])-1)*sum(apply(df2.scale[,-c(66,67)],2,var))
for (i in 2:20) wss[i] <- sum(kmeans(df2.scale[,-c(66,67)], centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

# K-means clustering
kc<-kmeans(df2.scale[,-c(66,67)],3)

#we see that the ratio between_SS values and total_SS values is: 25.4%
kc

kc$cluster
kc$centers

#Determine which Id belongs to which cluster and add this data
# to the df2.scale in form of cluster number:1, 2, or 3 per each id
out <- cbind(df2.scale, Cluster.Num = kc$cluster)
head(out,1)
plot(out[,c("AMT_CREDIT","Cluster.Num")])
plot(out[,"Cluster.Num"], (out[, "AMT_INCOME_TOTAL"]))

#there are 0 unavailable values for credit clusters since we removed the NA info at the begining
sum(is.na(out$Cluster.Num))

#there are 0 duplicate credit records
sum(duplicated(out$SK_ID_CURR))

#adding the credit cluster data to the training data too:
loan.data.all.wCluster <- cbind(loan.data.all, Cluster.Num = NA) #create column
for (row in 1:nrow(out)) {
  SK_ID <- out[row, "SK_ID_CURR"]
  SK_ID
  #update column
  loan.data.all.wCluster[loan.data.all.wCluster$SK_ID_CURR==SK_ID, "Cluster.Num"] <- out[out$SK_ID_CURR==SK_ID, "Cluster.Num"]
}
head(loan.data.all.wCluster)

#loan.data.all.wClusterCredit["SK_ID_CURR"==100083, "Credit.Cluster.Num"] <- out["SK_ID_CURR"==100083, "Credit.Cluster.Num"]
cluster.training.set <- na.omit(loan.data.all.wCluster)
sum(cluster.training.set$TARGET==1)
sum(cluster.training.set$TARGET==0)

# Load libraries
#install.packages('mlbench') & install.packages('caret') & install.packages('caretEnsemble')
library(mlbench)
library(caret)
library(caretEnsemble)
# Example of Bagging algorithms
control <- trainControl(method="cv", number=10)#, repeats=3)#cv
seed <- 7
metric <- "Accuracy"
# Bagged CART
set.seed(seed)
dataset<-na.omit(cluster.training.set)
fit.treebag <- train(as.factor(TARGET)~., data=dataset, method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(seed)
fit.rf <- train(as.factor(TARGET)~., data=dataset, method="rf", metric=metric, trControl=control, ntree=100)
# summarize results
bagging_results <- resamples(list(treebag=fit.treebag, rf=fit.rf))
summary(bagging_results)
dotplot(bagging_results)
