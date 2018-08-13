
#Mihai

#install.packages("FactoMineR") & install.packages("factoextra")
library(FactoMineR)
library(factoextra)
library(dplyr)
library(tidyr)
library(mlbench)
library(caret)
library(corrplot)
library(caretEnsemble)
library(rpart)
library(rattle)

setwd("~/Documents/UNCC/ITCS6162_KDD/DataProcessingandModeling")

#loan.data <- read.csv(file="application_train.csv", header=TRUE, sep=",")
loan.data.all <- read.csv(file="./all/application_train_all.csv", header=TRUE, sep=",")
loan.data.all2.no.na <-na.omit(loan.data.all[ , colSums(is.na(loan.data.all)) <100])

#standardize selected columns
out_col <- c(8,9,10,16,17,18,19,20,44)
for(i in 1:length(out_col)){
  loan.data.all2.no.na[,out_col[i]] <- (loan.data.all2.no.na[,out_col[i]] - mean(loan.data.all2.no.na[,out_col[i]]))/sd(loan.data.all2.no.na[,out_col[i]])
}

#outliers removing 
for(i in 1:length(out_col)){
  loan.data.all2.no.na <- loan.data.all2.no.na[!(loan.data.all2.no.na[,out_col[i]] > 3) ,]
}

#write.csv(loan.data.all2.no.na, 'all/training.all.no.na.csv', row.names=FALSE)
loan.data.all2.no.na <- read.csv(file="./all/training.all.no.na.csv", header=TRUE, sep=",")
test.data <- read.csv(file="./all/application_test.csv", header=TRUE, sep=",")
#take a random sample of size 3000
loan.data.all2.no.na <- loan.data.all2.no.na[sample(1:nrow(loan.data.all2.no.na), 1000,
                                                    replace=FALSE),]
options(warn=-1)
suppressWarnings() 

#saving the ones
target.one <- loan.data.all2.no.na[loan.data.all2.no.na$TARGET==1,]
#how many samples have target 1
sum(target.one$TARGET)
#saving zeros
target.zero <- loan.data.all2.no.na[loan.data.all2.no.na$TARGET==0,]
#how many zeros
sum(!target.zero$TARGET)

#clustering zeros
classes <- sapply(target.zero, class)
num.loan <- target.zero[,classes=="numeric"]
num.loan$SK_ID_CURR <- target.zero$SK_ID_CURR
num.loan$TARGET <- target.zero$TARGET
head(num.loan,1)
df <- na.omit(num.loan) #just to make sure NA omit
df2.scale<-na.omit(num.loan)
omit.columns <- c("SK_ID_CURR","TARGET")
m <- apply(df2.scale[,-which(names(df2.scale) %in% omit.columns)], 2, mean)
s <- apply(df2.scale[,-which(names(df2.scale) %in% omit.columns)], 2, sd)
#classes <- sapply(df, class)
#z<- scale(df[,classes=="numeric"], m, s)
df2.scale[,-which(names(df2.scale) %in% omit.columns)] <- lapply(df2.scale[,-which(names(df2.scale) %in% omit.columns)], function(x) c(scale(x), m, s))
z<- scale(df[,-which(names(df2.scale) %in% omit.columns)], m, s)
head(df2.scale,1)

# Scree Plot for k-means and selecting the number of clusters
#there are 3 obvious clusters
wss <- (nrow(df2.scale[,-which(names(df2.scale) %in% omit.columns)])-1)*sum(apply(df2.scale[,-which(names(df2.scale) %in% omit.columns)],2,var))
for (i in 2:20) wss[i] <- sum(kmeans(df2.scale[,-which(names(df2.scale) %in% omit.columns)], centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares") 

# K-means clustering
kc<-kmeans(df2.scale[,-which(names(df2.scale) %in% omit.columns)],3)

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
target.zero.wCluster <- cbind(target.zero, Cluster.Num = NA) #create column
for (row in 1:nrow(out)) {
  SK_ID <- out[row, "SK_ID_CURR"]
  SK_ID
  #update column
  target.zero.wCluster[target.zero.wCluster$SK_ID_CURR==SK_ID, "Cluster.Num"] <- out[out$SK_ID_CURR==SK_ID, "Cluster.Num"]
}
head(target.zero.wCluster)

#loan.data.all.wClusterCredit["SK_ID_CURR"==100083, "Cluster.Num"] <- out["SK_ID_CURR"==100083, "Credit.Cluster.Num"]
cluster.training.set <- na.omit(target.zero.wCluster)
sum(cluster.training.set$TARGET==1)
sum(cluster.training.set$TARGET==0)

#uncheck the following to save the ones and zeros
#write.csv(target.zero.wCluster, 'all/target.zero.wCluster.csv', row.names=FALSE)
#write.csv(target.one, 'all/target.one.csv', row.names=FALSE)


###############################################
# ensure the results are repeatable
set.seed(7)
# calculate correlation matrix
correlationMatrix <- cor(target.zero[,classes=="numeric"])#3:64
# summarize the correlation matrix
print(correlationMatrix)
# find attributes that are highly corrected (ideally >0.75)
highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# print indexes of highly correlated attributes
print(highlyCorrelated) #AMT_ANNUITY/AMT_CREDIT 
corrplot(correlationMatrix, method=c('number'),title='correlation matrix',diag=T)

#Rank features by importance using the caret r packageR

# prepare training scheme
loan.data.all2.no.na$TARGET<-as.factor(loan.data.all2.no.na$TARGET)
d <- loan.data.all2.no.na[,classes=="numeric" || classes=="integer" || classes=="double"]
control <- trainControl(method="repeatedcv", number=3)#, repeats=3)
# train the model
model <- train(TARGET~., data=d[,c(2:64)], method="lvq", preProcess="scale", trControl=control)
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
plot(importance)



##Feature selection (dimensionality reduction) with PCA
classes <- sapply(loan.data.all2.no.na, class)
loan.data.numeric <- loan.data.all2.no.na[,classes=="numeric"]
#loan.data.complete <- complete.cases(loan.data.all[,classes=="numeric"])
loan.data.complete <- na.omit(loan.data.numeric) #!is.na(loan.data.numeric))
m.pca <- apply(loan.data.complete, 2, mean)
s.pca <- apply(loan.data.complete, 2, sd)
z.pca <- scale(loan.data.complete, m.pca, s.pca)


#we observe 65 principal components with 26 PCs over 85% contributing to the 
#variability we see in the sample
l.pca <- prcomp(z.pca)#, center = TRUE, scale. = TRUE)
summary.pca <- summary(l.pca)
#head(l.pca)
#str(l.pca)
#distance <- dist(z.pca)
#str(distance)

#percentage of variance captured by PCA
pca_pr <- round(100*summary.pca$importance[2, ], digits = 1)
pca_pr
#axis labels
pc_lab <- paste0(names(pca_pr), " (", pca_pr, "%)")
pc_lab
#pca biplot
biplot(l.pca, cex = c(0.8, 1), col = c("grey40", "deeppink2"), xlab = pc_lab[1], ylab = pc_lab[2])

#PCA with MLBench
preprocParam <- preProcess(loan.data.numeric, method=c("center", "scale", "pca"))
# summarize preprocessed 
print(preprocParam)
# transform the data
tran <- predict(preprocParam, loan.data.all2.no.na)
# summarize the transformed dataset
summary(tran)




## Feature selection with unsupervised Multiple Correspondence Analysis (MCA)
theme_set(theme_bw(12))
nfactors <- apply(loan.data.all2.no.na, 2, function(x) nlevels(as.factor(x))) 
nfactors
head(loan.data.all2.no.na[,c(2:5)])
col.selected <- c(3:6, 11:15, 27, 31, 39:43)
loan.selected <- select(loan.data.all2.no.na, col.selected)
mca <- MCA(loan.selected, graph = FALSE)
#length(loan.data.all2.no.na)
# summary of the model
summary(mca)
summary(loan.selected)

#visualize the dataset
dev.off()
gather(loan.selected) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar() + theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 3))
# visualize MCA
plot.MCA(mca)
plot(mca, invisible=c("ind"), habillage = "quali", cex=0.5)

#income type occupation type


## Supervised feature selection with Caret
# prepare training scheme
control <- trainControl(method="repeatedcv", number=3)#, repeats=3)
# train the model
loan.data.all2.no.na$TARGET<-as.factor(loan.data.all2.no.na$TARGET)
model <- train(TARGET~., data=loan.data.all2.no.na[,c(2:64)], method="lvq", preProcess="scale", trControl=control)#, ntree=50
# estimate variable importance
importance <- varImp(model, scale=FALSE)
# summarize importance
print(importance)
# plot importance
par(cex.lab=2)
dev.off()
plot.new()
axis(2,cex.axis=1, cex.lab=1)
plot(importance, cex.names=0.1, cex.lab=1, cex.axis=1) #cex=1

##Supervised Recursive Feature Elimination (RFE)
control <- rfeControl(functions=rfFuncs, method="cv", number=3)
# run the RFE algorithm
#results <- rfe(loan.data.all2.no.na[,c(2:10)], loan.data.all2.no.na[,c(1)], sizes=c(2:10), rfeControl=control)
results <- rfe(loan.d[,c(2:10)], loan.data.all2.no.na[,c(1)], sizes=c(2:10), rfeControl=control)
# summarize the results
print(results)
# list the chosen features
predictors(results)
# plot the results
plot(results, type=c("g", "o"))


#creating levels
#str(loan.data.all2.no.na)
#int.numerical <- loan.data.all2.no.na[, classes=='integer']
#creating factors out of the integer
for(i in 1:length(loan.data.all2.no.na)){
  if(class(loan.data.all2.no.na[,i])=='integer'){
    loan.data.all2.no.na[,i] <- as.factor(loan.data.all2.no.na[,i])
  }
}
# creating 3 level categorical variables from numerical using
# lower, medium and upper quartile

#checking the categories for each variable
nfactors <- apply(loan.data.all2.no.na, 2, function(x) nlevels(as.factor(x))) 
nfactors
#ensure the categories per variable are at least 2
loan.data.all2.no.na <- loan.data.all2.no.na[,!(nfactors<=1)]

# Stacking ensemble
#ensuring the TARGET has valid category names YES and NO not 0 and 1
levels(loan.data.all2.no.na$TARGET) <- c("NO", "YES")
levels(loan.data.all2.no.na$TARGET) 
#control <- trainControl(method="repeatedcv", number=10, repeats=3, savePredictions=TRUE, classProbs=TRUE)
control <- trainControl(method="repeatedcv", number=3, savePredictions=TRUE, classProbs=TRUE)
alg <- c('rpart', 'knn')# 'lda', 'svmRadial', 'glm'
set.seed(7)
models <- caretList(TARGET~., data=loan.data.all2.no.na[,2:4], trControl=control, methodList=alg)
r <- resamples(models)
summary(r)
dotplot(r)
# correlation
modelCor(r)
splom(r)

# stack using glm
stackControl <- trainControl(method="repeatedcv", number=3, repeats=3, savePredictions=TRUE, classProbs=TRUE)#
set.seed(7)
stack.glm <- caretStack(models, method='rpart', metric="Accuracy", trControl=stackControl)
print(stack.glm)

# stack using random forest
set.seed(7)
stack.rf <- caretStack(models, method="rf", metric="Accuracy", trControl=stackControl)
print(stack.rf)

## Ensamble bagging CART and Random Forest
control <- trainControl(method="repeatedcv", number=2, repeats=2)#cv
metric <- "Accuracy"
# Bagged CART
set.seed(7)
#dataset<-na.omit(cluster.training.set)
fit.treebag <- train(TARGET~., data=loan.data.all2.no.na[,1:5], method="treebag", metric=metric, trControl=control)
# Random Forest
set.seed(7)
fit.rf <- train(TARGET~., data=loan.data.all2.no.na[,1:5], method="rf", metric=metric, trControl=control, ntree=100)
# summarize results
bagging_results <- resamples(list(treebag=fit.treebag, rf=fit.rf))
summary(bagging_results)
dotplot(bagging_results)


##Simply rpart
loan.data.all2.no.na <- read.csv(file="./all/training.all.no.na.csv", header=TRUE, sep=",")

#-----

table(loan.data.all2.no.na$CODE_GENDER)
loan.data.all2.no.na[, CODE_GENDER=="F"]
value0 <- loan.data.all2.no.na[loan.data.all2.no.na$TARGET==0,]
value1 <- loan.data.all2.no.na[loan.data.all2.no.na$TARGET==1,]
value0.balance <- value0[sample(1:nrow(value0), nrow(value1),
                            replace=FALSE),]
data.balanced <- rbind(value0.balance,value1)

prop.table(table(loan.data.all2.no.na$TARGET,loan.data.all2.no.na$CODE_GENDER)/100)
table(loan.data.all2.no.na$TARGET,loan.data.all2.no.na$NAME_CONTRACT_TYPE)/100
table(loan.data.all2.no.na$TARGET,loan.data.all2.no.na$FLAG_OWN_CAR)/100

#rpart the decision tree
loan.tree <- rpart(TARGET ~ ., data = data.balanced, method = "class")

# Visualize the decision tree using plot() and text()
plot(loan.tree)
text(loan.tree)

# Load in the packages to build a fancy plot
#needs rattle
fancyRpartPlot(loan.tree)

##Kaggle: - Preparing the solutions and CSV file for the submission
# Making predictions on the test set
my_prediction <- predict(loan.tree, newdata = test.data, type = "class")

my_solution <- data.frame(SK_ID_CURR = test.data$SK_ID_CURR, TARGET = my_prediction)

nrow(my_solution)

write.csv(my_solution, file = "group5.csv", row.names = FALSE)
