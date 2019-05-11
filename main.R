library(caret)
library(e1071)
library(AUC)


train <- read.csv("Credit_train.csv")
test <- read.csv("Credit_test.csv")

#Rows and Cols
dim(train)
dim(test)

#Columns name
colnames(train)
colnames(test)

#Show  
head(train)
head(test)


#----------
# Bayesian
#----------

#train
model.Bayes <- naiveBayes(DEFAULT~., data = train)
model.Bayes

#test
pc <-NULL
pc <- predict(model.Bayes, test, type = "class")
summary(pc)
xtab <- table(pc, test$DEFAULT)
caret::confusionMatrix(xtab, positive = "Y")



#---------------------
# K Nearest Neighbors
#---------------------
library(caret)
library(kknn)
library(AUC)

#remove all records with missing values
train <- na.omit(train)
test <- na.omit(test)

#train
model.KNN <- kknn(DEFAULT~., train, test, k=5, distance = 2, scale=FALSE)
summary(model.KNN)

#confusion matrix
pc <- NULL
pc <- predict(model.KNN, test, type="raw")
xtab <- table(pc, test$DEFAULT)
caret::confusionMatrix(xtab, positive="Y")