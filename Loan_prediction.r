#Predict Loan
library(ggplot2)
library(gridExtra)
library(corrplot)
library(plyr)
library(dplyr)
library(caret)
# imputaion of missing values
library(mice)
library(VIM)
# parallel computing
library(doParallel)

setwd("C:/Users/srinivas/Desktop/6 Dataset to solve/Loan Prediction Data/Loan-Prediction")
train_data <- read.csv("train_u6lujuX_CVtuZ9i.csv",header = T ,na.strings = "")
test_data <- read.csv("test_Y3wMUE5_7gLdaTN.csv",header = T ,na.strings = "")
apply(apply(train_data,2,is.na),2,sum);nrow(train_data)
apply(apply(test_data,2,is.na),2,sum);nrow(test_data)
str(train_data)
head(train_data)
summary(train_data)
sapply(train_data, function(x) length(unique(x)))

#check dimesions ( number of row & columns) in data sets
dim(train_data)
dim(test_data)

#Imputing Missing Values
sum(is.na(train_data))
library(DMwR)
train_data <- knnImputation(train_data)
test_data  <- knnImputation(test_data) 
str(train_data)
str(test_data)
head(train_data)
head(test_data)


#Missing Values
# train_data$LoanAmount[which(is.na(train_data$LoanAmount))] <- 0
# train_data$Loan_Amount_Term[which(is.na(train_data$Loan_Amount_Term))] <- 0
# train_data$Credit_History[which(is.na(train_data$Credit_History))] <- 0
# train_data$Gender[which(is.na(train_data$Gender))] -> Others
# train_data$Married[which(is.na(train_data$Married))] -> Others
# train_data$Dependents[which(is.na(train_data$Dependents))] <- 0
# train_data$Self_Employed[which(is.na(train_data$Self_Employed))] -> Others
# 
# test_data$LoanAmount[which(is.na(test_data$LoanAmount))] <- 0
# test_data$Loan_Amount_Term[which(is.na(test_data$Loan_Amount_Term))] <- 0
# test_data$Credit_History[which(is.na(test_data$Credit_History))] <- 0
# test_data$Gender[which(is.na(test_data$Gender))] -> Others
# test_data$Dependents[which(is.na(test_data$Dependents))] <- 0
# test_data$Self_Employed[which(is.na(test_data$Self_Employed))] -> Others


#Target Variable
train_data$Loan_Status <- ifelse(train_data$Loan_Status == "Y", 1, 0)
train_data$Loan_Status <- factor(train_data$Loan_Status, levels = c(0, 1))
str(train_data)
#Combine train data and test data in one
test_data$Loan_Status <- 0
combi <- rbind(train_data, test_data)
str(combi)
head(combi)


#splitting the data to train and test
smp_size <- floor(0.75 * nrow(combi))
set.seed(123)
training <- sample(seq_len(nrow(combi)), size = smp_size)

train <- combi[training, ]
test <- combi[-training, ]
attach(combi)

train <- subset(train,select = c(2,3,4,5,6,7,8,9,10,11,12,13))
test  <- subset(test,select = c(2,3,4,5,6,7,8,9,10,11,12,13))

#Build a Model
model1 <- glm(Loan_Status ~.,family=binomial(link='logit'),data=train) 
summary(model1)
anova(model1, test="Chisq")


#Prediction
fitted.result <- predict(model1,newdata= test,type='response')
fitted.results <- ifelse(fitted.result > 0.5,1,0)
misClasificError <- mean(fitted.results != test$Loan_Status)
print(paste('Accuracy',1-misClasificError))
table(test$Loan_Status,fitted.results)



