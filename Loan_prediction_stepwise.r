#Predict Loan
library(ggplot2, quietly=TRUE)
library(scales, quietly=TRUE)
library(lattice, quietly=TRUE)
library(corrplot, quietly=TRUE)
library(dplyr, quietly=TRUE)
library(tidyr, quietly=TRUE)
library(grid, quietly=TRUE)
library(gridExtra, quietly=TRUE)
library(pROC, quietly=TRUE)
library(ROCR, quietly=TRUE)
library(caret, quietly=TRUE)
library(data.table, quietly=TRUE)


#working directory
path <- "~/Loan-Prediction"
#setwd("C:/Users/srinivas/Desktop/6 Dataset to solve/Loan Prediction Data/Loan-Prediction")
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
validate <- combi[-training, ]
attach(combi)


# Specify a null model with no predictors
null_model <- glm(Loan_Status ~ 1, data = train,family = "binomial")

# Specify the full model using all of the potential predictors
full_model <- glm(Loan_Status ~ ., data = train,family = "binomial")

# Use a forward stepwise algorithm to build a parsimonious model
step_model <- step(null_model, scope = list(lower = null_model, upper = full_model), direction = "forward")
summary(step_model)

# Store the perdictions in the source dataframe
validate$pred <- predict(step_model,validate,type="response")
validate$real <- validate$Loan_Status

train$pred <- predict(step_model,train,type="response")
train$real <- train$Loan_Status

plot.roc(validate$real,validate$pred, col = "red", main="ROC Validation set",
         percent = TRUE, print.auc = TRUE)


#A good cut-off point is the most challenging part. In Choosing Logisitic Regression's Cutoff Value for Unbalanced Dataset some usefull code is given
AccuracyCutoffInfo <- function( train, test, predict, actual )
{
  # change the cutoff value's range as you please 
  cutoff <- seq( .1, .9, by = .05 )
  
  accuracy <- lapply( cutoff, function(c)
  {
    # use the confusionMatrix from the caret package
    cm_train <- confusionMatrix( as.numeric( train[[predict]] > c ), train[[actual]] )
    cm_test  <- confusionMatrix( as.numeric( test[[predict]]  > c ), test[[actual]]  )
    
    dt <- data.table( cutoff = c,
                      train  = cm_train$overall[["Accuracy"]],
                      test   = cm_test$overall[["Accuracy"]] )
    return(dt)
  }) %>% rbindlist()
  
  # visualize the accuracy of the train and test set for different cutoff value 
  # accuracy in percentage.
  accuracy_long <- gather( accuracy, "data", "accuracy", -1 )
  
  plot <- ggplot( accuracy_long, aes( cutoff, accuracy, group = data, color = data ) ) + 
    geom_line( size = 1 ) + geom_point( size = 3 ) +
    scale_y_continuous( label = percent ) +
    ggtitle( "Train/Test Accuracy for Different Cutoff" )
  
  return( list( data = accuracy, plot = plot ) )
}

theme_set(theme_minimal())

accuracy_info <- AccuracyCutoffInfo( train = train, test = validate, predict = "pred", actual = "real" )
accuracy_info$plot
