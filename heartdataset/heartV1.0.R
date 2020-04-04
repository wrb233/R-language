#Load the libraries to be used
library(tidyverse)
library(kableExtra)
library(rsample)
library(recipes)
library(parsnip)
library(yardstick)
library(viridisLite)
library(GGally)
library(nnet)
library(caret)
library(e1071)

#Import the Cleveland Dataset
heart.data <- read.csv(url("https://archive.ics.uci.edu/ml/machine-learning-databases/heart-disease/processed.cleveland.data"), header=FALSE,sep=",",na.strings = '?')
#Prepare column names and Apply column names to the dataframe
names(heart.data) <- c("age",
                       "sex",
                       "cp",
                       "trestbps",
                       "chol",
                       "fbs",
                       "restecg",
                       "thalach",
                       "exang",
                       "oldpeak",
                       "slope",
                       "ca",
                       "thal",
                       "num")




#Delete missing data
heart.data[heart.data == "?"] <- NA
heart.data <- na.omit(heart.data)
#the sum of missing data is 0 
sum(is.na(heart.data))



heart.data$restecg[heart.data$restecg == 1] <- 2


#Make sure that the variable “num” is turned into a factor with two categories: 0 and 1 (for categories different from 0).
heart.data$num[heart.data$num > 0] <- 1


str(heart.data)

#change a few predictor variables from integer to factors (make dummies)
#Make sure that all those variables that should be factors are converted into factors, and all the variables that should be numeric are numeric.
heart.data$sex <- as.factor(heart.data$sex)
heart.data$cp <- as.factor(heart.data$cp)
heart.data$fbs <- as.factor(heart.data$fbs)
heart.data$restecg <- as.factor(heart.data$restecg)
heart.data$exang <- as.factor(heart.data$exang)
heart.data$slope <- as.factor(heart.data$slope)
heart.data$ca <- as.factor(heart.data$ca)
heart.data$thal <- as.factor(heart.data$thal)
heart.data$num <- as.factor(heart.data$num)

str(heart.data)







#Randomly split the dataset so that 70% of the data is your training set and 30% of the data is your testing set.
set.seed(10)
inTrainRows <- createDataPartition(heart.data$num,p=0.7,list=FALSE)
trainData <- heart.data[inTrainRows,]
testData <-  heart.data[-inTrainRows,]
nrow(trainData)/(nrow(testData)+nrow(trainData)) #checking whether is 70% -> OK



#Create a logistic regression model that yields the output below.
#logRegModel <- train(num ~ ., data=trainData, method = '(glm)', family = 'binomial')
#summary(logRegModel)
heart_logitMod <- glm(formula = num ~ ., 
                      family = binomial(link="logit"), 
                      data = trainData)
summary(heart_logitMod)









heart_prediction <- predict(heart_logitMod, testData, type="response")

pre_df <- data.frame(heart_prediction)

summary(pre_df)

head(pre_df)







train.control <- trainControl(method = "cv", number = 10)
trainmodel <- train(num ~., data = heart.data, method = "glm",
               trControl = train.control)
summary(trainmodel)



ame<-predict(trainmodel,heart.data)
t<-table(ame,heart.data$num)
t
1-(sum(diag(t))/sum(t))







