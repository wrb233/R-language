library(glmnet)
library(class)
library(caret)
library(boot)
df1<-read.csv("~/Downloads/processed.cleveland.data",header=FALSE,sep=",",na.strings = "?")
df=na.omit(df1)
clevnames=c('age','sex','cp','trestbps','chol','fbs','restecg','thalach','exang','oldpeak','slope','ca','thal','num')
colnames(df)=clevnames
df$num[df$num > 0] <- 1
df$restecg[df$restecg == 1] <- 2
str(df)


df$sex <- as.factor(df$sex)
df$cp <- as.factor(df$cp)
df$fbs <- as.factor(df$fbs)
df$restecg <- as.factor(df$restecg)
df$exang <- as.factor(df$exang)
df$slope <- as.factor(df$slope)
df$ca <- as.factor(df$ca)
df$thal <- as.factor(df$thal)
df$num <- as.factor(df$num)
str(df)




RNGkind(sample.kind = "Rounding")
set.seed(10)

inTrainRows <- createDataPartition(df$num,p=0.7,list=FALSE)
trainData <- df[inTrainRows,]
testData <-  df[-inTrainRows,]
nrow(trainData)/(nrow(testData)+nrow(trainData))

logRegModel <- train(num ~ ., data=trainData, method = 'glm', family = 'binomial')
summary(logRegModel)

logRegPredictionprob <- predict(logRegModel, testData, type='prob')[2]
summary(logRegPredictionprob)
head(logRegPredictionprob)

train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(num ~., data = df, method = "glm",
               trControl = train.control)

summary(model)

p<-predict(model,df)
tab<-table(p,df$num)
tab
sum(diag(tab))/sum(tab)
1-(sum(diag(tab))/sum(tab))
