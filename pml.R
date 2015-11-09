library(ElemStatLearn)
library(caret)
library(rpart)
library(randomForest)
set.seed(250)

#read in training data
pml_train <- read.csv("pml-training.csv", header=TRUE, sep=",", na.strings=c("NA",""))
pml_train <- pml_train[,-1] # Remove the first column that represents a ID Row


inTrain = createDataPartition(pml_train$classe, p=0.60, list=FALSE)
training = pml_train[inTrain,]
validating = pml_train[-inTrain,]

sum((colSums(!is.na(training[,-ncol(training)])) < 0.6*nrow(training)))

Keep <- c((colSums(!is.na(training[,-ncol(training)])) >= 0.6*nrow(training)))
training   <-  training[,Keep]
validating <- validating[,Keep]

modelRF <- randomForest(classe~.,data=training)
print(modelRF)

importance(modelRF)

confusionMatrix(predict(modelRF,newdata=validating[,-ncol(validating)]),validating$classe)

accurancyRate <-c (as.numeric(predict(modelRF,newdata=validating[,-ncol(validating)])==validating$classe))
accurancyRate <- sum(accurancyRate)*100/nrow(validating)

accurancy

# read in test data
pml_test <- read.csv("pml-testing.csv", header=TRUE, sep=",", na.strings=c("NA",""))
pml_test <- pml_test[,-1] # Remove the first column that represents a ID Row
pml_test <- pml_test[,-ncol(pml_test)] # Remove the problem ID
pml_test <- pml_test[,Keep] # Keep the same columns of testing dataset


testing <- rbind(training[100, -59] , pml_test) # Coerce testing dataset to same class and strucuture of training dataset 
row.names(testing) <- c(100, 1:20)
predictions <- predict(modelRF,newdata=testing[-1,])
print(predictions)


pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictions)