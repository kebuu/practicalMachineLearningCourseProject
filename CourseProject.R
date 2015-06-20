setwd("E:/ctardella/MachineLearning/coursera/PracticalMLCourse")

library(caret)
library(randomForest)

wleTraining <-  read.csv(file = "pml-training.csv")
wleTrainingNewWindowRows <- which(wleTraining$new_window == "yes")

technicalFeatures <- c("X","user_name","raw_timestamp_part_1","raw_timestamp_part_2","cvtd_timestamp","new_window","num_window")

tidyWleTraining <- wleTraining[,which(!names(wleTraining) %in% technicalFeatures)]
nsv <- nearZeroVar(tidyWleTraining,saveMetrics=TRUE)
tidyWleTrainingVar <- tidyWleTraining[,!nsv$nzv]

rfModel <- randomForest(classe ~ ., data=tidyWleTrainingVar, ntree = 10)

# prData <- prcomp(tidyWleTrainingVar[, -53], center = T,scale. = T)
# fitControl <- trainControl(method = "cv", number = 10)


correl <- findCorrelation(cor(tidyWleTrainingVar[,!names(tidyWleTrainingVar) %in% c("classe")]))
sort(abs(cor(tidyWleTrainingVar[,-53])[,1]))

tidyWleTrainingVar <- tidyWleTrainingVar[,-correl]
rfModel <- train(classe ~ ., data=wleTraining, method = "rf", trControl = trainControl(method = "cv", number=10), tuneLength = 5, ntree=10)
rpartModel <- train(classe ~ ., data=tidyWleTraining, method = "rpart", trControl = trainControl(method = "cv", number=10), tuneLength = 1)
rfModel <- train(classe ~ ., data=tidyWleTrainingVar, method = "rf", trControl = trainControl(method = "cv", number=10), tuneGrid = data.frame(mtry=log2(159)+1), ntree=50)
rfModel <- train(classe ~ ., data=tidyWleTrainingVar[,-correl], method = "rf", trControl = trainControl(method = "cv", number=10, preProcOptions = list(thresh=1)), tuneGrid = data.frame(mtry=log2(52)+1), ntree=50, preprocess="pca")
rfModel <- train(classe ~ ., data=tidyWleTrainingVar, method = "rf", trControl = trainControl(method = "cv", number=10), tuneGrid = data.frame(mtry=log2(52)+1), ntree=50)

