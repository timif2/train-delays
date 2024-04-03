library(randomForest)
library(caret)

# Load the training data
load("DesignMatrix.RData")

# Reset data with the 3 outliers removed
matrixData <- matrixData[matrixData$delayNotts < 2000, ]

# Set.seed to Reproduce the same result
set.seed(9088)

# Create the Random Forest model and Use 10-Fold Cross-Validation to Optimize the model
rf.fit <- train(delayNotts ~ ., data=matrixData, method="rf", trControl=trainControl(method="cv", number=10))

# Make predictions on test data
load("test_Matrix.RData")
pred_test <- predict(rf.fit, newdata = data_test)

write.csv(pred_test, file = "trains_group_D_week_6.csv", row.names=FALSE)