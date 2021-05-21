data <- read.csv(file = "Boston.csv", header = TRUE)
head(Boston)

install.packages('RSNNS')
library(RSNNS)
set.seed(20)
Boston <- Boston[sample(1:nrow(Boston), length(1:nrow(Boston))), 1:ncol(Boston)]
Boston_Values <- Boston[, 1:4]
Boston_Target <- decodeClassLabels(Boston[, 5])
Boston <- splitForTrainingAndTest(Boston_Values, Boston_Target, ratio = 0.15)
Boston <- normTrainingAndTestSet(Boston)
hist(Boston$inputsTrain)

model <- mlp(Boston$inputsTrain,
             Boston$targetsTrain,
             size = 5,
             learnFuncParams = c(0.1),
             maxit = 50,
             inputsTest = Boston$inputsTest,
             targetsTest = Boston$targetsTest)
summary(model)
model
weightMatrix(model)

par(mfrow = c(2, 2))
plotIterativeError(model)

predictions <- predict(model, Boston$inputsTest)
plotRegressionError(predictions[,2], Boston$targetsTest[,2])

confusionMatrix(Boston$targetsTrain, fitted.values(model))
confusionMatrix(Boston$targetsTest, predictions)

plotROC(fitted.values(model)[,2], Boston$targetsTrain[,2])
plotROC(predictions[,2], Boston$targetsTest[,2])

confusionMatrix(Boston$targetsTrain, encodeClassLabels(fitted.values(model), method = "402040", l = 0.5, h = 0.51))

library(NeuralNetTools)
par(mfrow = c(1, 1))
plotnet(model)
