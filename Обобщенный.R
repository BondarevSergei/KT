install.packages('MASS')
library('MASS')
View(Boston)
set.seed(20)

install.packages('neuralnet')
library('neuralnet')

data <- Boston

max_data <- apply(data, 2, max)
min_data <- apply(data, 2, min)

data_scaled <- scale(data, center = min_data, scale = max_data - min_data)

index <- sample(1:nrow(data), round(0.80*nrow(data)))

train_data <- as.data.frame(data_scaled[index,])
test_data <- as.data.frame(data_scaled[-index,])

n <- colnames(data)
f <- as.formula(paste('indus ~', paste(n[!n %in% 'indus'], collapse = '+')))

n1 <- neuralnet(f,data = train_data, hidden = c(9, 5, 3), linear.output = F)
plot(n1)

pr <- compute(n1, test_data[2:14])
print(pr$net.result)

pr$net.result <- sapply(pr$net.result, round, digits = 0)

test1 <- table(test_data$age, pr$net.result)
test1

sum(test1[1,])
sum(test1[2,])

Accuracy1 <- (test1[1,1] + test1[2, 2])/sum(test1)
Accuracy1




library('neuralnet')
install.packages('RSNNS')
library(RSNNS) 

base2 <- Boston[sample(1:nrow(Boston), length(1:nrow(Boston))), 
                1:ncol(Boston)]


base2_Values <- base2[,1]
base2_Target <- base2[, 2]


base2 <- splitForTrainingAndTest(base2_Values, base2_Target, ratio = 0.2)
base2 <- normTrainingAndTestSet(base2)


model <- mlp(base2$inputsTrain,
             base2$targetsTrain,
             size = 5,
             maxit = 50,
             inputsTest = base2$inputsTest,
             targetsTest = base2$targetsTest)

test2 <- confusionMatrix(base2$targetsTrain, encodeClassLabels(fitted.values(model), 
                                                               method = "402040", l = 0.5, h = 0.51))
test2

sum(test2[1,])
sum(test2[2,])

Accuracy2 <- (test2[1,1] + test2[2, 2])/sum(test2)
Accuracy2




install.packages("kohonen")
library('kohonen')
set.seed(20)

data3 <- Boston[2:14]
data3_1 <- Boston[1:1]
table(data3_1)

train <- sample(nrow(data3), 405)
X_train <- scale(data3[train,])
X_test <- scale(data3[-train,],
                center = attr(X_train, "scaled:center"),
                scale = attr(X_train, "scaled:center"))
train_data <- list(measurements = X_train,
                   data3_1 = data3_1[train,])
test_data <- list(measurements = X_test,
                  data3_1 = data3_1[-train,])

mygrid <- somgrid(5, 5, 'hexagonal')
som.data3 <- supersom(train_data, grid = mygrid)
som.predict <- predict(som.data3, newdata = test_data)


test3 <- table(data3_1[-train,], som.predict$predictions$data3_1)

sum(test3[1,])
sum(test3[2,])
test3

#Îöåíèì òî÷íîñòü ìîäåëè
Accuracy3 <- (test3[1,1] + test3[2, 2])/sum(test3)
Accuracy3
