library(MASS)
library(neuralnet)
data <- read.csv(file = "Boston.csv", header = TRUE)
set.seed(20)
apply(data,2,function(x) sum(is.na(x)))
index <- sample(1:nrow(data),round(0.80*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(medv~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)
maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)
scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))
train_ <- scaled[index,]
test_ <- scaled[-index,]
n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)
plot(nn)
predicted <- compute(nn, test_[2:15])
print(head(predicted$net.result))
predicted$net.result <- sapply(predicted$net.result, round, digits = 0)


