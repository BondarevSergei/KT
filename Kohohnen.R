
install.packages("kohonen")
library('kohonen')
data <- read.csv(file = "Boston.csv", header = TRUE)
str(data)
head(data)
set.seed(20)
     VarName = c('indus', 'dis', 'nox',  'medv', 'lstat', 'age', 'rad')
     data_train <- data[, VarName]
     data_train_matrix <- as.matrix(scale(data_train))

som_grid <- somgrid(xdim = 9, ydim = 6, topo = "hexagonal") 
som_model <- som(data_train_matrix, grid = som_grid, rlen = 100,
                 alpha = c(0.05,0.01), keep.data = TRUE)
plot(som_model, type = "changes")
# Çàäàäèì ïàëèòðó öâåòîâ
coolBlueHotRed <- function(n, alpha = 1) {
  rainbow(n, end = 4/6, alpha = alpha)[n:1] 
}
par(mfrow = c(2, 1))

plot(som_model, type = "counts", palette.name = coolBlueHotRed)

plot(som_model, type = "quality", palette.name = coolBlueHotRed)


train <- sample(nrow(data), 15)
X_train <- scale(data[train,])
X_test <- scale(data[-train,],
                center = attr(X_train, "scaled:center"),
                scale = attr(X_train, "scaled:center"))
train_data <- list(measurements = X_train,
                   data = data[train])
test_data <- list(measurements = X_test,
                  data = data[-train])

mygrid <- somgrid(5, 5, 'hexagonal')

som.data <- supersom(train_data, grid = mygrid)                

som.predict <- predict(som.data, newdata = test_data)
table(data[-train], som.predict$predictions[['Boston']])
map(som.data)

#ãðàôèê äëÿ îáó÷åííîé íåéðîííîé ñåòè
plot(som.data, main = 'Boston data Kohonen SOM')
