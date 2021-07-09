library(tidyverse)
library(dslabs)

if(!exists("mnist")) mnist <- read_mnist()
set.seed(0, sample.kind = "Rounding") # if using R 3.6 or later
ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)

#the predictors are in x and the labels in y
x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]

y[1:3]

x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

#distance between two numbers
sqrt(sum((x_1 - x_2)^2))
sqrt(sum((x_1 - x_3)^2))
sqrt(sum((x_2 - x_3)^2))

#compute distance using matrix algebra
sqrt(crossprod(x_1 - x_2))
sqrt(crossprod(x_1 - x_3))
sqrt(crossprod(x_2 - x_3))

#compute distance between each row
d <- dist(x)
class(d)
as.matrix(d)[1:3,1:3]

#visualize these distances
image(as.matrix(d))

#order the distance by labels
image(as.matrix(d)[order(y), order(y)])

#compute distance between predictors
d <- dist(t(x))
dim(as.matrix(d))

d_492 <- as.matrix(d)[492,]

image(1:28, 1:28, matrix(d_492, 28, 28))


library(dslabs)
data(tissue_gene_expression)
dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)
d <- dist(tissue_gene_expression$x)
as.matrix(d)[39,74]
ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

image(as.matrix(d))


library(tidyverse)
library(dslabs)
data("mnist_27")
mnist_27$test %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()

#logistic regression
library(caret)
fit_glm <- glm(y~x_1+x_2, data=mnist_27$train, family="binomial")
p_hat_logistic <- predict(fit_glm, mnist_27$test)
y_hat_logistic <- factor(ifelse(p_hat_logistic > 0.5, 7, 2))
confusionMatrix(data = y_hat_logistic, reference = mnist_27$test$y)$overall[1]

#fit knn model
knn_fit <- knn3(y ~ ., data = mnist_27$train)

x <- as.matrix(mnist_27$train[,2:3])
y <- mnist_27$train$y
knn_fit <- knn3(x, y)

knn_fit <- knn3(y ~ ., data = mnist_27$train, k=5)

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")  # class here means to return the most likely prediction (2 or 7) rather than a probability
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]


y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class") 
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")  
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

#fit knn with k=1
knn_fit_1 <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn_1 <- predict(knn_fit_1, mnist_27$train, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$train$y)$overall[["Accuracy"]]

y_hat_knn_1 <- predict(knn_fit_1, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_1, reference=mnist_27$test$y)$overall[["Accuracy"]]

#fit knn with k=401
knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall["Accuracy"]

#pick the k in knn
ks <- seq(3, 251, 2)
library(purrr)
accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
  
})

#pick the k that maximizes accuracy using the estimates built on the test data
ks[which.max(accuracy$test)]
max(accuracy$test)



set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
library(caret)
library(dslabs)
heights
test_index <- createDataPartition(heights$sex, 1, 0.5, list = FALSE)
train <- heights[-test_index,]
test <- heights[test_index,]
y <- train$sex
x <- train$height
ks <- seq(1, 101, 3)
F_1 <- sapply(ks, function(k){
  knn_fit <- knn3(sex ~ height, data = train, k = k)
  y_hat <- predict(knn_fit, newdata = test, type = "class")
  F_meas(data = y_hat, reference = test$sex)
})

plot(ks, F_1)
max(F_1)
ks[which.max(F_1)]



library(dslabs)
library(caret)
data("tissue_gene_expression")

# My code
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
class(tissue_gene_expression$x)
test_index <- createDataPartition(tissue_gene_expression$y, 1, 0.5, list = FALSE)
train_x <- tissue_gene_expression$x[-test_index,]
train_y <- tissue_gene_expression$y[-test_index]
test_x <- tissue_gene_expression$x[test_index,]
test_y <- tissue_gene_expression$y[test_index]
train <- as.data.frame(train_x)
train$y = train_y
test <- as.data.frame(test_x)
test$y = test_y
levels(y_hat) = levels(test$y)

k=seq(1,11,2)
sapply(k, function(k){
  knn_fit <- knn3(train_x, train_y, k = k)
  y_hat <- predict(knn_fit, newdata = data.frame(x = test_x), type = "class")
  mean(y_hat == test$y)
  })

# course code
set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
test_index <- createDataPartition(y, list = FALSE)
sapply(seq(1, 11, 2), function(k){
  fit <- knn3(x[-test_index,], y[-test_index], k = k)
  y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                   type = "class")
  mean(y_hat == y[test_index])
})

