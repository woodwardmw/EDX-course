x <- seq(0, 0.1, 0.0001)
x

f <- function(x){
  p <- 0.01
  p * log(x) + (1-p) * log(1-x)
}
y <- lapply(x, f)
y
x[which.max(y)]
plot(x, y)