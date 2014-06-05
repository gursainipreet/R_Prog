myfunction <- function(x){
  y<- rnorm(100)
  mean(y)
}

Second <- function(x){
  x+rnorm(length(x))
}