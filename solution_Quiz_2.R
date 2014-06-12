cube <- function(x, n) {
  x^3
}

c<- function(n){
x <- 1:10
if(x > 5) {
  x <- 0
  x
}
}

z<

f3<- function (x){
  
  x <- 5
  y <- if(x < 3) {
    NA
  } else {
    10
  }
  y
}


above10<- function(x){
  use<-x>10
  x[use]
  
  
}

above <- function(x,n=10) {
  use <- x > n
  x[use]
  
}

colMean <- function(x,removeNA =TRUE){
  nc<-ncol(y)
  means<- numeric(nc)
  for(i in 1:nc)
  {
    mean[i]<- mean(y[,i], na.rm = removeNA)
  }
  means
}
