
library(seewave)

CalcMAV <- function(x){
  return(mean(abs(x)))
}


CalcZC <- function(x, eps = 0.00005){
  n <- length(x)
  zc <- 0
  for (i in 1:(n-1)){
    if( (x[i] > 0) && (x[i+1] < 0) ){
      if(abs(x[i]-x[i+1]) >= eps){
        zc <- zc + 1
      }
    }
    if( (x[i] < 0) && (x[i+1] > 0) ){
      if(abs(x[i]-x[i+1]) >= eps){
        zc <- zc + 1
      }
    }
  }
  return(zc)
}


CalcSSC <- function(x, eps = 0.00005){
  n <- length(x)
  ssc <- 0
  for (i in 2:(n-1)){
    if( (x[i] > x[i-1]) && (x[i] > x[i+1]) ){
      if( (abs(x[i]-x[i+1]) >= eps)&&(abs(x[i]-x[i-1]) >= eps) ){
        scc <- scc + 1
      }
    }
    if((x[i] < x[i-1]) && (x[i] < x[i+1]) ){
      if( (abs(x[i]-x[i+1]) >= eps)&&(abs(x[i]-x[i-1]) >= eps) ){
        scc <- scc + 1
      }
    }
  }
  return(scc)
}


CalcWL <- function(x){
  n <- length(x)
  WL <- 0
  for(i in 2:n){
    delta <- abs(x[i] - x[i-1])
    WL <- WL + delta
  }
  return(WL)
}
