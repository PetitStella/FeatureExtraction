
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
      if( (abs(x[i]-x[i+1]) >= eps)||(abs(x[i]-x[i-1]) >= eps) ){
        ssc <- ssc + 1
      }
    }
    if((x[i] < x[i-1]) && (x[i] < x[i+1]) ){
      if( (abs(x[i]-x[i+1]) >= eps)||(abs(x[i]-x[i-1]) >= eps) ){
        ssc <- ssc + 1
      }
    }
  }
  return(ssc)
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


splitWithOverlap <- function(vec, seg.length, overlap) {
  starts = seq(1, length(vec), by=seg.length-overlap)
  ends   = starts + seg.length - 1
  ends[ends > length(vec)] = length(vec)
  
  lapply(1:length(starts), function(i) vec[starts[i]:ends[i]])
}

GetHudginsFeatureSet <- function(x, samplingFreq, windowTime = 0.25, overlapTime = 0.125){
  nrows <- NROW(x)
  
  vec <- seq(1,nrows)
  
  seg.length <- samplingFreq*windowTime
  overlap <- samplingFreq*overlapTime
  
  segIndex <- splitWithOverlap(vec, seg.length, overlap)
  
  features <- matrix(0, nrow = length(segIndex), ncol = 12)
  
  for (i in 1:length(segIndex)){
    MAV <- apply(x[segIndex[[i]],], 2, CalcMAV)
    SSC <- apply(x[segIndex[[i]],], 2, CalcSSC)
    WL <- apply(x[segIndex[[i]],], 2, CalcWL)
    ZC <- apply(x[segIndex[[i]],], 2, CalcZC)
    featureSet <- cbind(MAV, SSC, WL, ZC)
    dim(featureSet) <- c(12,1)
    features[i,] <- featureSet
  }
  
  return(features)
}
