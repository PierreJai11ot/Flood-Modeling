# Introducing a function to generate a moving average 

movingAverage <- function(dataSet, currentIndexValue, radius) {
  result <- 0
  if((currentIndexValue >= min(dataSet$Year) + radius)){
    for(i in (-radius + 1):0 ){
      value <- sum(dataSet$Year== (currentIndexValue + i))
      result <- result + value
    }
    result <- result/(radius)
  }
  else{
    return(NA)
  }
}