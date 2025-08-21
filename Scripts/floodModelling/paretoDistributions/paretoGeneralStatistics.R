# Introducing functions relevant to the fitting of a truncated Pareto distributions

# A function to evaluate the R_{k,n} statistic

statisticR <- function(randomSample, k){
  if( ! k %in% (0:length(randomSample))){
    print("R statistic - Out of bounds k parameter")
    return(NA)
  }
  else{
    n <- length(randomSample)
    r <- (sort(randomSample)[(n-k)]) / (sort(randomSample)[n])
    if ( ! (r <= 1 & r > 0 )){
      print("ISSUE 1")
    }
    return(r)
  }
}


# A function to evaluate the Hill statistic H_{k,n}

statisticHill <- function(randomSample, k) {
  if( ! k %in% (0:length(randomSample))){
    print("Out of bounds k parameter")
    return(NA)
  }
  else{
    n <- length(randomSample)
    truncatedSortedSample <- sort(randomSample)[(n-k+1):n]
    h <- (1/k) * sum(log(truncatedSortedSample) - log(sort(randomSample)[(n-k)]))
    return(h)
  }
}


# A function evaluating the value of the Newton-Raphson iteration function used un the estimation of the shape parameter alpha of a truncated Pareto distribution 

newtonRaphsonFunction <- function(x, H, R) {
  RPower <- R^(x^(-1))
  result <- H - x - ((log(R)*RPower)/(1-RPower))
  return(result)
}


# A function evaluating the value of the derivative of the Newton-Raphson iteration function used un the estimation of the shape parameter alpha of a truncated Pareto distribution 

newtonRaphsonFunctionDerivative <- function(x, H, R) {
  RPower <- R^(x^(-1))
  result <- - 1 + ((log(R)^2*RPower)/((1-RPower)^2*x^2))
  return(result)
}


# A function proceeding with a step of the Newton-Raphson iteration used un the estimation of the shape parameter alpha of a truncated Pareto distribution 

newtonRaphsonStepper <- function(currentStep, H, R) {
  nextStep <- currentStep - (newtonRaphsonFunction(currentStep,H,R)/newtonRaphsonFunctionDerivative(currentStep,H,R))
  return(nextStep)
}


# A function proceeding with the Newton-Raphson iteration used un the estimation of the shape parameter alpha of a truncated Pareto distribution for a given number of steps

newtonRaphsonIteration <- function(numberOfSteps, initialGuess, H, R) {
  steps <- c(initialGuess)
  currentStep <- initialGuess
  
  for (i in 1:numberOfSteps) {
    currentStep <- newtonRaphsonStepper(currentStep, H, R)
    steps <- c(steps, currentStep) 
  }
  
  iterationsData <- data.frame(index = 1:length(steps) - 1, steps = steps)
  return(currentStep)
}


# A function estimating the shape parameter alpha of a truncated Pareto distribution

estimatorAlpha <- function(numberOfSteps, H, R) {
  iterationResult <- newtonRaphsonIteration(numberOfSteps, H, H, R) 
  if (iterationResult == 0) {
    return(NA)
  }
  alphaEstimator <- 1/iterationResult
  return(alphaEstimator)
}


# A function estimating the lower parameter tau of a truncated Pareto distribution (corresponding to the MLE)

estimatorTau <- function(randomSample, k, alpha) {
  n <- length(randomSample)
  estimator <- ((k/(n - (n-k)*(sort(randomSample)[n-k]/sort(randomSample)[n])^alpha))^(1/alpha))*(sort(randomSample)[n-k])
  if (is.nan(estimator)) {
    return(NA)
  }
  return(estimator)
}


# A function estimating the odds ratio D of a truncated Pareto distribution

estimatorD <- function(randomSample, k, alpha, R) {
  n <- length(randomSample)
  RAlpha <- R^alpha
  D <- ((k+1)*(RAlpha - (1/(k+1)))) / ((n+1)*(1 - RAlpha))
  # D <- max(0, D) # Admissibility (under our assumptions)
  return(D)
}


# A function selecting k* the number of considered sample values yielding the highest absolute correlation between the observed sample and the fitted model 

selector_kStar <- function(randomSample, estimationData, minimal_kStar) {
  
  print(" ")
  print("Selection of kStar: ")
  
  n <- length(randomSample)
  correlations <- c()
  
  progress <- txtProgressBar(min = minimal_kStar, max = n-1, style = 3)
  
  for (k in minimal_kStar:(n-1)) {
    Dk <- estimationData$D[k]
    data <- truncatedParetoQQPlotData(randomSample, Dk)
    
    datax <- data$x
    # print("x before")
    # print(datax)
    datax <- datax[1:k]
    # print("x after")
    # print(datax)
    
    datay <- data$y
    # print("y before")
    # print(datay)
    datay <- datay[1:k]
    # print("y after")
    # print(datay)
    # print("break")
    
    currentCorrelation <- cor(datax, datay)
    correlations <- c(correlations, currentCorrelation)
    
    setTxtProgressBar(progress, k)
  }
  
  selectionData <- data_frame(
    k = minimal_kStar:(n-1), 
    cor = correlations
  )
  
  kStar <- selectionData$k[which.max(abs(selectionData$cor))]
  
  print(" ")
  print(paste0("Selected kStar: ", kStar))
  
  cor <- selectionData$cor
  
  if (minimal_kStar > 2) {
    cor <- c(NA*1:(minimal_kStar-1), selectionData$cor)
  }
  
  result <- list(kStar = kStar, cor = cor)
  return(result)
}


# A function proceeding to the statistical test deciding between a Pareto distribution (H_0) and a truncated Pareto distribution (H_1)

truncationTestA <- function(randomSample, k, R, H) {
  T_Akn <- k*R^(1/H)
  qLevel <- exp(T_Akn)^(-1)
  pValue <- exp(-T_Akn)
  result <- list(qLevel = qLevel, pValue = pValue)
  return(result)
}


# A function evaluating the E_{k,n}(\alpha) statistic

statisticE <- function(randomSample, k, alpha) {
  n <- length(randomSample)
  E <- 0 
  for (j in 1:k) {
    E <- E + ((sort(randomSample)[n-k])/(sort(randomSample)[n-j+1]))^alpha
  }
  E <- E/k
  return(E)
}


# A function proceeding to the statistical test deciding between a light (H_0) or a rough (H_1) truncation of the Pareto distribution

truncationTestB <- function(randomSample, k, H) {
  alpha <- 1/H
  E <- statisticE(randomSample, k, alpha)
  L <- (E - 1/2)/(1 - E)
  T_Bkn <- sqrt(12*k)*L
  qLevel <- 1 - pnorm(-T_Bkn,0,1)
  pValue <- pnorm(T_Bkn,0,1)
  result <- list(qLevel = qLevel, pValue = pValue)
  return(result)
}


# A function estimating the extreme quantile q_p at level p for a truncated Pareto distribution

estimatorQ <- function(randomSample, k, alpha, D, p, isRough) {
  n <-length(randomSample)
  estLoqQ <- log(sort(randomSample)[n-k]) + log((D + ((k+1)/(n+1)))/(D + p))/alpha
  q <-exp(estLoqQ)
  return(q)
}


# An alternative function estimating the extreme quantile q_p at level p for a truncated Pareto distribution

estimatorQAlternative <- function(randomSample, k, alpha, p) {
  n <-length(randomSample)
  q <- (sort(randomSample)[n-k])*((k+1)/((n+1)*p))^(1/alpha)
  return(q)
}


# A function estimating the upper parameter T of a truncated Pareto distribution

estimatorT <- function(q_limit, maximalObservation, allowingInfiniteT) {
  ln_T <- max( log(q_limit) , log(maximalObservation) )
  estimatorT <- exp(ln_T)
  if (allowingInfiniteT | (estimatorT < Inf)) {
    return(estimatorT)
  }
  return(maximalObservation)
}