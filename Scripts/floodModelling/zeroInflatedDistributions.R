# Implementing basic functions for Zero Inflated Poisson random variables

dzipois <- function(x, lambda, pi) {
  zero_vector <- (x==0)
  pois_vector <- dpois(x, lambda)
  return((pi*zero_vector) + ((1-pi)*pois_vector))
}


pzipois <- function(q, lambda, pi) {
  zero_vector <- (q>=0)
  pois_vector <- ppois(q, lambda)
  return((pi*zero_vector)+((1-pi)*pois_vector))
}


qzipois <- function(p, lambda, pi) {
  sapply(p, function(prob) {
    x <- 0
    while (pzipois(x, lambda, pi) < prob) {
      x <- x + 1
    }
    return(x)
  })
}


rzipois <- function(n, lambda, pi) {
  zero_vector <- rbinom(n, 1, pi)
  pois_vector <- rpois(n, lambda)
  return((1-zero_vector)*pois_vector)
}


# Implementing a function to evaluate the log likelihood of a Zero Inflated Poisson model 

logLikelihoodZipois <- function(sample, parameters) {
  lambda <- parameters[1]
  pi <- parameters[2]
  if (lambda < 0 | pi < 0 | pi > 1) {
    return(-Inf)
  }
  return(sum(log(dzipois(sample, lambda, pi))))
}


# Implementing functions for the estimation of the parameters of a Zero Inflated Poisson model

# Using the method of moments estimators

momZipois <- function(sample) {
  mean <- mean(sample)
  var <- var(sample)
  lambda <- -1 + (var + mean^2)/mean
  pi <- (var - mean)/(var + mean^2 - mean)
  return(c(lambda = lambda, pi = pi))
}
  

# Using maximum likelihood estimators

mleZipois <- function(sample) { 
  minLambda <- 0.001
  minPi <- 0.001
  maxPi <- 0.999
  startParameters <- momZipois(sample)
  startParameters <- c(lambda = max(minLambda, startParameters[1]), pi = min(maxPi, max(startParameters[2], minPi)))
  fit <- maxLik(logLik = function(parameters) logLikelihoodZipois(sample, parameters),
                start = startParameters)
  return(fit)
}


# Implementing a function that fits a Zero Inflated Poisson model

zipoisFitter <- function(sample){
  
  # Estimating the parameters
  
  mleFit <- mleZipois(sample)
  
  # Retrieving relevant statistics
  
  n <- length(sample)
  nParams <- length(coef(mleFit))
  logLik <- as.numeric(logLik(mleFit))
  
  # Computing additional statistics 
  
  AIC <- -2 * logLik + 2 * nParams
  BIC <- -2 * logLik + log(n) * nParams
  
  fitdist <- list(fit = mleFit,
    coefficients = coef(mleFit),
    logLik = logLik,
    AIC = AIC,
    BIC = BIC,
    sampleSize = n
  )
  
  return(fitdist)
}


# Implementing basic functions for Zero Inflated Negative Binomial random variables

dzinbinom <- function(x, size, prob, pi) {
  zero_vector <- (x==0)
  nbinom_vector <- dnbinom(x, size, prob)
  return((pi*zero_vector) + ((1-pi)*nbinom_vector))
}


pzinbinom <- function(q, size, prob, pi) {
  zero_vector <- (q>=0)
  nbinom_vector <- pnbinom(q, size, prob)
  return((pi*zero_vector)+((1-pi)*nbinom_vector))
}


qzinbinom <- function(p, size, prob, pi) {
  sapply(p, function(proba) {
    x <- 0
    while (pzinbinom(x, size, prob, pi) < proba) {
      x <- x + 1
    }
    return(x)
  })
}


rzinbinom <- function(n, size, prob, pi) {
  zero_vector <- rbinom(n, 1, pi)
  nbinom_vector <- rnbinom(n, size, prob)
  return((1-zero_vector)*nbinom_vector)
}


# Implementing a function to evaluate the log likelihood of a Zero Inflated Negative Binomial model 

logLikelihoodZinbinom <- function(sample, parameters) {
  size <- parameters[1]
  prob <- parameters[2]
  pi <- parameters[3]
  if (size <= 0 | prob < 0 | prob > 1 | pi < 0 | pi > 1) {
    return(-Inf)
  }
  return(sum(log(dzinbinom(sample, size, prob, pi))))
}


# Implementing functions for the estimation of the parameters of a Zero Inflated Negatve Binomial model

# Using the method of moments estimators arbitrarily supposing that the probability of the zero inflation is 0.5

momZinbinom <- function(sample, piEst = TRUE) {
  error <- 0.001
  if (! piEst ){
    pi <- 0.5
  }
  if (piEst) {
    prStandIn <- 0.1 
    zeroFreq <- sum(sample == 0)/length(sample)
    pi <- (1 - zeroFreq)/(1 - prStandIn)
    pi <- max(error, min(pi, 1- error))
  }
  m <- mean(sample)
  v <- var(sample)
  prob <- 1/(v/m - m*pi/(1-pi))
  size <- prob*m/((1-pi)*(1-prob))
  prob <- max(error, min(prob, 1- error))
  size <- max(1, round(size))
  return(c(size = size, prob = prob, pi = pi))
}


# Using maximum likelihood estimators

mleZinbinom <- function(sample) { 
  minLambda <- 0.01
  startParameters <- momZinbinom(sample)
  fit <- maxLik(logLik = function(parameters) logLikelihoodZinbinom(sample, parameters),
                start = startParameters)
  return(fit)
}


# Implementing a function that fits a Zero Inflated Negatve Binomial model

zinbinomFitter <- function(sample){
  
  # Estimating the parameters
  
  mleFit <- mleZinbinom(sample)
  
  # Retrieving relevant statistics
  
  n <- length(sample)
  nParams <- length(coef(mleFit))
  logLik <- as.numeric(logLik(mleFit))
  
  # Computing additional statistics 
  
  AIC <- -2 * logLik + 2 * nParams
  BIC <- -2 * logLik + log(n) * nParams
  
  fitdist <- list(fit = mleFit,
                  coefficients = coef(mleFit),
                  logLik = logLik,
                  AIC = AIC,
                  BIC = BIC,
                  sampleSize = n
  )
  
  return(fitdist)
}

