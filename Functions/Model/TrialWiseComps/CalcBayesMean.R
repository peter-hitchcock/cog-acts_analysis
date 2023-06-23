CalcBayesMean <- function(a, b, valence, trial=NULL) {
  ### Calculate Bayes mean and sign as negative if punishment ###
  bayes_mean <- 0
  if (a > 0) {
    
    bayes_mean_unsigned <- a/(a+b)
    bayes_mean <- if_else(valence=="reward", bayes_mean_unsigned, -bayes_mean_unsigned)  
  }
  
bayes_mean
}