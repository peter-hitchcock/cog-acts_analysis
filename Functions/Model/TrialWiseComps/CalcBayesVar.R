CalcBayesVar <- function(a, b, trial=NULL) {
    ### Find the variance of an arm given its non-zero (alpha) and zero outcomes (beta) ###
    bayes_var <-   a*b/((a + b)^2 * (a + b + 1))   
     
    browser(expr=is.nan(bayes_var))
    
bayes_var          
}
