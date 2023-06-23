CalcBayesStd <- function(a, b, trial=NULL) {
    ### Find the varaince an arm given its non-zero (alpha) and zero outcomes (beta) ###
    
    # Set to a high value if this arm hasn't been tried before 
    # beta_std <- 1
    
    # Set a constraint on gamma/decay so the alpha or beta values never go 
    # below 0  
    # ## Check after decay and force alpha/beta to 1  
    # 
    # 
    # # Evaluate as long as denominator won't go to 0 - otherwise uncertainty is equal 
    # # and goes to 0  (this can have numerical precision issues due to decay, so treat v low value as effectively 0)
    # if (a+b > 0) {
    #     # If a and b have both decayed to tiny values then treat info about them as lost, so keep 
    #     # at the high uncertainty level..
    #    if (abs(a) + abs(b) > 1e-3) {
           # .. whereas if both of these conds are met, calculate the beta_std  
       # }
    #}
    
    beta_std <- sqrt(
        # Variance 
        a*b/((a + b)^2 * (a + b + 1))   
    )
        
    browser(expr=is.nan(beta_std))
beta_std           
}
