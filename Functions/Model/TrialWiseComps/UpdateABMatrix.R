UpdateABMatrix <- function(ab_mat, state, reward, debug_info=NULL, alpha_free=NULL, beta_free=NULL) {
### Update alpha or beta in the appropriate state row, based 
# on whether there was a non-zero outcome ###
    # cat("\n\n *TRIAL* =", debug_info)
    # cat("\n State =", state)
    if (is.null(alpha_free)) alpha_free <- 1
    if (is.null(beta_free)) beta_free <- 1
    # If a non-zero outcome, update alpha...
    #if (debug_info == 30) browser()
    if (reward != 0) {
        #cat("\n Alpha update")
        # Spot check 11/29 that alpha is being updated both for punishments (states: 1:2; reward = -1) and reward (states 3:4; reward = 1)#browser(expr=state==4)
        ab_mat[state, 1] <- ab_mat[state, 1] + alpha_free
    # ... otherwise update beta as the number of pulls minus the number of non-zero rewards 
    } else {
        #cat("\n Beta update")
        ab_mat[state, 2] <- ab_mat[state, 2] + beta_free
    }
    
ab_mat
}

