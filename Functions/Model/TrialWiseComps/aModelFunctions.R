############# # Set up some learning and choice functions  ###########
RunSoftMax <- function(action_values, beta=50) {
  ### Runs softmax choice function and returns a list of probabilities ###
  
  num_1 <- exp(beta*action_values[1])
  num_2 <- exp(beta*action_values[2])
  denom <- sum(exp(beta*action_values[1]), exp(beta*action_values[2]))
  
  prob_val1 <- num_1/denom
  prob_val2 <- num_2/denom 
  
list(prob_val1, prob_val2)  
}

RunLSESoftmax <- function(action_values, # vector to apply softmax to 
                          beta=50,
                          debug_info=NULL,
                          verbose=0, # for printouts
                          identifier='default' # optional ident for debugging on spec SM calls
) {
  
  ### Performs softmax using log sum exp trick. Returns softmax(vec) ###
  # Pull out the index of the max term, breaking ties by just taking first
  beta <- as.numeric(beta)
  #print(beta)
  mi <- which(action_values == max(action_values))[1]
  
  # Demonominator (constant)
  term2 <- beta * action_values[mi] + # max term 
    log(sum(exp(beta * action_values - beta * action_values[mi])))
  
  # Preallocate vector of softmax outputs
  sm_vec <- rep(NA, length(action_values))
  # Numerators
  term1_vec <- beta * action_values
  
  # Calc softmax for each elem 
  for (i in seq_along(action_values)) sm_vec[i] <- term1_vec[i] - term2
  
  # Break likelihood in case of over/underflow
  if (any(sm_vec > 0)) {
    
    sm_vec <- rep(NA, 3)
  }
  if (verbose) cat('\n Log liks', sm_vec)
  
exp(sm_vec) # Return as probabilities  
}


RunLSESoftmaxWithAltStochasticCK <- function(action_values, # vector to apply softmax to
                                             beta=50,
                                             beta_ck,
                                             choice_kernel=c(0, 0),
                                             debug_info=NULL,
                                             verbose=0, # for printouts
                                             identifier='default' # optional ident for debugging on spec SM calls
) {
  
  ### Adds bias and choice kernel to action values then performs softmax using log sum exp trick. Returns softmax(vec) ###
  
  beta <- as.numeric(beta)
  
  mi <- which(action_values == max(action_values))[1]
  
  mi_ck <- which(choice_kernel == max(choice_kernel))[1]
  
  max_exp_term <- beta * action_values[mi] + beta_ck * choice_kernel[mi_ck]
  
  # Demonominator
  term2 <- max_exp_term + # max term
    log(sum(exp( (beta * action_values + beta_ck * choice_kernel) -
                   max_exp_term)))
  
  # Preallocate vector of softmax outputs
  sm_vec <- rep(NA, length(action_values))
  # Numerators
  term1_vec <- beta * action_values + beta_ck * choice_kernel
  
  # Calc softmax for each elem
  for (i in seq_along(action_values)) sm_vec[i] <- term1_vec[i] - term2
  
  # Break likelihood in case of over/underflow
  if (any(sm_vec > 0)) {
    sm_vec <- rep(NA, 3)
  }
  
  if (verbose) cat('\n Log liks', sm_vec)
  
  choice_probs <- exp(sm_vec) # Return as probabilities
  #browser(expr=is.nan(choice_probs))
choice_probs
}

RunSoftMaxWStochCK <- function(action_values, 
                               beta=50, 
                               beta_ck=50, 
                               choice_kernel=c(0, 0), 
                               tr=1) {
  ### Non LSE version with stochastic CK for testing (don't use for actual optimization bc may overflow) ### 
  
  term_1 <- exp(sum(beta*action_values[1], beta_ck*choice_kernel[1]))
  term_2 <- exp(sum(beta*action_values[2] + beta_ck*choice_kernel[2]))
  
  denom <- sum(term_1, term_2)
  
  prob_val1 <- term_1/denom
  prob_val2 <- term_2/denom 
  
  choice_probs <- c(prob_val1, prob_val2)  
  
choice_probs
}

RunLSESoftmaxWithBiasAndCK <- function(action_values, # vector to apply softmax to 
                                       beta=50,
                                       bias=0,
                                       choice_kernel=c(0, 0),
                                       debug_info=NULL,
                                       verbose=0, # for printouts
                                       identifier='default' # optional ident for debugging on spec SM calls
) {
  ### Adds bias and choice kernel to action values then performs softmax using log sum exp trick. Returns softmax(vec) ###
  
  values_plus_bias_bonus <- c( action_values[1] + choice_kernel[1], action_values[2] + choice_kernel[2] + bias )
  
  beta <- as.numeric(beta)
  
  mi <- which(values_plus_bias_bonus == max(values_plus_bias_bonus))[1]
  
  # Demonominator (constant)
  term2 <- beta * values_plus_bias_bonus[mi] + # max term 
    log(sum(exp(beta * values_plus_bias_bonus - beta * values_plus_bias_bonus[mi])))
  
  # Preallocate vector of softmax outputs
  sm_vec <- rep(NA, length(values_plus_bias_bonus))
  # Numerators
  term1_vec <- beta * values_plus_bias_bonus
  
  # Calc softmax for each elem 
  for (i in seq_along(values_plus_bias_bonus)) sm_vec[i] <- term1_vec[i] - term2
  
  # Break likelihood in case of over/underflow
  if (any(sm_vec > 0)) {
    sm_vec <- rep(NA, 3)
  }
  
  if (verbose) cat('\n Log liks', sm_vec)
  
  choice_probs <- exp(sm_vec) # Return as probabilities  
  #browser(expr=is.nan(choice_probs))
choice_probs  
}

RunLSESoftmaxDynInvTempCK <- function(
  action_values, 
  beta=50,
  choice_kernel=c(0, 0),
  mean_uncertainty=0,
  rho=0,
  beta_low=3,
  debug_info=NULL,
  verbose=0, # for printouts
  identifier='default' # optional ident for debugging on spec SM calls
) {
  ### Adds bias to action values then performs softmax using log sum exp trick. Inverse temp beta is modulated by 
  # the amount of uncertainty #
  # Returns softmax(vec) ###
  
  values_plus_ck <- c( action_values[1] + choice_kernel[1], action_values[2] + choice_kernel[2] )
  
  # Create a dynamic beta that can go as low as 1/2 beta when uncertainty is highest  
  beta <- as.numeric(beta)
  
  beta_min <- beta/beta_low 
  
  max_unc <- CalcBayesStd(1, 1)
  
  dyn_beta <- (1-mean_uncertainty/max_unc)*beta + mean_uncertainty/max_unc*beta_min
  
  # To allow this to drop out of the model, create an effective beta where beta never changes based on 
  # uncertainty when this goes to 0  
  beta_eff <- rho*dyn_beta + (1-rho)*beta
  
  mi <- which(values_plus_ck == max(values_plus_ck))[1]
  
  # Demonominator (constant)
  term2 <- beta_eff * values_plus_ck[mi] + # max term 
    log(sum(exp(beta_eff * values_plus_ck - beta_eff * values_plus_ck[mi])))
  
  # Preallocate vector of softmax outputs
  sm_vec <- rep(NA, length(values_plus_ck))
  
  # Numerators
  term1_vec <- beta_eff * values_plus_ck
  
  # Calc softmax for each elem 
  for (i in seq_along(values_plus_ck)) sm_vec[i] <- term1_vec[i] - term2
  
  # Break likelihood in case of over/underflow
  if (any(sm_vec > 0)) {
    browser()
    sm_vec <- rep(NA, 3)
  }
  if (verbose) cat('\n Log liks', sm_vec)
  
  choice_probs <- exp(sm_vec) # Return as probabilities  
  
  #browser(expr=is.nan(choice_probs))
choice_probs  
}

CalcQVals <- function(Q_vals, q_LR, action, reward, verbose, 
                      store_q_RPE=NULL, use_V_S=NULL, V_S=NULL, debug_info=NULL) {
  ### Calc Q vals tracking full reward info. Args: sidx=state index 
  # Optional arg to update V_Q - a Q-learner state value ###
  
  qPE <- reward - Q_vals[action]
  #cat("\nQ(s,a) PE"); print(qPE)
  Q_vals[action] <- Q_vals[action] + q_LR * qPE
  
  # Optionally store Q val RPE 
  if (!is.null(store_q_RPE)){
    out <- list("q_RPE"=qPE, "q_vals"=Q_vals)
  } else {
    out <- Q_vals
  }
  
  # Optionally update a state value 
  if (!is.null(use_V_S)) {
    
    out <- NULL
    V_PE <- reward - V_S
    
    V_S <- V_S + q_LR * V_PE
    out <- list("V_S"=V_S, "q_vals"=Q_vals)
    
    if (!is.null(store_q_RPE)) out$q_RPE <- qPE
    
  } 
  
out
}

CalcQValsSpreadCA <- function(CA, Q_vals, q_LR, action, reward, verbose, 
                              store_q_RPE=NULL, use_V_S=NULL, V_S=NULL, debug_info=NULL) {
  ### Calc Q vals but with some shared assignment to irrelevant action ###
  
  qPE <- reward - Q_vals[action]
  
  
  Q_vals[action] <- Q_vals[action] + q_LR * qPE * (1-CA)
  Q_vals[-action] <- Q_vals[-action] + q_LR * qPE * CA
  
  # Optionally store Q val RPE 
  if (!is.null(store_q_RPE)){
    out <- list("q_RPE"=qPE, "q_vals"=Q_vals)
  } else {
    out <- Q_vals
  }
  
  # Optionally update a state value 
  if (!is.null(use_V_S)) {
    
    out <- NULL
    V_PE <- reward - V_S
    
    V_S <- V_S + q_LR * V_PE
    out <- list("V_S"=V_S, "q_vals"=Q_vals)
    
    if (!is.null(store_q_RPE)) out$q_RPE <- qPE
    
  } 
  
out
}

CalcQValsValenced <- function(Q_vals, q_LR_neg, q_LR_pos, action, reward, verbose, store_q_RPE=1, V_Q_of_S=NULL, debug_info=NULL) {
  ### Calc Q vals tracking full reward info. Args: sidx=state index 
  # Optional arg to update V_Q - a Q-learner state value ###
  
  qPE <- reward - Q_vals[action]
  #cat("\nQ(s,a) PE"); print(qPE)
  #browser(expr=debug_info==97)
  if (qPE >= 0) {
    Q_vals[action] <- Q_vals[action] + q_LR_pos * qPE  
  } else {
    Q_vals[action] <- Q_vals[action] + q_LR_neg * qPE  
  }
  
  
  # Optionally use the Q RPE to update a V_Q state value 
  if (!is.null(V_Q_of_S)){
    
    q_V_PE <- reward - V_Q_of_S
    V_Q_of_S <- V_Q_of_S + q_LR * q_V_PE
    out <- list("V_Q_S"=V_Q_of_S, "q_vals"=Q_vals)
    
  } else {
    out <- Q_vals
  }
  
  # Optionally store Q val RPE 
  if (!is.null(store_q_RPE)){
    out <- list("q_RPE"=qPE, "q_vals"=Q_vals)
  } else {
    out <- Q_vals
  }
  
  out
  
out
}


SimReward <- function(val, prob, correct, verbose=NULL) {
  ### Simulate a reward based on the trial contingencies and whether the response was correct ###
  
  # Random prob 
  rdr <- runif(1, 0, 1)
  
  if (correct) {
    
    if (val=="reward") {
      
      if (prob == "90-10") { # Correct, 90% chance reward 
        reward <- if_else(rdr < .9, 1, 0)
      } else { # Correct, 40% chance reward 
        reward <- if_else(rdr < .4, 1, 0)
      }
      
    } else { # {Punishment, correct} — 10% chance pun
      reward <- if_else(rdr < .1, -1, 0) 
    }
    
  } else { # Incorrect 
    
    if (val=="reward") { # {Reward, incorrect} — 10% chance reward 
      reward <- if_else(rdr < .1, 1, 0) 
      
    } else { # Incorrect, punishment 
      if (prob == "90-10") { # Incorrect, 90% chance punishment
        reward <- if_else(rdr < .9, -1, 0)
      } else { # Incorrect, 40% chance punishment
        reward <- if_else(rdr < .4, -1, 0)
      }
      
    }
    
  } # End correct/incorrect conditional 
  
reward  
}