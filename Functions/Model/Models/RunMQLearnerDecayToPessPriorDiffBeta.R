RunMQLearnerDecayToPessPriorDiffBeta <- function(parameters, train_df, helpers, SIT_df=NULL, mle_means=NULL, mle_covar=NULL) { 
  ### Q-learner with decay with diff beta ###
  
  # State coding (see recode dfs):
  # 40-10 PUN: 1, 90-10 PUN: 2, 40-10 REW: 3, 90-10 REW: 4

  # Unpack lists by assigning to function environment # 
  list2env(helpers, environment())
  
  if (sim_opt=="opt") { # Hack to allow optim to work
    
    pars <- list()
    if (helpers$run_empirical_bayes == 0) {
      parameter_names <- names(parameters)  
    } else {
      parameter_names <- names(mle_means)
    }
    
    for (i in 1:length(parameter_names)) pars[[parameter_names[[i]]]] <- as.numeric(parameters[i])
    parameters <- pars
  }
  
  list2env(parameters, environment())
  
  # Pull into vectors 
  tr_in_cond  <- train_df$trial_within_condition  
  conds <- train_df$cond
  states <- train_df$state
  ans <- train_df$answer 
  vals <- train_df$valence
  probs <- train_df$probability
  stim_iter <- tr_in_cond
  
  # Index for making decays at the appropriate point (see DecayQVals below)
  idx <- 1 
  
  if (sim_opt == "opt") {
    resp <- train_df$resp
    rews <- train_df$rews 
    train_nll <- rep(NA, length(tr_in_cond))
  }
  
  if (sim_opt == "sim") {
    # Preallocate storers  
    sim_actions <- rep(NA, length(conds))
    sim_corrs <- rep(NA, length(conds))
    sim_rews <- rep(NA, length(conds))  
    sim_chosen_Q_vals <- rep(NA, length(conds))
    sim_unchosen_Q_vals <- rep(NA, length(conds))
    sim_correct_Q_vals <- rep(NA, length(conds))
    sim_incorrect_Q_vals <- rep(NA, length(conds))
    sim_prob_chosen <- rep(NA, length(conds))
    sim_prob_unchosen <- rep(NA, length(conds))
    sim_prob_correct <- rep(NA, length(conds))
    sim_prob_incorrect <- rep(NA, length(conds))
    sim_trial_RPE <- rep(NA, length(conds))
  }
  
  ## Iniitialize separate cognitive and overt Q matrices
  cog_Q_mat <- rbind(matrix(0, 2, 2), matrix(0, 2, 2))
  overt_Q_mat <- rbind(matrix(0, 2, 2), matrix(0, 2, 2))
  # Store initial matrix for decaying to on each trial 
  cog_Q_prior <- rbind(matrix(-1, 2, 2), matrix(0, 2, 2))
  overt_Q_prior <- rbind(matrix(-1, 2, 2), matrix(0, 2, 2))
  
  ## Learning-rate adjusted choice kernel 
  cog_choice_kernel <- matrix(0, 4, 2)  
  overt_choice_kernel <- matrix(0, 4, 2)  
  
  # Counters for overt and cogn 
  cc <- 0
  oc <- 1
  
  for (tr in 1:length(states)) {
    
    ## Store some info for debugging model fxs at various points 
    debug_info <- NULL
    
    ## Get trial-specific info ##
    state <- states[tr]
    cond <- conds[tr]
    
    # Can use this to track updates etc within state (Must comment before Oscar cluster because can't read if_else)
    #train_verbose <- if_else(state == 2, 1, 0)
    
    #### Choose #### 
    # First name the cog/overt vars and param generically ... 
    choice_kernel <- NULL; this_choice_kernel <- NULL; q_vals <- NULL
    beta <- NULL 
    if (cond=="cog") {
      q_vals <- cog_Q_mat[state, ]
      choice_kernel <- cog_choice_kernel
      beta <- cog_beta
    } else {
      choice_kernel <- overt_choice_kernel
      q_vals <- overt_Q_mat[state, ]
      beta <- overt_beta
    }
    
    # ... and define the current choice kernel (can be 0)  
    this_choice_kernel <- choice_kernel[state, ]
    
    # Allow for a burst of stochasticity on the 8 trials after the block break (4 contingencies * 2 conds ) 
    # (otherwise get mis-spec here, because pts perform temporarily worse)
    if (tr_in_cond[tr] == 21) {
      
      beta_scaled_down <- beta * (1-explor_scalar)
      
      directed_choice_probs <- RunLSESoftmaxWithBiasAndCK(
        q_vals, 
        beta=beta_scaled_down, 
        bias=0,
        choice_kernel=this_choice_kernel
      )
      
    } else {
      directed_choice_probs <- RunLSESoftmaxWithBiasAndCK(
        q_vals, 
        beta=beta, 
        bias=0,
        choice_kernel=this_choice_kernel
      )  
    }
    
    # Add undirected choice probs : 1 / n_actions = (.5, .5)
    choice_probs <- epsilon * c(.5, .5) + (1-epsilon) * directed_choice_probs
    
    if (train_verbose) {
      cat("\n\n\n\n### TRIAL *Training* ### \n", tr, "\n State", state, "\nCondition ", cond)
      cat("\nQ-values before update", q_vals, "\nCurrent choice kernel before update", this_choice_kernel,
          "\nBeta", beta, "\nDirected choice probabilities", directed_choice_probs, 
          "\nEpsilon", epsilon, "\nChoice probs after epsilon", choice_probs)
    }
    
    # Stop the code if choice probs is not valid  
    assert(sum(choice_probs) < 1.001)
    assert(sum(choice_probs) > .999)
    
    ## Get what we need for learning for sim vs. opt ##
    if (sim_opt=="sim") {
      
      rd <- runif(1, 0, 1)
      
      action <- if_else(rd < choice_probs[1], 1, 2) # 1 = bottom/diff, 2 = top/sum
      correct <- if_else(action == ans[tr], 1, 0)
      reward <- SimReward(vals[tr], probs[tr], correct)
      
      # Store the trial q value info for the trial before the update 
      sim_chosen_Q_vals[tr] <- as.numeric(q_vals[action])
      sim_unchosen_Q_vals[tr] <- as.numeric(q_vals[-action])
      
      sim_correct_Q_vals[tr] <- as.numeric(q_vals[ans[tr]])
      sim_incorrect_Q_vals[tr] <- as.numeric(q_vals[-ans[tr]])
      
      sim_prob_chosen[tr] <- as.numeric(choice_probs[action])
      sim_prob_unchosen[tr] <- as.numeric(choice_probs[-action])
      
      sim_prob_correct[tr] <- as.numeric(choice_probs[ans[tr]])
      sim_prob_incorrect[tr] <- as.numeric(choice_probs[-ans[tr]])
      
      if (cond == "cog") {
        cc <- cc + 1
      } else {
        oc <- oc + 1
      }
      
    } else if (sim_opt == "opt") {
      action <- resp[tr]
      reward <- rews[tr]
      nll_chosen <- -log(choice_probs[action])
    }
    
    #### Adjust q-values Q-values #### 
    q_vals_list <- CalcQVals(q_vals, q_LR, action, reward, train_verbose, store_q_RPE=1)
    
    q_vals <- NULL; q_RPE <- NULL
    
    q_vals <- q_vals_list$q_vals 
    q_RPE <- q_vals_list$q_RPE
    
    if (train_verbose) {
      cat("\nAction", action, "\nReward", reward,  "\nLearning rate", q_LR, 
          "\nRPE",  q_RPE,  "\nQ-values after update", q_vals, "\nPhi", phi)
    }
    
    # For use in settig the prior, set a running index indicating whether we are in the first or second condition 
    if (tr > 1) { # So the one-look back doesn't break.. 
      # If we're in the trial where the last tr_in_cond was 40 and this one is 1, then we're at the condition break, to set the idx to tr   
      if (tr_in_cond[tr-1]==40 & tr_in_cond[tr]==1) { idx <- tr } 
    }
    
    # Update the cq vals by putting into matrix 
    if (cond == "cog") {
      cog_Q_mat[state, ] <- q_vals
      
      # On the first trial within condition just decay to 0 
      if (tr == 1 ) {
        this_cog_Q_prior <- matrix(0, 4, 2)
      } else if  (tr == 161) {
        this_cog_Q_prior <- matrix(0, 4, 2)
      } else {
        # Once they have experienced a state before, can decay to an informed prior value (e.g., -1 for pessimistic prior about punishment)..
        unexperienced_states <- setdiff(unique(states), unique(states[idx:tr]))
        
        this_cog_Q_prior <- cog_Q_prior 
        # .. but for unexperienced states, set the prior to 0 because it should just decay to uniformed ie. 0
        if (length(unexperienced_states) > 0) {
          this_cog_Q_prior[unexperienced_states, ] <- 0
        } 
      } # End if first trial conditional 
      
      cog_Q_mat <- DecayQVals(cog_Q_mat, this_cog_Q_prior, action, state, phi, debug_info=tr)  
      
      cog_choice_kernel[state, action] <- cog_choice_kernel[state, action] + choice_LR * (1 - cog_choice_kernel[state, action])
      cog_choice_kernel[state, -action] <- cog_choice_kernel[state, -action] + choice_LR * (0 - cog_choice_kernel[state, -action])
      
      if (train_verbose) {
        cat("\n Q matrix after decay\n"); print(round(cog_Q_mat, 3))
        cat("\n Choice kernel after update\n"); print(round(cog_choice_kernel, 3))
        pause(.1)
      }
      
    } else {
      overt_Q_mat[state, ] <- q_vals
      
      if (tr == 1) {
        this_overt_Q_prior <- matrix(0, 4, 2)
      } else if  (tr == 161) {
        this_overt_Q_prior <- matrix(0, 4, 2)
      }  else {
        # Once they have experienced a state before, can decay to an informed prior value (e.g., -1 for pessimistic prior about punishment)..
        unexperienced_states <- setdiff(unique(states), unique(states[idx:tr]))
        
        this_overt_Q_prior <- overt_Q_prior 
        # .. but for unexperienced states, set the prior to 0 because it should just decay to uniformed ie. 0
        if (length(unexperienced_states) > 0) {
          this_overt_Q_prior[unexperienced_states, ] <- 0
        } 
      } # End if first trial conditional
      
      overt_Q_mat <- DecayQVals(overt_Q_mat, this_overt_Q_prior, action, state, phi, debug_info=tr)   
      
      overt_choice_kernel[state, action] <- overt_choice_kernel[state, action] + choice_LR * (1 - overt_choice_kernel[state, action])
      overt_choice_kernel[state, -action] <- overt_choice_kernel[state, -action] + choice_LR * (0 - overt_choice_kernel[state, -action])
      
      if (train_verbose) {
        cat("\n Q matrix after decay"); print(round(overt_Q_mat, 3))
        cat("\n Choice kernel after update"); print(round(overt_choice_kernel, 3))
        pause(.1)
      }
      
    }
    
    
    if (sim_opt=="sim") {
      ## Store sim outs  
      sim_actions[tr] <- action
      sim_corrs[tr] <- correct
      sim_rews[tr] <- reward
      sim_trial_RPE[tr] <- q_RPE
    } else if (sim_opt=="opt") {
      train_nll[tr] <- nll_chosen
    }
    
  } # End train trial loop 
  
  if (on_what == "train_SIT") {
    
    if (tr != length(conds)) { # Make sure training ran all the way through
      browser()  
    } 
    
    if (sim_opt == "sim") {
      # Drop the stuff we're simulating
      si_tdf <- subset(data.frame(SIT_df), select=-c(sit_resp, sit_correct))
      # Preallocate SIT storers
      sit_sim_actions <- rep(NA, length(si_tdf$state))
      sit_sim_corrs <- rep(NA, length(si_tdf$state))
    }
    
    # Vectorize vars
    sit_conds <- SIT_df$cond
    sit_states <- SIT_df$state
    si_ans <- SIT_df$answer
    sit_vals <- SIT_df$valence
    
    if (sim_opt == "opt") {
      sit_resp <- SIT_df$sit_resp
      sit_nll <- rep(NA, length(sit_resp))
    }
    
    for (si_idx in 1:length(sit_conds)) {
      
      # Get this trial info
      cond <- NULL
      cond <- sit_conds[si_idx]
      sit_state <- sit_states[si_idx]
      sit_val <- as.character(sit_vals[si_idx])
      
      # Clear vars before this trial's assignment 
      si_choice_kernel <- NULL
      this_si_choice_kernel <- NULL
      
      # Turn off before cluster  
      #test_verbose <- if_else(sit_state == 2, 1, 0)
      beta <- NULL 
      # Clear vars before this trial's assignment 
      if (cond == "cog") {
        si_q_vals <- cog_Q_mat[sit_state, ]
        this_si_choice_kernel <- cog_choice_kernel[sit_state, ]
        beta <- cog_beta
      } else {
        si_q_vals <- overt_Q_mat[sit_state, ]
        this_si_choice_kernel <- overt_choice_kernel[sit_state, ]
        beta <- overt_beta
      }
      
      ## Choose ##
      si_directed_choice_probs <- RunLSESoftmaxWithBiasAndCK(
        si_q_vals, 
        beta=beta, 
        bias=0, # Set bias to 0 for SIT  
        choice_kernel=this_si_choice_kernel
      )
      
      si_choice_probs <- epsilon * c(.5, .5) + (1-epsilon) * si_directed_choice_probs
      
      # Stop the code if choice probs is not valid  
      assert(sum(si_choice_probs) < 1.001)
      assert(sum(si_choice_probs) > .999)
      
      if (sim_opt=="sim") {
        ## Simulate action, find correct|choice, and simulate reward ##
        si_rd <- runif(1, 0, 1)
        si_action <- if_else(si_rd  < si_choice_probs[1], 1, 2) # 1 = bottom/diff, 2 = top/sum
        si_correct <- if_else(si_action == si_ans[si_idx], 1, 0)
        
        # Store
        sit_sim_actions[si_idx] <- si_action
        sit_sim_corrs[si_idx] <- si_correct
        
      } else if (sim_opt == "opt") {
        
        si_action <- sit_resp[si_idx]
        si_nll_chosen <- -log(si_choice_probs[si_action])
        # cat("\nbeta", beta)
        # cat("\nNLL", si_nll_chosen)
        
        # Store likelihood
        sit_nll[si_idx] <- si_nll_chosen
        
      }
      
      
      if (test_verbose) {
        cat("\n\n\n\n### TRIAL *Test* ### \n", si_idx, "\n State", sit_state, "\nCondition ", cond, "\nAction", si_action)
        cat("\nQ-values", round(si_q_vals, 3), 
            "\nCurrent choice kernel before update", round(this_si_choice_kernel, 3),
            "\nBeta", beta, "\nDirected choice probabilities", si_directed_choice_probs, 
            "\nEpsilon", epsilon, "\nChoice probs after epsilon", si_choice_probs)
      }
      
      # Continue updating the choice kernel in test (although it sometimes has already converged at 1 vs. 0 w sufficent choice LR and/or beta)
      if (cond == "cog") {
        cog_choice_kernel[sit_state, si_action] <- cog_choice_kernel[sit_state, si_action] + choice_LR * (1 - cog_choice_kernel[sit_state, si_action])
        cog_choice_kernel[sit_state, -si_action] <- cog_choice_kernel[sit_state, -si_action] + choice_LR * (0 - cog_choice_kernel[sit_state, -si_action])
        if (test_verbose) {
          cat("\n Choice learning rate", choice_LR)
          cat("\n Choice kernel after update\n"); print(round(cog_choice_kernel, 3))
        }
      } else {
        overt_choice_kernel[sit_state, si_action] <- overt_choice_kernel[sit_state, si_action] + choice_LR * (1 - overt_choice_kernel[sit_state, si_action])
        overt_choice_kernel[sit_state, -si_action] <- overt_choice_kernel[sit_state, -si_action] + choice_LR * (0 - overt_choice_kernel[sit_state, -si_action])
        if (test_verbose) {
          cat("\n Choice learning rate", choice_LR)
          cat("\n Choice kernel after update\n"); print(round(overt_choice_kernel, 3))
        }
      }
      
    } # End single-item test loop
    
  } # End whether to fit/sim single-item test
  
  if (sim_opt=="sim") {
    
    # Put together all the outs we can store in a nice df 
    sim_df <- data.table(data.frame(train_df), 
                         sim_actions, 
                         sim_chosen_Q_vals, 
                         sim_unchosen_Q_vals, 
                         sim_correct_Q_vals, 
                         sim_incorrect_Q_vals,
                         sim_prob_chosen,
                         sim_prob_unchosen,
                         sim_prob_correct,
                         sim_prob_incorrect,
                         sim_trial_RPE,
                         sim_corrs,  
                         sim_rews) %>% 
      rename(true_response=resp)
    
    sim_df$Q_val_diff <- sim_chosen_Q_vals - sim_unchosen_Q_vals
    
    if (on_what == "train_SIT") {
      sim_sit_df <- data.table(
        si_tdf, 
        sit_sim_actions, 
        sit_sim_corrs
      )
      
      output <- list("sim_train_df"=sim_df,
                     "sim_sit_df"=sim_sit_df)
      
    } 
    
  } else if (sim_opt=="opt") {
    
    # Return the appropriate likelihood 
    tr_cumsum_nll <- sum(train_nll)
    
    if (on_what == "train_SIT") {
      cum_SIT_nll <- sum(sit_nll)
      
      output <- sum(tr_cumsum_nll, 
                    cum_SIT_nll)
    }
    
    if (helpers$run_empirical_bayes == 1) {
      
      penalty <- -log(mvtnorm::dmvnorm(x=unlist(parameters), mean=mle_means, sigma=mle_covar))
      
      # Set to an exceedingly high number in case of effectively or actually 0 support  
      if (penalty > 1e9) penalty <- 1e9 
      
      # Needs to be off in s.R before going to cluster
      if (helpers$emp_bayes_verbose == 1) { 
        print_nll <- 1 
        cat("\n Penalty", penalty)
        cat("\n NLL otherwise", output)
      }
      
      output <- output + penalty  
    }
    
    if (print_nll) {
      
      cat("\n"); print(data.table(t(parameters)))
      cat("\n cum nll", output)
      cat("\n cum train nll", tr_cumsum_nll)
      cat("\n cum SIT nll", cum_SIT_nll)
      
    }
    
  } # End sim vs opt conditional 
  
output 
}



