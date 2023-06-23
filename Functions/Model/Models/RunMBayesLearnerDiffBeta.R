RunMBayesLearnerDiffBeta <- function(parameters, train_df, helpers, SIT_df=NULL, mle_means=NULL, mle_covar=NULL) { 
  ### Simple Bayes learner model ###
  
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
    sim_chosen_bayes_Q_vals <- rep(NA, length(conds))
    sim_unchosen_bayes_Q_vals <- rep(NA, length(conds))
    sim_correct_bayes_Q_vals <- rep(NA, length(conds))
    sim_incorrect_bayes_Q_vals <- rep(NA, length(conds))
    sim_prob_chosen <- rep(NA, length(conds))
    sim_prob_unchosen <- rep(NA, length(conds))
    sim_prob_correct <- rep(NA, length(conds))
    sim_prob_incorrect <- rep(NA, length(conds))
    sim_trial_RPE <- rep(NA, length(conds))
  }
  
  ## Initialize running alpha/beta matrices, where alpha = non-zero outcomes  
  cog_alpha_beta_tr_mat_a1 <- matrix(1, 4, 2)  
  cog_alpha_beta_tr_mat_a2 <- matrix(1, 4, 2)  
  
  overt_alpha_beta_tr_mat_a1 <- matrix(1, 4, 2)  
  overt_alpha_beta_tr_mat_a2 <- matrix(1, 4, 2)  
  
  ## Learning-rate adjusted choice kernel 
  cog_choice_kernel <- matrix(0, 4, 2)  
  overt_choice_kernel <- matrix(0, 4, 2)  
  
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
    choice_kernel <- NULL; this_choice_kernel <- NULL; bayes_Q_vals <- NULL; a1_row <- NULL; a2_row <- NULL; beta <- NULL
    
    if (cond=="cog") {
      beta <- cog_beta 
      
      a1_row <- cog_alpha_beta_tr_mat_a1[state, ]
      a2_row <- cog_alpha_beta_tr_mat_a2[state, ]
      choice_kernel <- cog_choice_kernel
    } else {
      beta <- overt_beta 
      
      a1_row <- overt_alpha_beta_tr_mat_a1[state, ]
      a2_row <- overt_alpha_beta_tr_mat_a2[state, ]
      choice_kernel <- overt_choice_kernel
    }
    
    # Use the counts of wins to calculate Bayesian Q-values 
    bayes_Q_vals <- c(
      CalcBayesMean(a1_row[1], a1_row[2], vals[tr], tr),
      CalcBayesMean(a2_row[1], a2_row[2], vals[tr], tr)
    )
    
    # ... and define the current choice kernel (can be 0)  
    this_choice_kernel <- choice_kernel[state, ]
    
    # Allow for a burst of stochasticity on the 8 trials after the block break (4 contingencies * 2 conds ) 
    # (otherwise get mis-spec here, because pts perform temporarily worse)
    if (tr_in_cond[tr] == 21) {
      
      beta_scaled_down <- beta * (1-explor_scalar)
      
      directed_choice_probs <- RunLSESoftmaxWithBiasAndCK(
        bayes_Q_vals, 
        beta=beta_scaled_down, 
        bias=0,
        choice_kernel=this_choice_kernel
      )
      
    } else {
      directed_choice_probs <- RunLSESoftmaxWithBiasAndCK(
        bayes_Q_vals, 
        beta=beta, 
        bias=0,
        choice_kernel=this_choice_kernel
      )  
    }
    
    # Add undirected choice probs : 1 / n_actions = (.5, .5)
    choice_probs <- epsilon * c(.5, .5) + (1-epsilon) * directed_choice_probs
    
    if (train_verbose) {
      cat("\n\n\n\n### TRIAL *Training* ### \n", tr, "\n State", state, "\nCondition ", cond)
      cat("\nBayes Q-values before update", bayes_Q_vals, "\nCurrent choice kernel before update", this_choice_kernel,
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
      sim_chosen_bayes_Q_vals[tr] <- as.numeric(bayes_Q_vals[action])
      sim_unchosen_bayes_Q_vals[tr] <- as.numeric(bayes_Q_vals[-action])
      
      sim_correct_bayes_Q_vals[tr] <- as.numeric(bayes_Q_vals[ans[tr]])
      sim_incorrect_bayes_Q_vals[tr] <- as.numeric(bayes_Q_vals[-ans[tr]])
      
      sim_prob_chosen[tr] <- as.numeric(choice_probs[action])
      sim_prob_unchosen[tr] <- as.numeric(choice_probs[-action])
      
      sim_prob_correct[tr] <- as.numeric(choice_probs[ans[tr]])
      sim_prob_incorrect[tr] <- as.numeric(choice_probs[-ans[tr]])
      
    } else if (sim_opt == "opt") {
      action <- resp[tr]
      reward <- rews[tr]
      nll_chosen <- -log(choice_probs[action])
    }
    
    #### Adjust q-values  #### 
    if (train_verbose) {
      cat("\nAction", action, "\nReward", reward, "\nQ-values after update", bayes_Q_vals)
    }
    
    # Update alpha beta table  
    if (cond == "cog") {
      
      if (action == 1) {
        #cat("\n Action == 1")
        cog_alpha_beta_tr_mat_a1 <- UpdateABMatrix(cog_alpha_beta_tr_mat_a1, state, reward, tr)
      } else {
        #cat("\n Action == 2")
        cog_alpha_beta_tr_mat_a2 <- UpdateABMatrix(cog_alpha_beta_tr_mat_a2, state, reward, tr)
      }
      
      cog_choice_kernel[state, action] <- cog_choice_kernel[state, action] + choice_LR * (1 - cog_choice_kernel[state, action])
      cog_choice_kernel[state, -action] <- cog_choice_kernel[state, -action] + choice_LR * (0 - cog_choice_kernel[state, -action])
      
      if (train_verbose) {
        cat("\n Choice kernel after update\n"); print(round(cog_choice_kernel, 3))
        pause(.1)
      }
      
    } else {
      
      if (action == 1) {
        
        overt_alpha_beta_tr_mat_a1 <- UpdateABMatrix(overt_alpha_beta_tr_mat_a1, state, reward, tr)
      } else {
        overt_alpha_beta_tr_mat_a2 <- UpdateABMatrix(overt_alpha_beta_tr_mat_a2, state, reward, tr)
      }
      
      overt_choice_kernel[state, action] <- overt_choice_kernel[state, action] + choice_LR * (1 - overt_choice_kernel[state, action])
      overt_choice_kernel[state, -action] <- overt_choice_kernel[state, -action] + choice_LR * (0 - overt_choice_kernel[state, -action])
      
      if (train_verbose) {
        cat("\n Choice kernel after update"); print(round(overt_choice_kernel, 3))
        pause(.1)
      }
      
    }
  
    # cat("\nCog mat arm 1 \n")
    # print(cog_alpha_beta_tr_mat_a1)
    # 
    # cat("\nCog mat arm 2 \n")
    # print(cog_alpha_beta_tr_mat_a2)
    
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
    
    # Make sure training ran all the way through
    assert(tr==length(conds)) 
    # Make sure the counts in each condition = half the trials + 16 initialization counts (all counts start at a 4*4 matrix of 1s before updating)
    assert(sum(cog_alpha_beta_tr_mat_a1)+sum(cog_alpha_beta_tr_mat_a2)==176)
    assert(sum(overt_alpha_beta_tr_mat_a1)+sum(overt_alpha_beta_tr_mat_a2)==176)
    
    ## Extract the final Bayesian Q-values now that we're done learning  
    
    # Preallocate
    final_cog_bayes_q_vals <- matrix(NA, 4, 2)
    final_overt_bayes_q_vals <- matrix(NA, 4, 2)
    
    for (st in 1:4) {
      
      # The first two states index punishment
      si_state_valence <- if_else(st %in% c(1, 2), "punishment", "reward")
      
      final_cog_bayes_q_vals[st, ] <- 
        c(
          CalcBayesMean(cog_alpha_beta_tr_mat_a1[st, 1], cog_alpha_beta_tr_mat_a1[st, 2], si_state_valence),
          CalcBayesMean(cog_alpha_beta_tr_mat_a2[st, 1], cog_alpha_beta_tr_mat_a2[st, 2], si_state_valence)
        )
      
      final_overt_bayes_q_vals[st, ] <- 
        c(
          CalcBayesMean(overt_alpha_beta_tr_mat_a1[st, 1], overt_alpha_beta_tr_mat_a1[st, 2], si_state_valence),
          CalcBayesMean(overt_alpha_beta_tr_mat_a2[st, 1], overt_alpha_beta_tr_mat_a2[st, 2], si_state_valence)
        )
      
    } # End state loop
    
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
      si_bayes_Q_vals <- NULL
      si_choice_kernel <- NULL
      this_si_choice_kernel <- NULL
      beta <- NULL 
      
      # Turn off before cluster  
      #test_verbose <- if_else(sit_state == 2, 1, 0)
      
      # Clear vars before this trial's assignment 
      if (cond == "cog") {
        beta <- cog_beta
        
        si_bayes_Q_vals <- final_cog_bayes_q_vals[sit_state, ]
        this_si_choice_kernel <- cog_choice_kernel[sit_state, ]
      } else {
        beta <- overt_beta
        
        si_bayes_Q_vals <- final_overt_bayes_q_vals[sit_state, ]
        this_si_choice_kernel <- overt_choice_kernel[sit_state, ]
      }
      
      ## Choose ##
      si_directed_choice_probs <- RunLSESoftmaxWithBiasAndCK(
        si_bayes_Q_vals, 
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
        
        # Store likelihood
        sit_nll[si_idx] <- si_nll_chosen
        
      }
      
      if (test_verbose) {
        cat("\n\n\n\n### TRIAL *Test* ### \n", si_idx, "\n State", sit_state, "\nCondition ", cond, "\nAction", si_action)
        cat("\nQ-values", round(si_bayes_Q_vals, 3), 
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
                         sim_chosen_bayes_Q_vals, 
                         sim_unchosen_bayes_Q_vals, 
                         sim_correct_bayes_Q_vals, 
                         sim_incorrect_bayes_Q_vals,
                         sim_prob_chosen,
                         sim_prob_unchosen,
                         sim_prob_correct,
                         sim_prob_incorrect,
                         sim_trial_RPE,
                         sim_corrs,  
                         sim_rews) %>% 
      rename(true_response=resp)
    
    sim_df$Q_val_diff <- sim_chosen_bayes_Q_vals - sim_unchosen_bayes_Q_vals
    
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



