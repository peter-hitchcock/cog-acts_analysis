RunMHybridDiffDecayQLearnerBayesAlt <- function(parameters, train_df, helpers, SIT_df=NULL, mle_means=NULL, mle_covar=NULL) { 
  ### Hybrid Bayes and Q-learner w Bayes varying by cond — alternate version with uncertainty on Bayes at action rather than avg level  ###
  
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
    sim_chosen_hybrid_Q_vals <- rep(NA, length(conds))
    sim_unchosen_hybrid_Q_vals <- rep(NA, length(conds))
    sim_correct_hybrid_Q_vals <- rep(NA, length(conds))
    sim_incorrect_hybrid_Q_vals <- rep(NA, length(conds))
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
  
  ## Iniitialize separate cognitive and overt Q matrices
  cog_Q_mat <- rbind(matrix(0, 2, 2), matrix(0, 2, 2))
  overt_Q_mat <- rbind(matrix(0, 2, 2), matrix(0, 2, 2))

  # Store initial matrix for decaying to on each trial 
  cog_Q_prior <- rbind(matrix(-1, 2, 2), matrix(0, 2, 2))
  overt_Q_prior <- rbind(matrix(-1, 2, 2), matrix(0, 2, 2))
  
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
    
    # First name the cog/overt vars and param generically ... 
    choice_kernel <- NULL; this_choice_kernel <- NULL
    bayes_Q_vals <- NULL; q_vals <- NULL; hybrid_Q_vals <- NULL; a1_row_ideal <- NULL; a2_row_ideal <- NULL
    proportion_a1 <- NULL; proportion_a2 <- NULL; rho_scaled <- NULL; rho_scalar <- NULL; weighted_uncertainty <- NULL
    phi <- NULL
    
    if (cond=="cog") {
      phi <- cog_phi 
      
      q_vals <- cog_Q_mat[state, ]
      
      a1_row_ideal <- cog_alpha_beta_tr_mat_a1[state, ]
      a2_row_ideal <- cog_alpha_beta_tr_mat_a2[state, ]
      
      choice_kernel <- cog_choice_kernel
    } else {
      phi <- overt_phi 
      
      q_vals <- overt_Q_mat[state, ]
      
      a1_row_ideal <- overt_alpha_beta_tr_mat_a1[state, ]
      a2_row_ideal <- overt_alpha_beta_tr_mat_a2[state, ]
      
      choice_kernel <- overt_choice_kernel
    }
    
    ## Calculate the mean and variance stats we need from Bayes
    # Use the counts of wins to calculate Bayesian Q-values 
    bayes_Q_vals <- c(
      CalcBayesMean(a1_row_ideal[1], a1_row_ideal[2], vals[tr], tr),
      CalcBayesMean(a2_row_ideal[1], a2_row_ideal[2], vals[tr], tr)
    )
    
    ## Calculate the precision of the ideal observer ## 
    arm1_std <- CalcBayesStd(a1_row_ideal[1], a1_row_ideal[2], tr)
    arm2_std <- CalcBayesStd(a2_row_ideal[1], a2_row_ideal[2], tr)
    
    ## Now calculate precision weight for ideal observer
    # Develop a scaled Bayesian contriubtion for each arm based on how much uncertainty there is relative to the total  
    rho_scaled_arm1 <- rho * (1-(arm1_std/max_uncertainty))
    rho_scaled_arm2 <- rho * (1-(arm2_std/max_uncertainty))
    
    # Weight the Bayesian contribution (max = rho) based on the amt of uncertainty in the arm 
    hybrid_Q_vals <- c(
      (1-rho_scaled_arm1)*q_vals[1] + rho_scaled_arm1*bayes_Q_vals[1],
      (1-rho_scaled_arm2)*q_vals[2] + rho_scaled_arm2*bayes_Q_vals[2]
    )
    
    # bayes_Q_vals <- c(
    #   CalcBayesMean(a1_row_ideal[1], a1_row_ideal[2], vals[tr], tr),
    #   CalcBayesMean(a2_row_ideal[1], a2_row_ideal[2], vals[tr], tr)
    # )
    # 
    # ## Calculate the precision of the ideal observer ## 
    # arm1_std <- CalcBayesStd(a1_row_ideal[1], a1_row_ideal[2], tr)
    # arm2_std <- CalcBayesStd(a2_row_ideal[1], a2_row_ideal[2], tr)
    # 
    # # Now calculate precision weight for ideal observer
    # 
    # # Calculate uncertainty proportional to policy so that the 
    # # uncertainty in the arm chosen more frequently is given more weight 
    # proportion_a2 <- 
    #   sum(a2_row_ideal)/(sum(a1_row_ideal)+sum(a2_row_ideal))
    # 
    # proportion_a1 <- 1-proportion_a2
    # 
    # weighted_uncertainty <- proportion_a2*arm2_std + 
    #   proportion_a1*arm1_std
    # 
    # rho_scalar <- 1-(weighted_uncertainty/max_uncertainty)
    # 
    # rho_scaled <- rho*rho_scalar
    # 
    # hybrid_Q_vals <- (1-rho_scaled)*q_vals + rho_scaled*bayes_Q_vals
    
    # ... and define the current choice kernel (can be 0)  
    this_choice_kernel <- choice_kernel[state, ]
    
    #### Choose #### 
    # Allow for a burst of stochasticity on the 8 trials after the block break (4 contingencies * 2 conds ) 
    # (otherwise get mis-spec here, because pts perform temporarily worse)
    if (tr_in_cond[tr] == 21) {
      
      beta_scaled_down <- beta * (1-explor_scalar)
      
      directed_choice_probs <- RunLSESoftmaxWithBiasAndCK(
        hybrid_Q_vals, 
        beta=beta_scaled_down, 
        bias=0,
        choice_kernel=this_choice_kernel
      )
      
    } else {
      directed_choice_probs <- RunLSESoftmaxWithBiasAndCK(
        hybrid_Q_vals, 
        beta=beta, 
        bias=0,
        choice_kernel=this_choice_kernel
      )  
    }
    
    # Add undirected choice probs : 1 / n_actions = (.5, .5)
    choice_probs <- epsilon * c(.5, .5) + (1-epsilon) * directed_choice_probs
    
    if (train_verbose) {
      cat("\n\n\n\n### TRIAL *Training* ### \n", tr, "\n State", state, "\nCondition ", cond)
      cat("\nHybrid Q-values before update", hybrid_Q_vals, "\nCurrent choice kernel before update", this_choice_kernel,
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
      sim_chosen_hybrid_Q_vals[tr] <- as.numeric(hybrid_Q_vals[action])
      sim_unchosen_hybrid_Q_vals[tr] <- as.numeric(hybrid_Q_vals[-action])
      
      sim_correct_hybrid_Q_vals[tr] <- as.numeric(hybrid_Q_vals[ans[tr]])
      sim_incorrect_hybrid_Q_vals[tr] <- as.numeric(hybrid_Q_vals[-ans[tr]])
      
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
    
    #### Adjust q-values Q-values #### 
    q_vals_list <- CalcQVals(q_vals, q_LR, action, reward, train_verbose, store_q_RPE=1)
    
    q_vals <- NULL; q_RPE <- NULL
    
    q_vals <- q_vals_list$q_vals 
    q_RPE <- q_vals_list$q_RPE
    
    if (train_verbose) {
      cat("\nAction", action, "\nReward", reward, "\nQ-values after update", q_vals)
    }
    
    # For use in settig the prior, set a running index indicating whether we are in the first or second condition 
    if (tr > 1) { # So the one-look back doesn't break.. 
      # If we're in the trial where the last tr_in_cond was 40 and this one is 1, then we're at the condition break, to set the idx to tr   
      if (tr_in_cond[tr-1]==40 & tr_in_cond[tr]==1) { idx <- tr } 
    }
    
    # Update alpha beta table  
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
        # .. but for unexperienced states, set the prior to 0 because it should just decay to uninformed ie. 0
        if (length(unexperienced_states) > 0) {
          this_cog_Q_prior[unexperienced_states, ] <- 0
        } 
      } # End if first trial conditional 
      
      cog_Q_mat <- DecayQVals(cog_Q_mat, this_cog_Q_prior, action, state, phi, debug_info=tr)  
      
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
    cog_hybrid_Q_vals <- matrix(NA, 4, 2)
    overt_hybrid_Q_vals <- matrix(NA, 4, 2)
    
    # Calculate the final state hybrid values 
    for (i in 1:4) {
      
      si_valence <- NULL 
      
      overt_a1_row_ideal <- NULL
      overt_a2_row_ideal <- NULL
      overt_arm1_std <- NULL
      overt_arm2_std <- NULL
      overt_uncertainty_mean <- NULL
      overt_rho_scaled_arm1 <- NULL
      overt_rho_scaled_arm2 <- NULL
      overt_bayes_Q_vals <- NULL
      
      cog_a1_row_ideal <- NULL
      cog_a2_row_ideal <- NULL
      cog_arm1_std <- NULL
      cog_arm2_std <- NULL
      cog_rho_scaled_arm1 <- NULL
      cog_rho_scaled_arm2 <- NULL
      this_cog_rho <- NULL
      cog_bayes_Q_vals <- NULL
      cog_weighted_uncertainty <- NULL
      
      if (i %in% c(1:2)) {
        si_valence <- "punishment"
      } else {
        si_valence <- "reward"
      }
      
      # Overt 
      overt_a1_row_ideal <- overt_alpha_beta_tr_mat_a1[i, ]
      overt_a2_row_ideal <- overt_alpha_beta_tr_mat_a2[i, ]
      
      overt_arm1_std <- CalcBayesStd(overt_a1_row_ideal[1], overt_a1_row_ideal[2])
      overt_arm2_std <- CalcBayesStd(overt_a2_row_ideal[1], overt_a2_row_ideal[2])
      
      ## Calculate the mean and variance stats we need from Bayes
      # Use the counts of wins to calculate Bayesian Q-values 
      overt_bayes_Q_vals <- c(
        CalcBayesMean(overt_a1_row_ideal[1], overt_a1_row_ideal[2], si_valence, tr),
        CalcBayesMean(overt_a2_row_ideal[1], overt_a2_row_ideal[2], si_valence, tr)
      )
      
      ## Calculate the precision of the ideal observer ## 
      overt_arm1_std <- CalcBayesStd(overt_a1_row_ideal[1], overt_a1_row_ideal[2], tr)
      overt_arm2_std <- CalcBayesStd(overt_a2_row_ideal[1], overt_a2_row_ideal[2], tr)
      
      ## Now calculate precision weight for ideal observer
      # Develop a scaled Bayesian contriubtion for each arm based on how much uncertainty there is relative to the total  
      overt_rho_scaled_arm1 <- rho * (1-(overt_arm1_std/max_uncertainty))
      overt_rho_scaled_arm2 <- rho * (1-(overt_arm2_std/max_uncertainty))
      
      # Weight the Bayesian contribution (max = rho) based on the amt of uncertainty in the arm 
      overt_hybrid_Q_vals[i, ] <- c(
        (1-overt_rho_scaled_arm1)*overt_Q_mat[i, 1] + overt_rho_scaled_arm1*overt_bayes_Q_vals[1],
        (1-overt_rho_scaled_arm2)*overt_Q_mat[i, 2] + overt_rho_scaled_arm2*overt_bayes_Q_vals[2]
      )
      
      # overt_proportion_a1 <- NULL 
      # overt_proportion_a2 <- NULL 
      # 
      # overt_proportion_a2 <- 
      #   sum(overt_a2_row_ideal)/(sum(overt_a1_row_ideal)+sum(overt_a2_row_ideal))
      # 
      # overt_proportion_a1 <- 1-overt_proportion_a2
      # 
      # overt_weighted_uncertainty <- overt_proportion_a2*overt_arm2_std + 
      #   overt_proportion_a1*overt_arm1_std
      # 
      # overt_rho_scalar <- 1-(overt_weighted_uncertainty/max_uncertainty)
      # 
      # this_overt_rho <- rho*overt_rho_scalar
      # 
      # overt_bayes_Q_vals <- c(
      #   CalcBayesMean(overt_a1_row_ideal[1], overt_a1_row_ideal[2], si_valence),#, #fill in val),
      #   CalcBayesMean(overt_a2_row_ideal[1], overt_a2_row_ideal[2], si_valence)
      # )
      # 
      # overt_hybrid_Q_vals[i, ] <- (1-this_overt_rho)*overt_Q_mat[i, ] + this_overt_rho*overt_bayes_Q_vals
      
      # Cog 
      cog_a1_row_ideal <- cog_alpha_beta_tr_mat_a1[i, ]
      cog_a2_row_ideal <- cog_alpha_beta_tr_mat_a2[i, ]
      
      cog_arm1_std <- CalcBayesStd(cog_a1_row_ideal[1], cog_a1_row_ideal[2])
      cog_arm2_std <- CalcBayesStd(cog_a2_row_ideal[1], cog_a2_row_ideal[2])
      
      ## Calculate the mean and variance stats we need from Bayes
      # Use the counts of wins to calculate Bayesian Q-values 
      cog_bayes_Q_vals <- c(
        CalcBayesMean(cog_a1_row_ideal[1], cog_a1_row_ideal[2], si_valence, tr),
        CalcBayesMean(cog_a2_row_ideal[1], cog_a2_row_ideal[2], si_valence, tr)
      )
      
      ## Calculate the precision of the ideal observer ## 
      cog_arm1_std <- CalcBayesStd(cog_a1_row_ideal[1], cog_a1_row_ideal[2], tr)
      cog_arm2_std <- CalcBayesStd(cog_a2_row_ideal[1], cog_a2_row_ideal[2], tr)
      
      ## Now calculate precision weight for ideal observer
      # Develop a scaled Bayesian contriubtion for each arm based on how much uncertainty there is relative to the total  
      cog_rho_scaled_arm1 <- rho * (1-(cog_arm1_std/max_uncertainty))
      cog_rho_scaled_arm2 <- rho * (1-(cog_arm2_std/max_uncertainty))
      
      # Weight the Bayesian contribution (max = rho) based on the amt of uncertainty in the arm 
      cog_hybrid_Q_vals[i, ] <- c(
        (1-cog_rho_scaled_arm1)*cog_Q_mat[i, 1] + cog_rho_scaled_arm1*cog_bayes_Q_vals[1],
        (1-cog_rho_scaled_arm2)*cog_Q_mat[i, 2] + cog_rho_scaled_arm2*cog_bayes_Q_vals[2]
      )
      
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
      si_hybrid_Q_vals <- NULL
      si_choice_kernel <- NULL
      this_si_choice_kernel <- NULL
      
      # Turn off before cluster  
      #test_verbose <- if_else(sit_state == 2, 1, 0)
      
      # Clear vars before this trial's assignment 
      if (cond == "cog") {
        si_hybrid_Q_vals <- cog_hybrid_Q_vals[sit_state, ]
        this_si_choice_kernel <- cog_choice_kernel[sit_state, ]
      } else {
        si_hybrid_Q_vals <- overt_hybrid_Q_vals[sit_state, ]
        this_si_choice_kernel <- overt_choice_kernel[sit_state, ]
      }
      
      ## Choose ##
      si_directed_choice_probs <- RunLSESoftmaxWithBiasAndCK(
        si_hybrid_Q_vals, 
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
        cat("\n Hybrid Q-values", round(si_hybrid_Q_vals, 3), 
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
                         sim_chosen_hybrid_Q_vals, 
                         sim_unchosen_hybrid_Q_vals, 
                         sim_correct_hybrid_Q_vals, 
                         sim_incorrect_hybrid_Q_vals,
                         sim_prob_chosen,
                         sim_prob_unchosen,
                         sim_prob_correct,
                         sim_prob_incorrect,
                         sim_trial_RPE,
                         sim_corrs,  
                         sim_rews) %>% 
      rename(true_response=resp)
    
    sim_df$hybrid_val_diff <- sim_chosen_hybrid_Q_vals - sim_unchosen_hybrid_Q_vals
    
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



