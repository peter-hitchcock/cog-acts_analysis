RunOptTrainSITPR <- function(train_df, helpers, SIT_df, n_iters=1, mle_model_res=NULL) {
  ### *Specifically for parameter recov* - Optimize on one subject's data #
  # on training + single item test #
  
  ## Inits for pars not in [0, 1] ##
  ## Inverse temp ## 
  beta_init <- runif(1, 0, 20)
  beta_lb <- 1 # At too close to 0 indistinguishable from eps
  beta_50_ub <- 50
  beta_30_ub <- 30
  
  epsilon_custom_ub <- .3
  epsilon_init <- runif(1, 0, epsilon_custom_ub)
  
  # Rename the simulated variables for passing to optim function 
  train_df <- train_df %>% 
    rename(correct=sim_corrs, action=sim_actions, reward=sim_rews)
  SIT_df <- SIT_df %>% rename(sit_resp=sit_sim_actions)
  
  assert("Helpers set to fit on wrong data", helpers$on_what == "train_SIT")
  #helpers$info <- "in_from_opt"
  out <- PrepDfForModelPROpt(train_df, SIT_df, helpers)
  
  helpers <- out$helpers
  helpers$sim_opt <- "opt"
  train_df <- out$train_df
  SIT_df <- out$SIT_df

  all_results <- list()
  all_eb_results <- list()
  
  for (i in 1:n_iters) {
    
    if (helpers$which_model == "RunMQLearnerDiffDecayToPessPrior") {
      
      # Set this from to control all the repetitions of standard range pars
      n_standard_pars <- 5
      
      params <- c(
        epsilon_init,
        runif(n_standard_pars, 0, 1),
        # Non-standard
        beta_init
      )
      
      names(params) <- helpers$par_names
      
      lower_bound <- c(
        rep(0, n_standard_pars+1), 
        beta_lb
      )
      
      upper_bound <- c(
        epsilon_custom_ub,
        rep(1, n_standard_pars),
        beta_30_ub
      )
      
      opt_fun <- function(pars) RunMQLearnerDiffDecayToPessPrior(pars, train_df, helpers, SIT_df)  
      
      emp_bayes_opt_fun <- 
        function(pars) RunMQLearnerDiffDecayToPessPrior(pars, train_df, helpers, SIT_df, 
                                                        mle_means=param_means, 
                                                        mle_covar=param_covar)  
      
      new_params <- c(
        epsilon_init,
        runif(n_standard_pars, 0, 1),
        beta_init
      )
      
    }
    
    
    
    
    if (helpers$which_model == "RunMQLearnerDiffDecayToPessPriorESAndEpsFixed") {
      
      # Set this from to control all the repetitions of standard range pars
      n_standard_pars <- 4
      
      params <- c(
        runif(n_standard_pars, 0, 1),
        # Non-standard
        beta_init
      )
      
      names(params) <- helpers$par_names
      
      lower_bound <- c(
        rep(0, n_standard_pars), 
        beta_lb
      )
      
      upper_bound <- c(
        rep(1, n_standard_pars),
        beta_30_ub
      )
      
      opt_fun <- function(pars) RunMQLearnerDiffDecayToPessPriorESAndEpsFixed(pars, train_df, helpers, SIT_df)  
      
      emp_bayes_opt_fun <- 
        function(pars) RunMQLearnerDiffDecayToPessPriorESAndEpsFixed(pars, train_df, helpers, SIT_df, 
                                                               mle_means=param_means, 
                                                               mle_covar=param_covar)  
      
      new_params <- c(
        runif(n_standard_pars, 0, 1),
        beta_init
      )
      
    }
    
    if (helpers$run_empirical_bayes == 0) {
      
      # Standard optimization  
      helpers$parameter_names <- names(params)
        
        res <- 
          solnp(
            params,
            opt_fun,
            LB = lower_bound,
            UB = upper_bound,
            control = list(trace = 0, maxit=5e4)
          )
         
        all_results[[i]] <- res 
        
    } else if (helpers$run_empirical_bayes == 1) {
      
      #recovmle_model_res[grep("recovered", names(mle_model_res))]
      # Get means and covariance matrix from MLE fit 
      param_means <- unlist(colMeans(mle_model_res %>% select(names(params))))
      param_covar <- cov(mle_model_res %>% select(names(params)))
      
      # if (helpers$which_model == "RunMQLearnerDiffDecayToPessPrior") {
      #   
      #   
      # }
      # 
      
      # Rerun as empirical Bayes
      eb_res <- 
        solnp(
          new_params,
          emp_bayes_opt_fun,
          LB = lower_bound,
          UB = upper_bound,
          control = list(trace = 0, maxit=5e4)
        )
      
      all_eb_results[[i]] <- eb_res
      
    } # End Emp Bayes conditional 
    
  } # End loop through iters of same opt  
  
  if (helpers$run_empirical_bayes == 0) {
    all_results_general <- all_results 
  } else {
    all_results_general <- all_eb_results  
  }
  
  # Loop through list here and take out any NAs 
  collect_valid <- list()
  
  for (li in 1:length(all_results_general)) {
    this_opt <- all_results_general[[li]]
    if (!is.null(names(this_opt))) {
      collect_valid[[li]] <- this_opt
    }
  }

collect_valid
}

