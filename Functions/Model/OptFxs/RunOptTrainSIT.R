RunOptTrainSIT <- function(train_df, helpers, SIT_df, n_iters=1, mle_model_res=NULL) {
  ### Optimize on one subject's data on training + single item test ###
  # All models here have choice and learning biases #  
  
  ## Inits for pars not in [0, 1] ##
  ## Inverse temp ## 
  beta_init <- runif(1, 0, 20)
  beta_lb <- 1 # At too close to 0 indistinguishable from eps
  beta_50_ub <- 50
  beta_30_ub <- 30

  epsilon_custom_ub <- .3
  epsilon_init <- runif(1, 0, epsilon_custom_ub)
  
  assert("Helpers set to fit on wrong data", helpers$on_what == "train_SIT")
  helpers$info <- "in_from_opt"
  out <- PrepDfForModel(train_df, SIT_df, helpers)
  
  helpers <- out$helpers
  train_df <- out$train_df
  SIT_df <- out$SIT_df

  # For MLE  
  all_results <- list() 
  # For empirical bayes  
  all_eb_results <- list()
  
  for (i in 1:n_iters) {
    
    if (helpers$which_model == "RunMQLearnerDiffDecayToPessPrior") {
      
      # Set this from to control all the repetitions of standard range pars
      n_standard_pars <- 5
      
      params <- c(
        epsilon_init,
        runif(n_standard_pars, 0, 1),
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
      
    }
    
    if (helpers$which_model == "RunMQLearnerDecayToPessPrior") {
      
      n_standard_pars <- 4
      
      params <- c(
        epsilon_init,
        runif(n_standard_pars, 0, 1),
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
      
      opt_fun <- function(pars) RunMQLearnerDecayToPessPrior(pars, train_df, helpers, SIT_df)  
      
      emp_bayes_opt_fun <- 
        function(pars) RunMQLearnerDecayToPessPrior(pars, train_df, helpers, SIT_df, mle_means=param_means, mle_covar=param_covar)  
      
      new_params <- c(
        # Eps 
        runif(1, 0, epsilon_custom_ub),
        runif(n_standard_pars, 0, 1),
        # Beta 
        runif(1, 0, 20)
      )  
    }
   
    if (helpers$which_model == "RunMQLearner") {
      
      n_standard_pars <- 3
      
      params <- c(
        epsilon_init,
        runif(n_standard_pars, 0, 1),
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
      
      opt_fun <- function(pars) RunMQLearner(pars, train_df, helpers, SIT_df)  
      
      emp_bayes_opt_fun <- 
        function(pars) RunMQLearner(pars, train_df, helpers, SIT_df, mle_means=param_means, mle_covar=param_covar)  
      
      new_params <- c(
        # Eps 
        runif(1, 0, epsilon_custom_ub),
        runif(n_standard_pars, 0, 1),
        # Beta 
        runif(1, 0, 20)
      )  
    }
    
    if (helpers$which_model == "RunMBayesLearner") {
      
      n_standard_pars <- 2
      
      params <- c(
        epsilon_init,
        runif(n_standard_pars, 0, 1),
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
      
      opt_fun <- function(pars) RunMBayesLearner(pars, train_df, helpers, SIT_df)  
      
      emp_bayes_opt_fun <- 
        function(pars) RunMBayesLearner(pars, train_df, helpers, SIT_df, mle_means=param_means, mle_covar=param_covar)  
      
      new_params <- c(
        # Eps 
        runif(1, 0, epsilon_custom_ub),
        runif(n_standard_pars, 0, 1),
        # Beta 
        runif(1, 0, 20)
      )  
    }
    
    if (helpers$which_model == "RunMHybridDecayingQLearnerDiffBayesAlt") {
      
      n_standard_pars <- 6
      
      params <- c(
        epsilon_init,
        runif(n_standard_pars, 0, 1),
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
      
      opt_fun <- function(pars) RunMHybridDecayingQLearnerDiffBayesAlt(pars, train_df, helpers, SIT_df)  
      
      emp_bayes_opt_fun <- 
        function(pars) RunMHybridDecayingQLearnerDiffBayesAlt(pars, train_df, helpers, SIT_df, mle_means=param_means, mle_covar=param_covar)  
      
      new_params <- c(
        # Eps 
        runif(1, 0, epsilon_custom_ub),
        runif(n_standard_pars, 0, 1),
        # Beta 
        runif(1, 0, 20)
      )  
      
    }
    
    if (helpers$which_model == "RunMHybridDiffDecayQLearnerBayesAlt") {
      
      n_standard_pars <- 6
      
      params <- c(
        epsilon_init,
        runif(n_standard_pars, 0, 1),
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
      
      opt_fun <- function(pars) RunMHybridDiffDecayQLearnerBayesAlt(pars, train_df, helpers, SIT_df)  
      
      emp_bayes_opt_fun <- 
        function(pars) RunMHybridDiffDecayQLearnerBayesAlt(pars, train_df, helpers, SIT_df, mle_means=param_means, mle_covar=param_covar)  
      
      new_params <- c(
        # Eps 
        runif(1, 0, epsilon_custom_ub),
        runif(n_standard_pars, 0, 1),
        # Beta 
        runif(1, 0, 20)
      )  
      
    }
    
    if (helpers$which_model == "RunMBayesLearnerDiffBeta") {
      
      n_standard_pars <- 2
      
      params <- c(
        epsilon_init,
        runif(n_standard_pars, 0, 1),
        beta_init,
        runif(1, 0, 20)
      )
      
      names(params) <- helpers$par_names 
      
      lower_bound <- c(
        rep(0, n_standard_pars+1), 
        beta_lb, 
        beta_lb
      )
      
      upper_bound <- c(
        epsilon_custom_ub,
        rep(1, n_standard_pars),
        beta_30_ub,
        beta_30_ub
      )
      
      opt_fun <- function(pars) RunMBayesLearnerDiffBeta(pars, train_df, helpers, SIT_df)  
      
      emp_bayes_opt_fun <- 
        function(pars) RunMBayesLearnerDiffBeta(pars, train_df, helpers, SIT_df, mle_means=param_means, mle_covar=param_covar)  
      
      new_params <- c(
        # Eps 
        runif(1, 0, epsilon_custom_ub),
        runif(n_standard_pars, 0, 1),
        # Beta 
        runif(1, 0, 20),
        runif(1, 0, 20)
      )  
      
    }
   
    
    if (helpers$which_model == "RunMQLearnerDecayTo0Inits") {
      
      # Set this from to control all the repetitions of standard range pars
      n_standard_pars <- 4
      
      params <- c(
        epsilon_init,
        runif(n_standard_pars, 0, 1),
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
      
      opt_fun <- function(pars) RunMQLearnerDecayTo0Inits(pars, train_df, helpers, SIT_df)  
      
      emp_bayes_opt_fun <- 
        function(pars) RunMQLearnerDecayTo0Inits(pars, train_df, helpers, SIT_df, mle_means=param_means, mle_covar=param_covar)  
      
      new_params <- c(
        # Eps 
        runif(1, 0, epsilon_custom_ub),
        runif(n_standard_pars, 0, 1),
        # Beta 
        runif(1, 0, 20)
      )  
    }
    
    if (helpers$which_model == "RunMQLearnerDecayToPessPriorDiffBeta") {
      
      n_standard_pars <- 4
      
      params <- c(
        epsilon_init,
        runif(n_standard_pars, 0, 1),
        beta_init,
        runif(1, 0, 20)
      )
      
      names(params) <- helpers$par_names 
      
      lower_bound <- c(
        rep(0, n_standard_pars+1), 
        beta_lb,
        beta_lb
      )
      
      upper_bound <- c(
        epsilon_custom_ub,
        rep(1, n_standard_pars),
        beta_30_ub,
        beta_30_ub
      )
      
      opt_fun <- function(pars) RunMQLearnerDecayToPessPriorDiffBeta(pars, train_df, helpers, SIT_df)  
      
      emp_bayes_opt_fun <- 
        function(pars) RunMQLearnerDecayToPessPriorDiffBeta(pars, train_df, helpers, SIT_df, mle_means=param_means, mle_covar=param_covar)  
      
      new_params <- c(
        epsilon_init,
        runif(n_standard_pars, 0, 1),
        beta_init,
        runif(1, 0, 20)
      )  
      
    }
    
    if (helpers$which_model == "RunMQLearnerDecayToPessPriorDiffLR") {
      
      # Set this from to control all the repetitions of standard range pars
      n_standard_pars <- 5
      
      params <- c(
        epsilon_init,
        runif(n_standard_pars, 0, 1),
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
      
      opt_fun <- function(pars) RunMQLearnerDecayToPessPriorDiffLR(pars, train_df, helpers, SIT_df)  
      
      emp_bayes_opt_fun <- 
        function(pars) RunMQLearnerDecayToPessPriorDiffLR(pars, train_df, helpers, SIT_df, mle_means=param_means, mle_covar=param_covar)  
      
      new_params <- c(
        # Eps 
        runif(1, 0, epsilon_custom_ub),
        runif(n_standard_pars, 0, 1),
        # Beta 
        runif(1, 0, 20)
      )  
    }
    
    # Set up for FIRST TIME OPT #
    # This will always be 0 on the first round of opt if Empirical Bayes so can first get MLE fit  
    if (helpers$run_empirical_bayes == 0) { 
      
      if (helpers$handle_errors) {
        
        res <- 
          tryCatch(error=function(cnd) NA,
                   Rsolnp::solnp(
                     params,
                     opt_fun,
                     LB = lower_bound,
                     UB = upper_bound,
                     control = list(trace = 0, maxit=5e4)
                   ), silent=TRUE)
      
      } else {
        
        # Standard optimization  
        helpers$parameter_names <- names(params)
        if (helpers$non_diff == 0) {
          
          res <- 
            solnp(
              params,
              opt_fun,
              LB = lower_bound,
              UB = upper_bound,
              control = list(trace = 0, maxit=5e4)
            )
        
        # Non=diff optimization (requires nloptr)
        } else {
          
          max_eval <- 5e5
          
          if (helpers$or == 1) {
            algo <- "NLOPT_LN_COBYLA"
          } else if (helpers$or == 2) {
            algo <- "NLOPT_LN_SBPLX"
          } else if (helpers$or == 3) {
            algo <- "NLOPT_GN_CRS2_LM"
            #max_eval <- 1e6
          }
          
          res <- nloptr(
            x0=params,
            eval_f=opt_fun,
            lb=lower_bound, 
            ub=upper_bound,
            opts = list("algorithm"=algo, maxeval=max_eval)
          )
          
        }
        
      }
      all_results[[i]] <- res
    
    # Refit — this should only get hit on the second round of opt and in case of using Empirical Bayes 
    } else if (helpers$run_empirical_bayes == 1) {
      
      # Get means and covariance matrix from MLE fit 
      param_means <- unlist(colMeans(mle_model_res %>% select(names(params))))
      param_covar <- cov(mle_model_res %>% select(names(params)))
      #cat("\n Inside run opt")
      
      if (helpers$which_model == "RunMQLearnerDiffDecayToPessPrior") {
        
       emp_bayes_opt_fun <- function(pars) RunMQLearnerDiffDecayToPessPrior(pars, train_df, helpers, SIT_df, mle_means=param_means, mle_covar=param_covar)  
        
        new_params <- c(
          epsilon_init,
          runif(n_standard_pars, 0, 1),
          beta_init
        )  
      }
      
      # if (helpers$which_model == "RunMQLearnerDiffDecayToPessPriorCKDecay") {
      # 
      # }
      
      # Rerun as empirical Bayes
      eb_res <- 
        Rsolnp::solnp(
          new_params,
          emp_bayes_opt_fun,
          LB = lower_bound,
          UB = upper_bound,
          control = list(trace = 0, maxit=5e4)
        )
      
      all_eb_results[[i]] <- eb_res
      
    }
    
  } # End loop through iters of same opt 
  
  ## Give a generic name for this last step whether EB or MLE...   
  if (helpers$run_empirical_bayes == 0) {
    all_results_general <- all_results 
  } else {
    all_results_general <- all_eb_results  
  }
  
  # ... and loop through list to take out any NAs 
  collect_valid <- list()
  for (li in 1:length(all_results_general)) {
    this_opt <- all_results_general[[li]]
    if (!is.null(names(this_opt))) {
      collect_valid[[li]] <- this_opt
    }
  }

# Return just the ones without NAs
collect_valid
}

