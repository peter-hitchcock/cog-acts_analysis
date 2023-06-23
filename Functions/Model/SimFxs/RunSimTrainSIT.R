RunSimTrainSIT <- function(this_fit_df, train_df, helpers, SIT_df, n_iters=1, with_delay=0) {
  ### Simulate on one subject's data on training + SIT, given a set of parameters ###
  
  assert("Helpers set wrong — won't sim full data", helpers$on_what == "train_SIT")
  
  out <- PrepDfForModel(train_df, SIT_df, helpers, with_delay)
  helpers <- out$helpers
  train_df <- out$train_df
  
  SIT_df <- out$SIT_df
    
    parameters <- this_fit_df %>% select(helpers$par_names)
    
    sim_out_one_subj <- foreach (i = 1:n_iters) %do% {
        
      if (helpers$which_model == "RunMQLearnerDiffDecayToPessPrior") {
        this_sim <- RunMQLearnerDiffDecayToPessPrior(parameters, train_df, helpers, SIT_df)  
      }
    
      if (helpers$which_model == "RunMQLearnerDecayTo0Inits") {
        this_sim <- RunMQLearnerDecayTo0Inits(parameters, train_df, helpers, SIT_df)  
      }
    
      if (helpers$which_model == "RunMQLearnerDecayToPessPrior") {
        this_sim <- RunMQLearnerDecayToPessPrior(parameters, train_df, helpers, SIT_df)  
      }
      
    this_sim  
    } 
    
    # Bind up to send out 
    sim_train_df <- 
      foreach (i=1:n_iters) %do% {data.frame(sim_out_one_subj[[i]]$sim_train_df, "iter"=i)} %>% bind_rows()
    sim_SIT <- 
      foreach (i=1:n_iters) %do% {data.frame(sim_out_one_subj[[i]]$sim_sit_df, "iter"=i)} %>% bind_rows()
    
    sim_out_one_subj <- list("sim_train"=sim_train_df, "sim_sit"=sim_SIT)

sim_out_one_subj
}

