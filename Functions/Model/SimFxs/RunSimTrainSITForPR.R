RunSimTrainSITForPR <- function(parameters, train_df, helpers, SIT_df, n_iters=1, with_delay=0) {
  ### *For parameter recovery only* — Simulate on one subject's data on training + SIT, given a set of parameters ###
  
  assert("Helpers set wrong — won't sim full data", helpers$on_what == "train_SIT")
  
  out <- PrepDfForModel(train_df, SIT_df, helpers, with_delay)

  helpers <- out$helpers
  train_df <- out$train_df
  
  SIT_df <- out$SIT_df
  helpers$sim_opt <- "sim"
  # Sim rews is labeled but null this for safety. In the model fx in sim, the empirical 
  # response will get relabeled as true resp so that's okay and in SIT sit_resp, sit_correct
  # will both get removed
  train_df$rews <- NULL 
  
    pr_sim_out_one_subj <- foreach (i = 1:n_iters) %do% {

      if (helpers$which_model == "RunMQLearnerDiffDecayToPessPrior") {
        this_pr_sim <- RunMQLearnerDiffDecayToPessPrior(parameters, train_df, helpers, SIT_df)  
      }
      
      if (helpers$which_model == "RunMQLearnerDiffDecayToPessPriorESAndEpsFixed") {
        this_pr_sim <- RunMQLearnerDiffDecayToPessPriorESAndEpsFixed(parameters, train_df, helpers, SIT_df)  
      }
      
    this_pr_sim 
    } 
    
    # Bind up to send out 
    pr_sim_train_df <- 
      foreach (i=1:n_iters) %do% {data.frame(pr_sim_out_one_subj[[i]]$sim_train_df, "iter"=i)} %>% bind_rows()
    pr_sim_SIT <- 
      foreach (i=1:n_iters) %do% {data.frame(pr_sim_out_one_subj[[i]]$sim_sit_df, "iter"=i)} %>% bind_rows()
    
    pr_sim_out_one_subj <- list("pr_sim_train"=pr_sim_train_df, "pr_sim_sit"=pr_sim_SIT)

pr_sim_out_one_subj
}

