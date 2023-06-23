PrepDfForModel <- function(train_df, SIT_df, helpers, with_delay=0) {
  ### Prep helpers train df and SIT do to go into the model ####
  #cat("\n"); print(helpers)
  #if (helpers$info=="in_from_opt") browser()
  
  #if (!helpers$sim_opt=="par_recov") {
  train_df <- RecodeTrainDfForModel(train_df)
  SIT_df <- RecodeSingleItemTestDfForModel(SIT_df, train_df)   
  #}
  
  cog_tdf <- train_df %>% filter(cond=="cog")
  overt_tdf <- train_df %>% filter(cond=="overt")
  
  # For fitting PST, store the unique image defining the state and stash them in helpers
  overt_stims <- rep(NA, 4)
  cog_stims <- rep(NA, 4)
  
  for (s in 1:4) {
    overt_stims[s] <- as.character(unlist(unique(overt_tdf[overt_tdf$state==s, "stimulus"])))
    cog_stims[s] <- as.character(unlist(unique(cog_tdf[cog_tdf$state==s, "stimulus"])))
  }
  
  helpers$overt_stims <- overt_stims
  helpers$cog_stims <- cog_stims
  
  if (with_delay) {
      train_df <- list(
          "ID"=train_df$ID,
          "trial_within_condition"=train_df$trial_within_condition,
          "cond"=train_df$cond,
          "state"=train_df$state,
          "answer"=train_df$answer,
          "valence"=train_df$valence,
          "probability"=train_df$probability,
          "rews"=train_df$reward,
          "resp"=train_df$resp,
          "delay"=train_df$delay
        )
  } else {
    train_df <- list(
          "ID"=train_df$ID,
          "trial_within_condition"=train_df$trial_within_condition,
          "cond"=train_df$cond,
          "state"=train_df$state,
          "answer"=train_df$answer,
          "valence"=train_df$valence,
          "probability"=train_df$probability,
          "rews"=train_df$reward,
          "resp"=train_df$resp
        )
  }
  
  SIT_df$probability <- factor(unlist(map(strsplit(as.character(SIT_df$valence_and_probability), "_"), 1)))
  SIT_df$valence <- factor(unlist(map(strsplit(as.character(SIT_df$valence_and_probability), "_"), 2)))
  
  SIT_df <- list(
    "ID"=SIT_df$ID,
    "answer"=SIT_df$answer,
    "cond"=SIT_df$cond,
    "state"=SIT_df$state,
    "sit_resp"=SIT_df$resp,
    "sit_correct"=SIT_df$correct,
    "valence_and_probability"=SIT_df$valence_and_probability,
    "valence"=SIT_df$valence,
    "probability"=SIT_df$probability
  )
  
  # if (helpers$par_recov) {
  #   train_df$resp <- train_df$sim_actions
  # } else {
  #   "resp"=train_df$resp
  # }

list("helpers"=helpers, "train_df"=train_df, "SIT_df"=SIT_df) 
}