FindDelayAlt <- function(pt_df) {
  ### Alternate delay that preserves condition order, and codes block break as NA ### 
  
  pt_df$vp_c <- pt_df$valence_and_probability
  
  # Create these separately for the cognitive and overt blocks 
  cdf_b1 <- ReturnDelayAndUniqueIntervening(
    pt_df %>% filter(condition=="cognitive" & trial_within_condition < 21)
  )
  cdf_b2 <- ReturnDelayAndUniqueIntervening(
    pt_df %>% filter(condition=="cognitive" & trial_within_condition > 20)
  )
  
  odf_b1 <- ReturnDelayAndUniqueIntervening(
    pt_df %>% filter(condition=="overt" & trial_within_condition < 21)
  )
  odf_b2 <- ReturnDelayAndUniqueIntervening(
    pt_df %>% filter(condition=="overt" & trial_within_condition > 20)
  )

  df_out <- rbind(cdf_b1, cdf_b2, odf_b1, odf_b2)
df_out
}

ReturnDelayAndUniqueIntervening <- function(df_sub) {
  ### Return the absolute delay (bounded only at number of trials) and number of intervening states (1-4) ###
  
  delay <- rep(NA, nrow(df_sub))
  unique_intervening_states <- rep(NA, nrow(df_sub))
  
  for (r in 1:nrow(df_sub))  {
     
      if (r > 1) {
        # Run through the history of valence+probability conditions and find all prior ones that match 
        # the current vp cond 
        vp_hx <- df_sub$vp_c[1:r]
        
        record <- 
          as.numeric(unlist(foreach(vi = 1:(length(vp_hx)-1)) %do% {(vp_hx[length(vp_hx)] == vp_hx[vi])*1 }))
        
        delay_tmp <- 0
        # If there are any matches, find the delay between them 
        if (any(record == 1)) {
          delay_tmp <- length(record) - max(which(record==1))
          if (is.na(delay_tmp)) browser()
          delay[r] <- delay_tmp
        }
        
        # If was a delay of at least 1, then find how many unique stimuli intervened 
        # in the sequence from the amount of delay back idx (r-delay) to the trial before the current one (r-1)
        if (delay_tmp > 0) {
          unique_intervening_states[r] <- length(unique(vp_hx[(r-delay_tmp):(r-1)]))
        } else {
          unique_intervening_states[r] <- 0
        }
        
      }
    }
    
  df_sub$delay <- delay 
  df_sub$unique_intervening_states <- unique_intervening_states
  
  # Set to NA for trial within condition < 2 # Already taken care of by conditionals and init starting at NA 
  # df_sub[df_sub$trial_within_condition < 2, "delay"] <- NA
  # df_sub[df_sub$trial_within_condition < 2, "unique_intervening_stim"] <- NA
df_sub
}
