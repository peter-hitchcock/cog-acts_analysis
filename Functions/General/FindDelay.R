FindDelay <- function(df) {
  ### Within-condition (overt vs. cog) — in a given block, for a given  participant — find the # 
  # delay since this trial's image was last encountered. 
  # Delay is indexed at 0, meaning that if the image was encountered at t-1 then 
  # delay is assigned 0 ###
  
  df_out <- lapply(split(df, list(df$ID, df$block, df$condition)), function(x) {
    
    delay <- rep(NA, nrow(x))
    val_probs <- x$valence_and_probability
    
    # Loop through rows of this subset 
    for (r in 1:nrow(x)) {
      
      # Note the first trial of the subset, and all trials until the first match for the stimulus 
      # are not counted as delay trials, hence are kept as NA 
      if (r > 1) { 
        # Find the valence and probability history until the trial that occurred this row 
        # (since we'll then want to find the last time that v-p occurred)
        vp_hx <- as.character(val_probs[1:r])
        
        # Get the full record of matches vs. non-matches between this vp and prior ones 
        record <- 
          as.numeric(unlist(foreach(vi = 1:(length(vp_hx)-1)) %do% {(vp_hx[length(vp_hx)] == vp_hx[vi])*1 }))
        
        # Take the difference between trial t and the last trial with a match to find the delay 
        if (any(record == 1)) {
          delay[r] <- length(record) - max(which(record==1))
        }
      } 
      
    } # End for loop 
    
    x$delay <- delay
  x  
  }) %>% bind_rows()
  
df_out
}
