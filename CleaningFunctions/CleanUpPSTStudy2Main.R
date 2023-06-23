CleanUpPSTStudy2Main <- function(PST_raw, train_df) {
  ### Prep df for PST style test. Train df is used to match training to associate 
  # the choice with the correct condition ###
  
  # Delete all missing/invalid responses (but leave in responses under 200ms that triggered respond slower message)  
  te <- PST_raw %>% filter(!key_presses == "rightarrow" || !key_presses == "leftarrow") #PST_raw[PST_raw$validResp==TRUE, ]
  
  if (any(unlist(lapply(te$rt, is.null))==TRUE)) te <- te[-which(unlist(lapply(te$rt, is.null))), ] #PST_raw %>% filter(!is.null(rt))

  # .. then create df 
  left_option <- unlist(lapply(te$ordered_conditions_pair, function(x) x[1]))
  right_option <- unlist(lapply(te$ordered_conditions_pair, function(x) x[2])) 
  
  test <- data.frame(
    unlist(te$participant_id),
    unlist(te$phase),
    left_option,
    right_option,
    unlist(te$image1),
    unlist(te$image2),
    unlist(te$validResp),
    unlist(te$key_presses),
    unlist(te$key_raw),
    unlist(te$rt),
    1-length(which(unlist(PST_raw$validResp)))/length((unlist(PST_raw$validResp)))
  ) %>% setNames(c(
    "ID",
    "phase",
    "left_valence_prob",
    "right_valence_prob",
    "left_image",
    "right_image",
    "valid",
    "key_press_char",
    "key_press_raw",
    "rt",
    "prop_invalid"
  ))
  
  # Filter invalid responses  
  test <- test %>% filter(valid==TRUE)
  
  test$response <- as.character(test$key_press_char)
  test[test$response=="leftarrow", "response"] <- "left"
  test[test$response=="rightarrow", "response"] <- "right"
  
  ## Currently not saving out condition from test pull, so need to get it by matching up the image 
  # with the training data and finding the condition. Should fix this, but this is how done for now...
  
  # Fastly searchable train data 
  trd <- data.table(train_df$stimulus, train_df$condition) %>% setNames(c("stim", "cond"))
  
  li <- test$left_image
  ri <- test$right_image
  
  # Get the left and right training condition by matching up stim to training 
  left_training_cond <- unlist(foreach (i=seq_along(li)) %do% trd$cond[which(li[i]==train_df$stimulus)[1]])
  right_training_cond <- unlist(foreach (i=seq_along(ri)) %do% trd$cond[which(ri[i]==train_df$stimulus)[1]])
  test$test_condition <- paste(map(strsplit(as.character(left_training_cond), "Tr"), 1), 
                               map(strsplit(as.character(right_training_cond), "Tr"), 1), sep="_")
  
  # Create a single name for mixed test data 
  test[test$test_condition=="cognitive_overt", "test_condition"] <- "overt_cognitive"
  # Spot check that this is matched contingencies only — 
  #test %>% select(test_condition, left_valence_prob, right_valence_prob)
  
  test_df_new <- data.table(test, left_training_cond, right_training_cond)#, test_condition)
  
  ## Add reward history left and right 
  for (image in unique(test_df_new$left_image)) {
    test_df_new[test_df_new$left_image==image, "reward_history_left"] <-
      as.numeric(unique(train_df[train_df$stimulus == image, 'reward_history']))
  }
  
  for (image in unique(test_df_new$right_image)) {
    test_df_new[test_df_new$right_image==image, "reward_history_right"] <-
      as.numeric(unique(train_df[train_df$stimulus == image, 'reward_history']))
  }
  
  # Calculate rew hx difference
  test_df_new$left_min_right <- 
    test_df_new$reward_history_left - test_df_new$reward_history_right
  
  # Numeric response 
  test_df_new[test_df_new$response=="right", "resp_num"] <- 0
  test_df_new[test_df_new$response=="left", "resp_num"] <- 1
  
  test_df_new$rt <- as.numeric(unlist(test$rt))
  
  # This is ordered from highest to lowest expected value. Hence, choice 1 is superior to all other choices, 
  # choice 2 superior to choices 3 and 4, etc. 
  key <- c("90-10_reward", "40-10_reward", "40-10_punishment", "90-10_punishment")
  # Order type by expected value 
  type_key <- c("freq-R", "infreq-R", "infreq-P", "freq-P")
  
  correct <- rep(NA, nrow(test_df_new))
  choice_type <- rep(NA, nrow(test_df_new))
  choice_type_plotting <- rep(NA, nrow(test_df_new))
  choice_as_category <- rep(NA, nrow(test_df_new))
  
  # Prep choice types (eg. infreq reward)
  l_type <- ConvertOptions(as.character(test_df_new$left_valence_prob))
  r_type <- ConvertOptions(as.character(test_df_new$right_valence_prob))
  # Spot check match 
  # test_df_new$left_valence_prob
  # l_type
  
  for (r in 1:nrow(test_df_new)) {
    
    this_choice_options <- test_df_new[r, c("left_valence_prob", "right_valence_prob")]
    this_choice_chars <- c(as.character(this_choice_options$left_valence_prob), as.character(this_choice_options$right_valence_prob))
    # Convert choice to an index of this this_choice_chars. Left was coded 1 and the first char corresponds to the left option, so 
    # that doesn't need to be converted...
    
    this_choice_to_idx <- test_df_new[r, "resp_num"]$resp_num 
    if (!is.na(this_choice_to_idx)) {
      if (this_choice_to_idx == 0) {
        this_choice_to_idx <- 2 
      } #... whereas choice 0 = right corresponds to index 2 
    }
    #if (r==29) browser()
    # Find the ordering of the chosen and unchosen option in key 
    order_of_choice <- which(key == this_choice_chars[this_choice_to_idx])
    order_of_other_option <- which(key == this_choice_chars[-this_choice_to_idx])
    
    # If this is not a mixed option (in which case the contingencies are the same), 
    # then it's between options with different expected values and thus correctness 
    # can be asssessed 
    if (!this_choice_chars[1] == this_choice_chars[2]) {
      if (order_of_choice < order_of_other_option) {
        correct[r] <- 1
      } else {
        correct[r] <- 0
      }  
      choice_as_category[r] <- 
        if_else(as.character(test_df_new[r, "response"])=="left", l_type[r], r_type[r])
    }
    
    # Describe the choice type (eg. infreq rew vs. freq pun) so that the 
    # first option is always the one with higher expected value (or in mixed,
    # the same expected value)  
    # VERY IMPORTANT NOTE: This *does not respect*  
    # the ordering of the choices. For instance, left= 90-10R vs. right = 90-10P and 
    # left = 90-10P vs. right = 90=10R both would map to freq-R__freq-P. Thus, you would 
    # *NOT* be able to look at p(left) to determine the freq of picking 
    # punishment. Instead, to see the specific category they chose, use 
    # choice_as_category 
    type_indices <- c(which(type_key == l_type[r]), which(type_key == r_type[r]))
    
    # If mixed type, the order doesn't matter.. 
    if (type_indices[1] == type_indices[2]) {
      choice_type[r] <- paste(l_type[r], r_type[r], sep="__")
      choice_type_plotting[r] <- paste(l_type[r], r_type[r], sep="\n")
    # .. but otherwise order by expected value 
    } else {
      # If the first option is higher EV (= lower index) than the second 
      if (type_indices[1] < type_indices[2]) {
        choice_type[r] <- paste(l_type[r], r_type[r], sep="__")
        choice_type_plotting[r] <- paste(l_type[r], r_type[r], sep="\n")
      } else {
        choice_type[r] <- paste(r_type[r], l_type[r], sep="__")  
        choice_type_plotting[r] <- paste(r_type[r], l_type[r], sep="\n")  
      }
    }
    
  }
  
  # Store 
  test_df_new$correct <- correct
  test_df_new$choice_type <- choice_type
  test_df_new$choice_type_plotting <- choice_type_plotting
  # This can be used to plot the Palminteri-style plots with freq of the 
  # given response  
  test_df_new$choice_as_category <- choice_as_category
  
test_df_new  
}