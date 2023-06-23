CleanUpSingleItemTestStudy2Main <- function(sit_test_raw, train_df, n_keys=NULL, dont_have_ground_truth_SIT=NULL) {
  ### Single item test phase (presentation same as train but no feedback) # 
  # Train df inputted to match up reward history # 
  
  test2 <- sit_test_raw[sit_test_raw$valid_response==TRUE, ]
   
  kp1_t2 <- VectorizeKP(test2$key_presses, 1)
  kp2_t2 <- VectorizeKP(test2$key_presses, 2)
  if (n_keys==3) kp3_t2 <- VectorizeKP(test2$key_presses, 3) # Change if 2 key version 
  
  rt_t2 <- test2$rt
  rt1_t2 <- unlist(map(rt_t2, 1))
  # rt2_t2 <- unlist(map(rt_t2, 2))
  # rt3_t2 <- unlist(map(rt_t2, 3)) # Change if 2 key version  
  
  # rt1_t2 <- as.numeric(unlist(lapply(rt_t2, function(x) strsplit(substr(x, 2, nchar(x)-3), ",")[[1]][1])))
  # rt2_t2_tmp <- unlist(lapply(rt_t2, function(x) strsplit(substr(x, 2, nchar(x)), ",")[[1]][2]))
  # rt2_t2 <- as.numeric(unlist(lapply(rt2_t2_tmp, function(x) gsub("\\[|\\]", "", x))))
  # This saves with the plugin type (train) so is wrong, so overwrite it with the correctly saved 
  # trial type variable 
  test2$phase <- test2$trial_type
  
  test2_out <-
    data.frame(
      unlist(test2$participant_id),
      unlist(test2$phase),
      unlist(test2$condition),
      unlist(test2$valence_and_probability),
      unlist(test2$valid_response),
      rt1_t2, 
      # rt2_t2,
      # rt3_t2,
      unlist(test2$image),
      unlist(test2$upper_display),
      unlist(test2$lower_display),
      # unlist(test2$bottom_item),
      # unlist(test2$top_item),
      kp1_t2, 
      kp2_t2,
      unlist(test2$true_answer),
      unlist(test2$true_answer_this_trial),
      #kp3_t2,
      1-length(which(unlist(sit_test_raw$valid_response)))/length(sit_test_raw$valid_response)) %>% 
    setNames(c(
        "ID",
        # Should be redundant just with diff labels (overtTest2 vs. overt)
        "phase",
        "condition",
        "valence_and_probability",
        "valid",
        "rt_key_1",
        # "rt_key_2",
        # "rt_key_3",
        "stimulus",
        "upper_display",
        "lower_display",
        # "bottom_item",
        # "top_item",
        "key_press_1",
        "key_press_2",
        "true_answer_as_category",
        "true_answer_as_key_press",
        #"key_press_3",
        "prop_invalid"
      ))
  
  assert("Train and test IDs don't match", unique(train_df$ID) == unique(test2_out$ID))
  
  if (n_keys == 3) test2_out$key_press_3 <- kp3_t2
  
  if (n_keys == 3) test2_out$responses <- paste0(test2_out$key_press_1, test2_out$key_press_2, test2_out$key_press_3)
  
  # Given that we're only now looking at valid responses, the designated response should always match 
  # the one of the options available on the trial — make sure that's true  
  option_1s <- map(test2$specific_options, 1)
  option_2s <- map(test2$specific_options, 2)

  for (tr in 1:length(test2_out$responses)) {
    these_options <- c(option_1s[tr], option_2s[tr])
    assert("Error: response doesn't match options, although this was counted as a valid trial",
           test2_out$responses[tr] %in% these_options)
  }
  
  test2_out$correct <- (test2_out$responses == test2_out$true_answer_as_key_press)*1
  
  # Add the reward history for this item  
  for (image in unique(test2_out$stimulus)) {
    this_train_sub <- train_df[train_df$stimulus == image, ]
    
    true_answer <- unique(this_train_sub$true_answer_as_category)
    reward_history <- as.numeric(unique(this_train_sub$reward_history))
    
    test2_out[test2_out$stimulus == image, "reward_history"] <- reward_history
    test2_out[test2_out$stimulus == image, "true_answer"] <- true_answer
    #test2_out[test2_out$stimulus == image, "true_answer"] <- true_answer
  }
  
  # TEMP ** delete  
  dont_have_ground_truth_SIT <- 1
  if (!is.null(dont_have_ground_truth_SIT)) {
    
    true_answers <- as.character(test2_out$true_answer)
    #true_answers == test2_out$true_answer_as_category
    # # Now that have true answer, create the true answer as actual keys given the available options 
    responses <- rep(NA, nrow(test2_out))
    corrects <- rep(NA, nrow(test2_out))
    resp_as_category <- rep(NA, nrow(test2_out))
    answer_as_keys <- rep(NA, nrow(test2_out))
    
    # 3.17 - did some spot checking and true answers appears to be coded right
    true_answers <- as.character(test2_out$true_answer)
  
    ud <- test2_out$upper_display
    ld <- test2_out$lower_display
    kp1s <- test2_out$key_press_1
    kp2s <- test2_out$key_press_2
  
    if (n_keys==3) kp3s <- test2_out$key_press_3
  
    # Manually calculate (from before was saving this out)
    for (sa in 1:length(responses)) {
  
      if (n_keys == 3) {
        these_keys <- paste0(kp1s[sa], kp2s[sa], kp3s[sa])
      } else {
        these_keys <- paste0(kp1s[sa], kp2s[sa])
      }
  
      this_display <- paste0(ud[sa], ld[sa])
      # If cognitive..
      # if (true_answers[sa] %in% c("alphabetize", "rev_alphabetize")) {
      
      #if (test2_out$condition[sa] == "cognTest2") {
      if (test2_out$condition[sa] == "cogn") {
        
        if (n_keys==3)  {
          if (true_answers[sa] == "alphabetize") {
              this_ta_tmp <-
                paste0(sort(c(unlist(strsplit(this_display, ""))[1], unlist(strsplit(this_display, ""))[2], unlist(strsplit(this_display, ""))[3])))
              this_ta <- paste0(this_ta_tmp[1], this_ta_tmp[2], this_ta_tmp[3])
            } else {
              this_ta_tmp <-
                rev(sort(c(unlist(strsplit(this_display, ""))[1], unlist(strsplit(this_display, ""))[2], unlist(strsplit(this_display, ""))[3])))
              this_ta <- paste0(this_ta_tmp[1], this_ta_tmp[2], this_ta_tmp[3])
            }
  
        } else {
            if (true_answers[sa] == "alphabetize") {
              this_ta_tmp <- sort(unlist(strsplit(this_display, "")))[1:2]
                #paste0(sort(c(unlist(strsplit(this_display, ""))[1], unlist(strsplit(this_display, ""))[2])))
  
              this_ta <- paste0(this_ta_tmp[1], this_ta_tmp[2])
            } else {
              this_ta_tmp <- rev(sort(unlist(strsplit(this_display, ""))))[1:2]
                #rev(sort(c(unlist(strsplit(this_display, ""))[1], unlist(strsplit(this_display, ""))[2])))
              this_ta <- paste0(this_ta_tmp[1], this_ta_tmp[2])
          }
  
        } # End 3 keys conditional
  
        # This this isnt' right and this is true answer as category, which already have
        #resp_as_category[sa] <- if_else(true_answers[sa] == "alphabetize", "alphabetize", "rev_alphabetize")
  
      } else { # ..vs overt
  
        if (true_answers[sa] == "backslash") {
  
          if (n_keys == 3) {
            this_ta <- paste0(substr(ud[sa], 2, 2), substr(ld[sa], 1, 2))
          } else {
            this_ta <- paste0(substr(ud[sa], 2, 2), substr(ld[sa], 1, 1))
          }
  
        } else { # If slash
  
          if (n_keys == 3) {
            this_ta <- paste0(substr(ud[sa], 1, 1), substr(ld[sa], 3, 4))
          } else {
            this_ta <- paste0(substr(ud[sa], 1, 1), substr(ld[sa], 3, 3))
          }
        }
        # Think this isnt' right and this is true answer as category, which already have
        #resp_as_category[sa] <- if_else(true_answers[sa] == "backslash", "backslash", "slash")
  
      }
  
      # Stuff that doesn't depend on conditio n
      if (n_keys == 3) {
        response <- paste0(test2_out$key_press_1[sa], test2_out$key_press_2[sa], test2_out$key_press_3[sa])
      } else {
        response <- paste0(test2_out$key_press_1[sa], test2_out$key_press_2[sa])
      }
      responses[sa] <- response
  
      if (response == this_ta) {
        corrects[sa] <- 1
      } else {
        corrects[sa] <- 0
      }
  
      answer_as_keys[sa] <- this_ta
      
    }
    
    # Some of these are now coming up as not matching, so investigate  
    #browser()
    # Check if the old way matches new way  
    # print(corrects == test2_out$correct)
    # print(responses == test2_out$responses)
    # print(true_answers == test2_out$true_answer_as_category)
    
    # cat("\nTrue answer", this_ta)
    # cat("\n Keys", kp1s[sa], kp2s[sa], kp3s[sa])
    # cat("\n Correct", corrects[sa])
    #
  }
  # 
  # test2_out <- data.frame(test2_out, data.frame("response"=responses, 
  #                                               "answer_as_keys"=answer_as_keys))#, 
                                                # "resp_as_category"=resp_as_category))
  #"correct"=corrects
  
test2_out
}
