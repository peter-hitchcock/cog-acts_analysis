CleanUpSingleItemTestStudy1Main <- function(sit_test_raw, train_df) {
  ### Single item test phase (presentation same as train but no feedback) # 
  # Train df inputted to match up reward history # 
  
  has_complete_data <- if_else(nrow(sit_test_raw) < 64, 0, 1)
  test2 <- sit_test_raw[sit_test_raw$valid_response==TRUE, ]
  
  kp1_t2 <- VectorizeKP(test2$key_presses, 1)
  kp2_t2 <- VectorizeKP(test2$key_presses, 2)
  
  rt_t2 <- test2$rt
  
  rt1_t2 <- as.numeric(unlist(lapply(rt_t2, function(x) strsplit(substr(x, 2, nchar(x)-3), ",")[[1]][1])))
  rt2_t2_tmp <- unlist(lapply(rt_t2, function(x) strsplit(substr(x, 2, nchar(x)), ",")[[1]][2]))
  rt2_t2 <- as.numeric(unlist(lapply(rt2_t2_tmp, function(x) gsub("\\[|\\]", "", x))))
  # This saves with the plugin type (train) so is wrong, so overwrite it with the correctly saved 
  # trial type variable 
  test2$phase <- test2$trial_type
  
  test2_out <-
    data.frame(
      unlist(test2$participant_id),
      unlist(test2$phase),
      unlist(test2$valence_and_probability),
      unlist(test2$valid_response),
      rt1_t2, 
      rt2_t2,
      unlist(test2$image),
      unlist(test2$bottom_item),
      unlist(test2$top_item),
      kp1_t2, 
      kp2_t2,
      1-length(which(unlist(sit_test_raw$valid_response)))/length(sit_test_raw$valid_response),
      has_complete_data) %>% 
    setNames(c(
        "ID",
        "phase",
        "valence_and_probability",
        "valid",
        "rt_key_1",
        "rt_key_2",
        "stimulus",
        "bottom_item",
        "top_item",
        "key_press_1",
        "key_press_2",
        "prop_invalid",
        "has_complete_data"
      ))
  
  # Add the reward history and assigned state index for this item  
  for (image in unique(test2_out$stimulus)) {
    this_train_sub <- train_df[train_df$stimulus == image, ]
    
    true_answer <- unique(this_train_sub$true_answer_as_category)
    reward_history <- as.numeric(unique(this_train_sub$reward_history))
    
    test2_out[test2_out$stimulus == image, "reward_history"] <- reward_history
    test2_out[test2_out$stimulus == image, "true_answer"] <- true_answer
  }
  
  # Now that have true answer, create the true answer as actual keys given the available options 
  responses <- rep(NA, nrow(test2_out))
  corrects <- rep(NA, nrow(test2_out))
  resp_as_category <- rep(NA, nrow(test2_out))
  true_answers <- as.character(test2_out$true_answer)
  top_items <- test2_out$top_item
  bottom_items <- test2_out$bottom_item 
  kp1s <- test2_out$key_press_1
  kp2s <- test2_out$key_press_2
  
  for (sa in 1:length(responses)) {
    
    # If cognitive..
    if (true_answers[sa] %in% c("difference", "sum")) {
      
      ti <- as.numeric(as.character(top_items[sa]))
      bi <- as.numeric(as.character(bottom_items[sa]))
      if (true_answers[sa] == "difference") {
        this_ta <- ti - bi
      } else {
        this_ta <- ti + bi
      }
      
      response <- as.numeric(paste0(as.character(kp1s[sa]), as.character(kp2s[sa])))
      responses[sa] <- response
      if (response == this_ta) {
        corrects[sa] <- 1
        resp_as_category[sa] <- if_else(true_answers[sa] == "difference", "difference", "sum")
      } else {
        corrects[sa] <- 0
        resp_as_category[sa] <- if_else(true_answers[sa] == "difference", "sum", "difference")
      }
      
    } else { # ..vs overt
      
      response <- paste0(as.character(kp1s[sa]), as.character(kp2s[sa]))
      responses[sa] <- response
      
      if (true_answers[sa] == "bottom") {
        specific_answer <- bottom_items[sa]
      } else {
        specific_answer <- top_items[sa]
      }
      if (specific_answer == response) {
        corrects[sa] <- 1
        resp_as_category[sa] <- if_else(true_answers[sa] == "top", "top", "bottom")
      } else {
        corrects[sa] <- 0
        resp_as_category[sa] <- if_else(true_answers[sa] == "top", "bottom", "top")
      }
      
    }
    
  }
  test2_out <- data.frame(test2_out, data.frame("response"=responses, "correct"=corrects, 
                                                "resp_as_category"=resp_as_category))
  # Spot check 2/10/22  
  #test2_out %>% select(top_item, bottom_item, true_answer, response, correct)
test2_out
}
