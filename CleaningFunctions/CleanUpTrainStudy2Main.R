CleanUpTrainStudy2Main <- function(train_raw, n_keys=NULL) {
  ### Clean training data d###
  
  # Some practice trials are labeled train, this gets rid of those 
  
  # Add a new version with newly added block and condition variables  
  # conds <- unlist(train_raw$block_and_cognVSovert)
  # 
  # # Add a first block / second block variable before removing invalid responses  
  # train_raw[1:sum(rle(conds)$lengths[1:2]), "block"] <- 1
  # train_raw[(sum(rle(conds)$lengths[1:2])+1):length(conds), "block"] <- 2
  # Create these to calculate invalid response by type 
  overt_df_tmp <- train_raw[train_raw$condition=="overt", ]
  cog_df_tmp <- train_raw[train_raw$condition=="cognitive", ]
  
  tr <- train_raw %>% filter(valid_response == TRUE)
  
  # Get first and second key presses
  kp1 <- VectorizeKP(tr$key_presses, 1)
  kp2 <- VectorizeKP(tr$key_presses, 2)
  if (n_keys == 3) kp3 <- VectorizeKP(tr$key_presses, 3) # Change if 2 key version  
  
  rt <- tr$rt
  rt1 <- unlist(map(rt, 1))
  # 
  # if (length(rt1) < length(tr$correct)) browser()
  # rt2 <- unlist(map(rt, 2))
  # rt3 <- unlist(map(rt, 3)) # Change if 2 key version  
  # rt1 <- as.numeric(unlist(lapply(rt, function(x) strsplit(substr(x, 2, nchar(x)-3), ",")[[1]][1])))
  # rt2_tmp <- unlist(lapply(rt, function(x) strsplit(substr(x, 2, nchar(x)), ",")[[1]][2]))
  # rt2 <- as.numeric(unlist(lapply(rt2_tmp, function(x) gsub("\\[|\\]", "", x))))
  
  # Run a test to make sure that how correct was calculated within code matches up with 
  # calculating it from the raw data 
  
  #** Works for 3 so put back if reverting to that 
  if (n_keys == 3) {
    assert("Error in correct calculation",
                   all(table((paste0(kp1, kp2, kp3) == unlist(tr$true_answer_this_trial))*1) == table(unlist(tr$correct))))
  } else {
    assert("Error in correct calculation",
                 all(table((paste0(kp1, kp2) == unlist(tr$true_answer_this_trial))*1) == table(unlist(tr$correct)))) 
  }
  
  #browser()
  train <-
    data.frame(
        unlist(tr$participant_id),
        unlist(tr$phase),
        unlist(tr$condition),
        # unlist(tr$block), # Put back — this was erroneously labeled in pilot 2.v1
        unlist(tr$val_prob),
        #unlist(tr$block_and_cognVSovert),
        #unlist(tr$val_prob), # Put back 
        # unlist(tr$probability), # Put back 
        # unlist(tr$valence),
        unlist(tr$valid_response),
        unlist(tr$correct),
        unlist(tr$outcome),
        rt1, 
        #rt2,
        # Add 3rd rt? 
        unlist(tr$image),
        unlist(tr$lower_display), 
        unlist(tr$upper_display),
        # Put these back 
        unlist(tr$true_answer),
        unlist(tr$true_answer_this_trial),
        kp1, 
        kp2,
        #kp3,
        1-length(which(unlist(train_raw$valid_response)))/nrow(train_raw),
        1-length(which(unlist(overt_df_tmp$valid_response)))/nrow(overt_df_tmp),
        1-length(which(unlist(cog_df_tmp$valid_response)))/nrow(cog_df_tmp)
      )
  
  tr_names <- c(
    "ID",
    "phase",
    #"block",#
    "condition",
    "valence_and_probability",
    # "probability",
    # "valence",
    "valid",
    "correct",
    # Binary for whether an outcome, either punishment 
    # or reward, was presented 
    "outcome",
    "rt_key_1",
    #"rt_key_2",
    "stimulus",
    "bottom_item",
    "top_item",
    # The actual experiemnter-defined answers 
    "true_answer_as_category",
    "true_answer-as_key-press",
    "key_press_1",
    "key_press_2",
    #"key_press_3",
    "prop_invalid",
    "prop_invalid_overt",
    "prop_invalid_cognitive"
  )
  
  train <- train %>% setNames(tr_names)
  if (n_keys == 3) tr$key_press_3 <- kp3
  #print(tr$key_press_3)
  trv <- train[which(train$valid), ]
  
  trv$probability <- unlist(map(strsplit(trv$valence_and_probability, "_"), 1))
  trv$valence <- unlist(map(strsplit(trv$valence_and_probability, "_"), 2))
  
  # Reward should be a clone of outcome, but with punishment outcomes -> -1
  trv$reward <- trv$outcome
  trv[trv$valence=="punishment" & trv$outcome==1, "reward"] <- -1
  # Spot checks
  #trv %>% filter(valence=="punishment") %>% select(valence, outcome, reward)
  #trv %>% filter(valence=="punishment") %>% select(valence, outcome, reward)
  #trv %>% group_by(correct, valence) %>% summarize(m=mean(reward))
  
  trv$correct <- trv$correct*1
  
  trv$stim_code <- as.numeric(as.factor(trv$stimulus))
  # This is the same variable as stim iter in the RLWM lit — n times exposed to that stim 
  trv$trial_within_condition <- 
    sapply(1:length(trv$stim_code), function(i) sum(trv$stim_code[i] == trv$stim_code[1:i])) 
  #trv %>% select(trial_within_condition, stim_code, stimulus) # Spot check 
  
  # Find the participants actual response in terms of the actions sum/diff or top/bottom (all that was 
  # recorded was key presses) How: Check if correct. If yes, then the participant responded 
  # with the true answer. If no, their response was the other type 
  response <- rep(NA, nrow(trv))
  # Vectorize 
  corr <- trv$correct 
  tac <- as.character(trv$true_answer_as_category)
  # Generate a numeric stimulus code for this subject's data. Note this 
  # will not respect condition boundaries, eg. 4,5,1,8 may be the overt stim 
  
  cog_options <- c("alphabetize", "rev_alphabetize")
  overt_options <- c("slash", "backslash")

  for (re in seq_along(response)) {
    if (corr[re]) {
      response[re] <- tac[re]
    } else { # If incorrect
      if (tac[re] %in% cog_options) { # If cognitive
        response[re] <- setdiff(cog_options, tac[re])
      } else { # If overt
        response[re] <- setdiff(overt_options, tac[re])
      }
    }
  }
  
  trv$response <- response
  # Spot checks 
  # all(trv %>% select(response, true_answer_as_category, correct) %>% filter(correct == 1) %>% 
  #       select(true_answer_as_category) == trv %>% select(response, true_answer_as_category, correct) %>% filter(correct == 1) %>% select(response))
  # all(trv %>% select(response, true_answer_as_category, correct) %>% filter(correct == 0) %>% 
  #       select(true_answer_as_category) != trv %>% select(response, true_answer_as_category, correct) %>% filter(correct == 0) %>% select(response))
  
  # These are in Cleaning.R
  cog_df <- FindRunningAverage(trv[trv$condition=="cognitive", ])
  overt_df <- FindRunningAverage(trv[trv$condition=="overt", ])
  
  # Note that cog and overt are now stacked on top of each other (not the actual order they were played)  
  tdf_out <- data.table(rbind(cog_df, overt_df))#, valid_percent)
  
tdf_out 
}
