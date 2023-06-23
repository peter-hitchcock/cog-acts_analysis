CleanUpTrainStudy1Main <- function(train_raw) {
  ### Clean training data d###
  
  # Some practice trials are labeled train, this gets rid of those 
  overt_invalid <-
    1-sum(unlist(train_raw[train_raw$block_and_cognVSovert == "overtTraining", "valid_response"])*1)/
    length(unlist(train_raw[train_raw$block_and_cognVSovert == "overtTraining", "valid_response"])*1)
  
  cogn_invalid <-
    1-sum(unlist(train_raw[train_raw$block_and_cognVSovert == "cognTraining", "valid_response"])*1)/
    length(unlist(train_raw[train_raw$block_and_cognVSovert == "cognTraining", "valid_response"])*1)
  
  conds <- unlist(train_raw$block_and_cognVSovert)
  
  # Add a first block / second block variable before removing invalid responses  
  train_raw[1:sum(rle(conds)$lengths[1:2]), "block"] <- 1
  train_raw[(sum(rle(conds)$lengths[1:2])+1):length(conds), "block"] <- 2
  
  tr <- train_raw 
  
  # Get first and second key presses
  kp1 <- VectorizeKP(tr$key_presses, 1)
  kp2 <- VectorizeKP(tr$key_presses, 2)
  rt <- tr$rt
  
  rt1 <- as.numeric(unlist(lapply(rt, function(x) strsplit(substr(x, 2, nchar(x)-3), ",")[[1]][1])))
  rt2_tmp <- unlist(lapply(rt, function(x) strsplit(substr(x, 2, nchar(x)), ",")[[1]][2]))
  rt2 <- as.numeric(unlist(lapply(rt2_tmp, function(x) gsub("\\[|\\]", "", x))))
  
  # Run a test to make sure that how correct was calculated within code matches up with 
  # calculating it from the raw data  
  assert("Error in correct calculation",
                 table(unlist(tr$true_answer_this_trial) == paste0(kp1, kp2)) == table(unlist(tr$correct)))
  
  train <-
    data.frame(
        unlist(tr$participant_id),
        unlist(tr$phase),
        unlist(tr$block),
        unlist(tr$block_and_cognVSovert),
        unlist(tr$valence_and_probability),
        unlist(tr$probability),
        unlist(tr$valence),
        unlist(tr$valid_response),
        unlist(tr$correct),
        unlist(tr$outcome),
        rt1, 
        rt2,
        unlist(tr$image),
        unlist(tr$bottom_item),
        unlist(tr$top_item),
        unlist(tr$true_answer_overall),
        unlist(tr$true_answer_this_trial),
        kp1, 
        kp2,
        1-length(which(unlist(tr$valid_response)))/nrow(tr)
      ) %>% setNames(c(
        "ID",
        "phase",
        "block",
        "condition",
        "valence_and_probability",
        "probability",
        "valence",
        "valid",
        "correct",
        # Binary for whether an outcome, either punishment 
        # or reward, was presented 
        "outcome",
        "rt_key_1",
        "rt_key_2",
        "stimulus",
        "bottom_item",
        "top_item",
        # The actual experiemnter-defined answers 
        "true_answer_as_category",
        "true_answer-as_key-press",
        "key_press_1",
        "key_press_2",
        "prop_invalid"
      ))
  
  train$overt_invalid <- overt_invalid
  train$cognitive_invalid <- cogn_invalid
  #valid_percent <- length(which(train$valid))/nrow(train)
  trv <- train[which(train$valid), ]
  
  table(trv$correct*1)[2]/nrow(trv)
  # Reward should be a clone of outcome, but with punishment outcomes -> -1
  trv$reward <- trv$outcome
  trv[trv$valence=="punishment" & trv$outcome==1, "reward"] <- -1
  
  trv$correct <- trv$correct*1
  
  # Spot checks
  #trv %>% select(valence, reward, valence_and_probability, correct) 
  #trv %>% group_by(correct, valence) %>% summarize(m=mean(reward))
  # Find the participants actual response in terms of the actions sum/diff or top/bottom (all that was 
  # recorded was key presses) How: Check if correct. If yes, then the participant responded 
  # with the true answer. If no, their response was the other type 
  response <- rep(NA, nrow(trv))
  # Vectorize 
  corr <- trv$correct 
  tac <- as.character(trv$true_answer_as_category)
  # Generate a numeric stimulus code for this subject's data. Note this 
  # will not respect condition boundaries, eg. 4,5,1,8 may be the overt stim 
  trv$stim_code <- as.numeric(trv$stimulus) 
  
  # This is redundant with trial within cond
  #trv$stim_iter <- sapply(1:length(trv$stim_code), function(i) sum(trv$stim_code[i] == trv$stim_code[1:i])) 
  cog_options <- c("sum", "difference")
  overt_options <- c("top", "bottom")
  
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
  
  cog_df <- FindRunningAverage(trv[trv$condition=="cognTraining", ])
  overt_df <- FindRunningAverage(trv[trv$condition=="overtTraining", ])
  
  # Note that cog and overt are now stacked on top of each other (not the actual order they were played)  
  tdf_out <- data.table(rbind(cog_df, overt_df))#, valid_percent)
  
tdf_out 
}
