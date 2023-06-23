RecodeTrainDfForModel <- function(tdf) {
  ### Recode some variables in the training df for modeling ###
  # Answer coded to matches indices of value matrices (col of Q value matrix)
  
  # Code appropriately for study 1 vs 2  
  if ("slash" %in% tdf$true_answer_as_category) {
    tdf[tdf$true_answer_as_category=="backslash", "answer"] <- 1
    tdf[tdf$true_answer_as_category=="rev_alphabetize", "answer"] <- 1
    
    tdf[tdf$true_answer_as_category=="slash", "answer"] <- 2
    tdf[tdf$true_answer_as_category=="alphabetize", "answer"] <- 2  
  } else {
    tdf[tdf$true_answer_as_category=="bottom", "answer"] <- 1
    tdf[tdf$true_answer_as_category=="difference", "answer"] <- 1
    
    tdf[tdf$true_answer_as_category=="top", "answer"] <- 2
    tdf[tdf$true_answer_as_category=="sum", "answer"] <- 2  
  }
  
  # Same for response
  # This may need to differ for param recover 
  if ("slash" %in% tdf$true_answer_as_category) {
    tdf[tdf$response=="backslash", "resp"] <- 1
    tdf[tdf$response=="rev_alphabetize", "resp"] <- 1
    
    tdf[tdf$response=="slash", "resp"] <- 2
    tdf[tdf$response=="alphabetize", "resp"] <- 2  
  } else {
    tdf[tdf$response=="bottom", "resp"] <- 1
    tdf[tdf$response=="difference", "resp"] <- 1
    
    tdf[tdf$response=="top", "resp"] <- 2
    tdf[tdf$response=="sum", "resp"] <- 2  
  }
  
  # Re-representation of valence and probability as a state index (row of Q value matrix)
  tdf[tdf$valence_and_probability=="40-10_punishment", "state"] <- 1
  tdf[tdf$valence_and_probability=="90-10_punishment", "state"] <- 2
  
  tdf[tdf$valence_and_probability=="40-10_reward", "state"] <- 3
  tdf[tdf$valence_and_probability=="90-10_reward", "state"] <- 4
  
  # Concise condition 
  tdf[tdf$condition=="cognitive", "cond"] <- "cog"
  tdf[tdf$condition=="overt", "cond"] <- "overt"
  
tdf     
}


RecodeSingleItemTestDfForModel <- function(single_item_test_df, train_df) {
  ### Recode some variables in the training df for modeling ###
  # Answer coded to matches indices of value matrices (col of Q value matrix)
  
  if ("slash" %in% single_item_test_df$true_answer_as_category) {
    single_item_test_df[single_item_test_df$true_answer_as_category=="backslash", "answer"] <- 1
    single_item_test_df[single_item_test_df$true_answer_as_category=="rev_alphabetize", "answer"] <- 1
    
    single_item_test_df[single_item_test_df$true_answer_as_category=="slash", "answer"] <- 2
    single_item_test_df[single_item_test_df$true_answer_as_category=="alphabetize", "answer"] <- 2  
  } else {
    single_item_test_df[single_item_test_df$true_answer=="bottom", "answer"] <- 1
    single_item_test_df[single_item_test_df$true_answer=="difference", "answer"] <- 1
    
    single_item_test_df[single_item_test_df$true_answer=="top", "answer"] <- 2
    single_item_test_df[single_item_test_df$true_answer=="sum", "answer"] <- 2  
  }
  
  # Same for response
  # This may need to differ for param recover 
  if ("slash" %in% single_item_test_df$true_answer_as_category) {
    single_item_test_df[single_item_test_df$resp_as_category=="backslash", "resp"] <- 1
    single_item_test_df[single_item_test_df$resp_as_category=="rev_alphabetize", "resp"] <- 1
    
    single_item_test_df[single_item_test_df$resp_as_category=="slash", "resp"] <- 2
    single_item_test_df[single_item_test_df$resp_as_category=="alphabetize", "resp"] <- 2  
  } else {
    single_item_test_df[single_item_test_df$resp_as_category=="bottom", "resp"] <- 1
    single_item_test_df[single_item_test_df$resp_as_category=="difference", "resp"] <- 1
    
    single_item_test_df[single_item_test_df$resp_as_category=="top", "resp"] <- 2
    single_item_test_df[single_item_test_df$resp_as_category=="sum", "resp"] <- 2  
  }
  
  # Re-representation of valence and probability as a state index (row of Q value matrix)
  single_item_test_df[single_item_test_df$valence_and_probability=="40-10_punishment", "state"] <- 1
  single_item_test_df[single_item_test_df$valence_and_probability=="90-10_punishment", "state"] <- 2
  
  single_item_test_df[single_item_test_df$valence_and_probability=="40-10_reward", "state"] <- 3
  single_item_test_df[single_item_test_df$valence_and_probability=="90-10_reward", "state"] <- 4
  
  # Concise condition 
  single_item_test_df[single_item_test_df$phase=="cognTest2", "cond"] <- "cog"
  single_item_test_df[single_item_test_df$phase=="overtTest2", "cond"] <- "overt"
  
single_item_test_df  
}