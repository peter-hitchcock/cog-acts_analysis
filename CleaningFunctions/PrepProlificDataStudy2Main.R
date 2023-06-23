PrepProlificDataStudy2Main <- function(js_file, n_keys=NULL, dont_have_ground_truth_SIT=NULL) {
  ###  For Study 1 (letters = overt, numbers = cog), main (ie. not pilot data) prep file ###
  # Takes in JSON file and outputs cleaned up training and test files ###
  
  ## Note that the function PrepProlificData from "Cleaning" was used to clean pilot data, but with the 
  # use of map instead of flatMap (to allow trial-wise repeats in case of invalid responses), the format 
  # changed and hence needed a different kind of loop  
  
  # Data storers #
  train_data <- list()
  pst_test <- list()
  single_item_test <- list()
  
  # Counters #
  tr <- 1
  pst_t <- 1
  sit_t <- 1
  
  have_pst_data <- NA
  have_SIT_data <- NA 
  pst_order <- NA
  SIT_order <- NA
  
  final_out <- lapply(js_file, function(x) ({
    # The data stored on firebase come in as a list in list, so need to lapply
    # through again for the main data, when length exceeds 1
  
    if (class(x) == "list") {
      
      for (i in 1:length(x)) {
        l <- data.table(t(x[[i]]))
        
        if ("which_experiment_phase" %in% names(l)) {
          
          if (l$which_experiment_phase %in% (c("training", "PST_test", "single_item_test"))) {
            
            if (l$which_experiment_phase == "training") {
              
              train_data[[tr]] <- l
              tr <- tr + 1
              
            } else if (l$which_experiment_phase == "PST_test") {
              
              # Record info about what data we have 
              pst_order <- if_else(is.na(have_SIT_data), "first", "second")
              have_pst_data <- 1
              
              # And store the actual data 
              pst_test[[pst_t]] <- l
              pst_t <- pst_t + 1
            } else if (l$which_experiment_phase == "single_item_test") {
              
              # Record info about what data we have 
              SIT_order <- if_else(is.na(have_pst_data), "first", "second")
              have_SIT_data <- 1
              
              # And store the actual data 
              single_item_test[[sit_t]] <- l
              sit_t <- sit_t + 1
            } # End experiment phase check  
            
          } # End first trial type check 
          
        } # End second trial type check 
        
      } # End for loop 
      
      # Package up in a list to send out 
      
      train_data <- train_data %>% bind_rows()
      
      #train_data <- data.frame(train_data, "PST_order"=pst_order, "SIT_order"=SIT_order)
      
      #train_data <- data.frame(train_data, "have_pst_data"=have_pst_data, "have_SIT_data"=have_SIT_data)
      all_out <- list("train_data"=train_data)
      print(unique(unlist(train_data$participant_id)))
      if (!is.na(have_pst_data)) {
        
        PST_test_data <- pst_test %>% bind_rows()
        PST_test_data <- data.frame(PST_test_data, pst_order)
        all_out$PST_data <- PST_test_data #<- list(all_out, "PST_data"=PST_test_data)
      }
      
      if (!is.na(have_SIT_data)) {
        
        single_item_test_data <- single_item_test %>% bind_rows()
        single_item_test_data <- data.frame(single_item_test_data, SIT_order)
        all_out$single_item_test_data <- single_item_test_data
        #all_out <- list(all_out, "single_item_test_data"=single_item_test_data)
      }
      
      all_out$pst_order <- pst_order
      all_out$SIT_order <- SIT_order
      cat("\n"); print(table(unlist(single_item_test_data$validResp)))
      cat("\n"); print(table(unlist(PST_test_data$validResp)))
      #if (table(unlist(PST_test_data < 80))) 
      #if (table(unlist(PST_test_data$validResp)) < 80) browser()
      cat("\n"); print(table(unlist(train_data$valid_resp)))
    } else {
      all_out <- NULL # Pass the stuff that's not task data out as null 
    }
    
  all_out
  }))
  all_final_out <- final_out[lapply(final_out, length) > 0]
  
  #print(unique(unlist(all_final_out$results$train_data$participant_id)))
  clean_results_out <- list()
  
  ## Clean up each dataframe ## 
  if ("train_data" %in% names(all_final_out$results)) {
    #CleanUpTrainStudy2Main
    train_df_out <- CleanUpTrainStudy2Main(data.frame(all_final_out$results$train_data), n_keys=n_keys)
    clean_results_out$train_df_this_pt <- train_df_out
  }
  #^ Train df: Examined and sanity checked in close detail on 2/10/22 
  
  if ("PST_data" %in% names(all_final_out$results)) {
    pst_df_out <- CleanUpPSTStudy2Main(all_final_out$results$PST_data, train_df_out)
    clean_results_out$pst_df_this_pt <- pst_df_out
  }
  #^ PST: Examined and sanity checked in close detail on 2/10/22  
  
  if ("single_item_test_data" %in% names(all_final_out$results)) {
    # Some pilot subjects for SIT had a potential bug in it, so skip collection for them 
    single_item_df_out <- CleanUpSingleItemTestStudy2Main(data.frame(all_final_out$results$single_item_test_data), 
                                                          train_df_out, n_keys=n_keys, dont_have_ground_truth_SIT=dont_have_ground_truth_SIT) # ** change for next version  
    clean_results_out$single_item_test_df_this_pt <- single_item_df_out
  }
  #^ Single-item-test: Examined and sanity checked in close detail on 2/10/22 
  
  clean_results_out$pst_order <- all_final_out$results$pst_order
  clean_results_out$SIT_order <- all_final_out$results$SIT_order

clean_results_out  
}

