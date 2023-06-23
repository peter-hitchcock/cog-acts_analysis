GatherDataForOneBatch <- function(filepath, n_keys=NULL, which_study=NULL, dont_have_ground_truth_SIT=NULL) {
  ### Read in all data for a batch of participants and organize into list collections of
  # training, PST, and SIT. 
  # Don't have ground truth SIT arg is from early practice version with possible bug in single item test ###
  
  if (which_study == 1) {
    
    this_batch <-
      lapply(list.files(filepath, pattern=".json"),
             function(x) PrepProlificDataStudy1Main(fromJSON(file=paste0(filepath, x))))  
  } else {
    this_batch <-
      lapply(list.files(filepath, pattern=".json"), 
             function(x) PrepProlificDataStudy2Main(fromJSON(file=paste0(filepath, x)), n_keys, dont_have_ground_truth_SIT)
             )
  }
  all_train_this_batch <- list()
  all_pst_this_batch <- list()
  all_SIT_this_batch <- list()
  all_SITorderings_this_batch <- list()
  
  for (i in 1:length(this_batch)) {
    
    this_one <- this_batch[[i]]
    
    if ("train_df_this_pt" %in% names(this_one)) all_train_this_batch[[i]] <- this_one$train_df_this_pt
    if ("pst_df_this_pt" %in% names(this_one)) all_pst_this_batch[[i]] <- this_one$pst_df_this_pt
    if ("single_item_test_df_this_pt" %in% names(this_one)) all_SIT_this_batch[[i]] <- this_one$single_item_test_df_this_pt
    if ("SIT_order" %in% names(this_one)) all_SITorderings_this_batch[[i]] <- this_one$SIT_order
  }
  
  
ls_out <- list(
     "training_subset"=all_train_this_batch, 
     "PST_subset"=all_pst_this_batch,
     "SIT_subset"=all_SIT_this_batch,
     "had_SIT_first"=all_SITorderings_this_batch
)
ls_out
}
