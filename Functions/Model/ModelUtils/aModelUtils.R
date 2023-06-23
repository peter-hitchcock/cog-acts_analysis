# FindBestFit <- function(opt_df, diff_optimizer="no", subset=NULL) {
#   # Deprecated since switched to rsolnp (so not same error msgs) 
###  Find pt with lowest nll, s.t. attempted constraint that they have only acceptable 
#   # error messages, although settling for bad error msgs if that's all they've got and geneating a warning ###
#   
#   # Idx  
#   if (!is.null(subset)) {
#     idx <- subset
#   } else {
#     idx <- 1:length(IDs)
#   }
#   
#   all_best_nll <-
#     
#     foreach (i = idx) %do% {
#       
#       if (diff_optimizer == "no") {
#         good_messages <- data.frame()
#         
#         ## Try to filter to only fits with acceptable error messages, but 
#         # resort to error fits if there are none 
#         good_messages <- opt_df %>% filter(ID==IDs[i]) %>% 
#           filter(message == "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH") 
#         
#         if (nrow(good_messages) < 1) {
#           
#           good_messages <- opt_df %>% filter(ID==IDs[i])  
#           cat("\nParticipant", unique(good_messages$ID), "had no fits with acceptable error messages." )
#         }  
#       } else {
#         good_messages <- opt_df
#       }
#       
#       ## Find the best nll.. 
#       best_nll <- good_messages %>%  filter(nll == min(nll))
#       ## .. breaking tie if there is more than one   
#       if (nrow(best_nll) > 1) best_nll <- best_nll[1, ]
#       
#       best_nll 
#     } %>% bind_rows()
#   
# all_best_nll  
# }

MatchIDs <- function(mdf1, mdf2) {
  ### Takes two model dfs mdf1 and mdf2 and orders them by ID - returns both as list ###
  mdf1$ID <- factor(mdf1$ID)
  mdf2$ID <- factor(mdf2$ID, levels=mdf1$ID)
  mdf2 <- mdf2[order(mdf2$ID), ]
  assert(mdf1$ID==mdf2$ID)
  
list(mdf1, mdf2)  
}
