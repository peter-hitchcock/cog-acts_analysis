AltPlotSimTrainingCurves <- function(emp_df, sim_df) {
  ### Alternate plots of empirical vs. sim learning curves:
  # First, take find the sim corresponding to the 5th and 95th percentile performance, marginalizing over 
  # stim iter. Then plot the actual aggregate curves (same amt of data as pts) against the empirical. (This will 
  # allow better ability to contrast the match to the shape of the aggregate empirical curve then older PlotSimTrainingCurves 
  # plots took the 5th/95th percentile anew each bin, which smooths over the actual oddities of individual curves by law 
  # of large numbers)  
  
  emp_summs <- NULL 
  sim_summs <- NULL
  full_sim_summs <- NULL
  
  emp_summs <- emp_df %>% 
    group_by(condition, trial_within_condition, valence_and_probability) %>% summarize(m=mean(correct))
  
  # Recode sim variables 
  sim_df$condition <- if_else(sim_df$cond=="cog", "cognitive", "overt")
  sim_df$valence_and_probability <- factor(paste(sim_df$probability, 
                                                 sim_df$valence, sep="_"))
  
  # Summary across iters  
  sim_summs <-
    sim_df %>% group_by(iter, 
                        condition, 
                        valence_and_probability) %>% 
    summarize(m=mean(sim_corrs))            
  
  
  # Find range and mean at each {condition, vp, trial within} level 
  percentile_by_vp_cond_summarizer <- 
    # Subset with {condition, valence+prob, trial}
    lapply(split(sim_summs, list(sim_summs$condition, 
                                 sim_summs$valence_and_probability)), function(x) {
                                   
                                   # Arrange from best to worst perf  
                                   x <- x %>% arrange(m)
                                   # Careful — only works if you ran 100 iters!!!
                                   assert(nrow(x) == 100)
                                   out <- 
                                     data.frame(x$valence_and_probability[1], x$condition[1], 
                                                x[5, "iter"], x[95, "iter"]) %>% 
                                     setNames(c("valence_and_probability", "condition", 
                                                "iter_5th_percentile",
                                                "iter_95th_percentile"))
                                   
                                   out                        
                                 }) %>% bind_rows()
  
  vps <- c("90-10_reward", "90-10_punishment", "40-10_reward", "40-10_punishment")
  conditions <- c("overt", "cognitive")
  labels <- c("90-10 Reward", "90-10 Punishment", "40-10 Reward", "40-10 Punishment")
  
  alt_empvs_sim_ps <- foreach (i = 1:length(vps)) %do% {
    
    this_vp_summary_cog <- percentile_by_vp_cond_summarizer %>% filter(valence_and_probability==vps[i] & condition=="cognitive")
    this_vp_summary_overt <- percentile_by_vp_cond_summarizer %>% filter(valence_and_probability==vps[i] & condition=="overt")
    
    sim_5_cog <- sim_df %>% filter(valence_and_probability==vps[i] & 
                                     condition=="cognitive" & iter== this_vp_summary_cog$iter_5th_percentile)
    sim_95_cog <- sim_df %>% filter(valence_and_probability==vps[i] & 
                                      condition=="cognitive" & iter== this_vp_summary_cog$iter_95th_percentile)
    
    sim_5_overt <- sim_df %>% filter(valence_and_probability==vps[i] & 
                                       condition=="overt" & iter== this_vp_summary_overt$iter_5th_percentile)
    sim_95_overt <- sim_df %>% filter(valence_and_probability==vps[i] & 
                                        condition=="overt" & iter== this_vp_summary_overt$iter_95th_percentile)
    
    sim_5_combined <- rbind(sim_5_overt, sim_5_cog)
    sim_95_combined <- rbind(sim_95_overt, sim_95_cog)
    
    
    sim_5_summs <- 
      sim_5_combined %>% group_by(condition, trial_within_condition) %>% summarize(m=mean(sim_corrs))
    sim_95_summs <- 
      sim_95_combined %>% group_by(condition, trial_within_condition) %>% summarize(m=mean(sim_corrs))
    
    
    this_plot <- ggplot(sim_5_summs, aes(x=trial_within_condition, y=m, group=condition, color=condition)) + 
      #geom_line(size=2, alpha=1, color="gray57") + 
      geom_line(size=1, alpha=1, linetype="longdash") +
      geom_line(data=sim_95_summs, aes(x=trial_within_condition, y=m, group=condition, color=condition),
                #color="black", size=2, alpha=1) +
                size=1, alpha=1, linetype="longdash") +
      geom_line(data=emp_summs %>% 
                  filter(valence_and_probability == vps[i]),
                aes(x=trial_within_condition, y=m, group=condition, color=condition), size=2, alpha=1) + 
      geom_hline(yintercept = c(seq(.5, 1, .1))) +
      geom_hline(yintercept = .5, size=3) +
      geom_vline(xintercept = 20, size=1, color="gray57", linetype="dotted") + 
      ga + ap + lp + facet_wrap(~ condition) + ft +
      xlab("Stim iteration") + ylab("Proportion correct") + tol +
      scale_color_manual(values=c("purple", "orange"))  + 
      ggtitle(labels[i]) + theme(plot.title = element_text(size = 35, hjust = .5))
    
    if (i %in% c(1:2)) this_plot <- this_plot + xlab("") + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
    
  this_plot
  } 
  
  
  list("aesp"=alt_empvs_sim_ps) 
}

AltOneSimEmpPlotRep <- function(plot_list, model_string="") {
  ### Plots faceted  ###
  
  out <- 
    plot_list$aesp[[1]] + plot_list$aesp[[2]] + 
    plot_list$aesp[[3]] + plot_list$aesp[[4]] + 
    plot_annotation(title = model_string,
                    theme = theme(plot.title = element_text(size = 25)))
out
}