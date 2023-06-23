SimplePlotSimTrainingDelay <- function(emp_df, sim_df, low_range=.1, high_range=.9) {
  ### Returns plots of empirical vs simulated delay for different levels of simulated performance (marginalizing 
  # over decay to calc levels, but then stratifying on delay within each sim at diff perf level) — thereby examining 
  # how well sims in low, middle, and upper range of perf capture delay at diff amts  
  
  emp_summs <- NULL 
  sim_summs <- NULL
  full_sim_summs <- NULL
  
  emp_summs <- emp_df %>% 
    group_by(condition, delay_trunc) %>% 
    summarize(m=mean(correct))
  
  emp_summs <- na.omit(emp_summs)
  
  # Recode sim variables 
  sim_df$condition <- if_else(sim_df$cond=="cog", "cognitive", "overt")
  
  sim_df$delay_trunc <- sim_df$delay
  sim_df$delay_trunc[sim_df$delay_trunc > 2 & !is.na(sim_df$delay_trunc)] <- "3+"
  
  # Summary across iters  
  sim_summs <-
          sim_df %>% group_by(iter, 
                              condition) %>% 
    summarize(m=mean(sim_corrs))            

  # Find iters coresponding to best and worst in each condition (marginalizing over decay )
  percentile_by_vp_cond_summarizer <- 
    # Subset with condition
    lapply(split(sim_summs, list(sim_summs$condition)), function(x) {
                                  
                                   # Arrange from best to worst perf  
                                   x <- x %>% arrange(m)
                                   # Careful — only works if you ran 100 iters!!!
                                   assert(nrow(x) == 100)
                                   out <- 
                                     data.frame(x$condition[1], 
                                                x[5, "iter"], x[50, "iter"], x[95, "iter"]) %>% 
                                     setNames(c("condition", 
                                                "iter_5th_percentile",
                                                "iter_50th_percentile",
                                                "iter_95th_percentile"))
                                   
                                   out                        
                                }) %>% bind_rows()
  
    # Then pull using those iters 
    this_vp_summary_cog <- percentile_by_vp_cond_summarizer %>% filter(condition=="cognitive")
    this_vp_summary_overt <- percentile_by_vp_cond_summarizer %>% filter(condition=="overt")
    
    sim_5_cog <- sim_df %>% filter(condition=="cognitive" & iter == this_vp_summary_cog$iter_5th_percentile)
    sim_50_cog <- sim_df %>% filter(condition=="cognitive" & iter == this_vp_summary_overt$iter_50th_percentile)
    sim_95_cog <- sim_df %>% filter(condition=="cognitive" & iter == this_vp_summary_cog$iter_95th_percentile)
    
    sim_5_overt <- sim_df %>% filter(condition=="overt" & iter == this_vp_summary_overt$iter_5th_percentile)
    sim_50_overt <- sim_df %>% filter(condition=="overt" & iter == this_vp_summary_overt$iter_50th_percentile)
    sim_95_overt <- sim_df %>% filter(condition=="overt" & iter == this_vp_summary_overt$iter_95th_percentile)
    
    sim_5_combined <- rbind(sim_5_overt, sim_5_cog)
    sim_50_combined <- rbind(sim_50_overt, sim_50_cog)
    sim_95_combined <- rbind(sim_95_overt, sim_95_cog)
    
    # And finally find means within by sim corr
    sim_5_summs <- 
      sim_5_combined %>% group_by(condition, delay_trunc) %>% summarize(m=mean(sim_corrs))
    sim_50_summs <- 
      sim_50_combined %>% group_by(condition, delay_trunc) %>% summarize(m=mean(sim_corrs))
    sim_95_summs <- 
      sim_95_combined %>% group_by(condition, delay_trunc) %>% summarize(m=mean(sim_corrs))
    
    sims_comb <- na.omit(rbind(data.frame(sim_5_summs, "percentile"="5th"), 
                  data.frame(sim_50_summs, "percentile"="50th"), 
                  data.frame(sim_95_summs, "percentile"="95th")))
    
    this_plot <- ggplot(sims_comb, aes(x=delay_trunc, y=m, group=percentile, color=percentile)) + 
      geom_line(size=2)  + #scale_color_grey(start=.8, end=.2) + 
       
      geom_line(data=emp_summs, inherit.aes=FALSE, 
                 aes(x=delay_trunc, y=m, group=condition, color=condition), size=3) + 
      
      geom_point(data=emp_summs, inherit.aes=FALSE, 
                 aes(x=delay_trunc, y=m, color=condition, color=condition), size=6) +
      ga + ap + lp + tp +
      facet_wrap(~ condition) + ft +
      xlab("Delay") + ylab("Proportion correct") + tol +
      scale_color_manual(values=c("gray57", "gray", "black", "purple", "orange")) + ylim(.62, .8)
    #geom_hline(yintercept = c(seq(.5, 1, .1)))  +
      #geom_hline(yintercept = .5, size=3) 
  
  this_plot
  #} 
  
this_plot
}
