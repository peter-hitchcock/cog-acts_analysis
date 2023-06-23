PlotAllWorstTestOptionsSimVsEmp <- function(emp_test_df, sim_test_df,
                                            m_string="",
                                            sim_low_range=.05, sim_high_range=.95) {
  ### Plot the number of times that the participant vs. model chose 
  # the incorrect choice on every choice within a {condition, contingency}, where
  # contingency = valence-probability. For instance, the number of times they 
  # picked the incorrect choice on 40-10  punishment in each condtion. 
  # So far this has seemed to be the key behavioral signature of the choice 
  # kernel (presumably in part reflecting why it so dominates in model 
  # comparison) — only choice kernel models have been able to capture 
  # the high rate of picking the worst choice at test (although more subtle 
  # choice kernel bxal signatures during the learning phase are that tend to appropriately decrease the 
  # slope of learning and capture the lowest points of lean esp punishment perf) ###
  
  
  # Summarize the empirical test data (point estimate of the data marginalizing over ID, of 
  # same data size as one sim)    
  emp_test_summs <- emp_test_df %>% 
    group_by(ID, condition, valence_and_probability) %>% 
    summarize(m=mean(correct))
  
  emp_test_summs$poor <- if_else(emp_test_summs$m < .125, 1, 0)
  emp_prop_poor <- 
    emp_test_summs %>% group_by(valence_and_probability, condition) %>% summarize(m=mean(poor))
  
  lab_list <- strsplit(emp_prop_poor$valence_and_probability, "_")
  labels <- unlist(foreach(i = c(2, 4, 6, 8)) %do% paste(lab_list[[i]][1], lab_list[[i]][2]))
  
  # Do the same thing for each iteration of the simulated df  
  sim_test_df$condition <- sim_test_df$cond
  
  sims_poor <- foreach (i=unique(sim_test_df$iter)) %do% {
    tmp_summs <- NULL; this_sim_prop_poor <- NULL
    
    tmp_summs <- sim_test_df %>% filter(iter==i) %>% 
      group_by(ID, condition, valence_and_probability) %>% summarize(m=mean(sit_sim_corrs))
    
    tmp_summs$poor <- if_else(tmp_summs$m < .125, 1, 0)
    
    this_sim_prop_poor <- 
      tmp_summs %>% group_by(valence_and_probability, condition) %>% summarize(m=mean(poor))
    
    data.frame(this_sim_prop_poor, iter=i)
  } %>% bind_rows()
  
  # Ensure same ordering of x-axis
  assert(all(emp_prop_poor$valence_and_probability==sims_poor$valence_and_probability[1:8]))
  
  emp_plot <- ggplot(emp_prop_poor,
                     aes(x=valence_and_probability, y=m, 
                         group=condition, color=condition)) + 
    geom_jitter(size=6, width=.1, height=0) + 
    scale_color_manual(values=rev(cf_vals)) + ga + ap + tol + tp + 
    ggtitle("Empirical") + 
    theme(axis.text.x = element_text(angle=30, hjust=1, size=18)) + 
    ylab("") + xlab("") + ylim(-.02, .23) + scale_x_discrete(labels=labels)
  
  sim_plot <- ggplot(sims_poor,
         aes(x=valence_and_probability, y=m, 
             group=condition, color=condition)) + 
    geom_jitter(height = 0.01, width = .25, alpha=.5) + 
    scale_color_manual(values=rev(cf_vals)) + ga + ap + tol + tp + 
    ggtitle(m_string) + 
    theme(axis.text.x = element_text(angle=30, hjust=1, size=18)) + 
    ylab("") + xlab("") + ylim(-.02, .23) + scale_x_discrete(labels=labels)
  
  
list(emp_plot, sim_plot)
}