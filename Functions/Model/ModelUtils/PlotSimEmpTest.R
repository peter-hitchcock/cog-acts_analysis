PlotSimEmpTest <- function(emp_df, sim_df, low_range=.1, high_range=.9, title="") {
  ### Returns plots of 1) empirical vs. sim learning curves, where the simulations are 
  # plotted as error bars showing the range of low_range - high_range proportion correct and 
  # 2) simulation ranges overlaid on each other ###
  
  emp_summs <- NULL 
  sim_summs <- NULL
  full_sim_summs <- NULL
  
  emp_summs <- emp_df %>% 
    group_by(condition, valence_and_probability) %>% 
    summarize(m=mean(correct))
  
  # Recode sim variables 
  sim_df$condition <- if_else(sim_df$cond=="cog", "cognitive", "overt")
  sim_df$valence_and_probability <- factor(paste(sim_df$probability, 
                                                 sim_df$valence, sep="_"))
  
  # Summary across iters  
  sim_summs <-
          sim_df %>% group_by(iter, 
                              condition, 
                              valence_and_probability) %>% 
    summarize(m=mean(sit_sim_corrs))            

  # Find range and mean at each {condition, vpwithin} level 
  full_sim_summs <- 
    # Subset with {condition, valence+prob, trial}
    lapply(split(sim_summs, list(sim_summs$condition, 
                                 sim_summs$valence_and_probability)), function(x) {
                                  
                                  # Copy the identifying information (which is the same across all 
                                  # rows of x  
                                  out <- data.frame(x[1, ], 
                                  # Use range to take the upper and lower bound of the quantile range
                                                    t(range(quantile(x$m, seq(.05, .95, .1))))) %>% 
                                    rename(lower=X1, upper=X2)
                                  out$m <- NULL # delete the mean from row 1
                                  out$mean <- mean(x$m)
                                  
                                out                        
                                }) %>% bind_rows()
  
  vps <- c("90-10_reward", "90-10_punishment", "40-10_reward", "40-10_punishment")
  #labels <- c("90-10 R", "90-10 P", "40-10 R", "40-10 P")
  labels <- c("40-10 P", "40-10 R", "90-10 P", "90-10 R")
  x_labs <- unlist(lapply(strsplit(as.character(levels(full_sim_summs$valence_and_probability)), 
                                   "_"), function(x) {
                                     out <- paste0(x, collapse=" ")
                                     out
                                   }))
  
    this_plot <- 
      ggplot(full_sim_summs, 
                     aes(x=valence_and_probability, y=mean, 
                         group=condition, color=condition)) +
      geom_errorbar(aes(ymin=lower, ymax=upper), 
                    size=3, width=.3, position=position_dodge(width=.6))  +
      geom_point(data=emp_summs, 
                aes(x=valence_and_probability, y=m, fill=condition), size=10, pch=21, position=position_dodge(width=.6)) +
      geom_hline(yintercept = c(seq(.5, 1, .1))) +
      geom_hline(yintercept = .5, size=3) +
      ga + ap + lp + tp +
      xlab("") + ylab("Proportion correct") + tol +
        scale_color_manual(values=c("purple", "orange"))  +
        scale_fill_manual(values=c("purple", "orange")) +
        theme(axis.text.x = element_text(size=25)) +
        scale_x_discrete(labels=labels) +
      ggtitle(title)
        #scale_x_discrete(labels=x_labs)
    

this_plot
}


DelaySimEmpPlot <- function(plot_list, model_string="") {
  ### Wrapper — facets plots   ###
  out <- 
    plot_list[[1]] + plot_list[[2]] + 
    plot_list[[3]] + plot_list[[4]] + 
    plot_annotation(title = model_string,
                    theme = theme(plot.title = element_text(size = 25)))
  
out
}