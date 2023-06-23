DefPlotPars <- function() {
  ### Set up some plot aspects we'll reuse across functions ####
  
  # Notes: Invoked only for side effect of returning general plot pars
  # to the global environment ###
  ga <<- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
               panel.background = element_blank(), axis.line = element_line(colour = "black"))
  
  # legend pars #
  lp <<- theme(legend.text = element_text(size = 20),
               legend.title = element_blank(),
               legend.key.size = unit(2.5, 'lines'))
  
  # turn off legend
  tol <<- theme(legend.position='none')
  
  # axis pars #
  ap <<- theme(axis.text = element_text(size=22),
               axis.title = element_text(size=25))
  # title pars #
  tp <<- theme(plot.title = element_text(size = 22, hjust = .5))
  
  stp <<- theme(plot.subtitle = element_text(size=18))
  
  # facet pars # 
  ft <<- theme(strip.text.x = element_text(size = 30))
  
  # color pars #
  cf_vals <<- c('orange', 'purple')
}

GenRandString <- function() round(runif(1, 1000, 9999))

InvLogit <- function(x) exp(x)/(1 + exp(x))

GenSingleSubjPlot <- function(single_subj_df) {
  cog_df <- FindRunningAverage(single_subj_df[single_subj_df$condition=="cognTraining", ])
  
  cp <- ggplot(cog_df, aes(x=trial_within_condition, y=running_average, group=valence_and_probability, color=valence_and_probability)) + 
    geom_hline(yintercept = .5) +
    geom_vline(xintercept = 20, linetype="dotted") +
    geom_line(size=2) + ga + ap  + tp + lp + 
    xlab("trial (within condition)") + ylab("running average") + 
    ggtitle("Cognitive") + ylim(0, 1)
  
  overt_df <- FindRunningAverage(single_subj_df[single_subj_df$condition=="overtTraining", ])
  
  op <- ggplot(overt_df, aes(x=trial_within_condition, y=running_average, group=valence_and_probability, color=valence_and_probability)) + 
    geom_hline(yintercept = .5) +
    geom_vline(xintercept = 20, linetype="dotted") +
    geom_line(size=2) + ga + ap  + tol + tp + 
    xlab("trial (within condition)") + ylab("running average") + 
    ggtitle("Overt") + ylim(0, 1)
cp / op
}


TrCompleteAndBind <- function(bt) lapply(bt, function(x) if (nrow(x) == 320) x) %>% bind_rows()



SITCompleteAndBind <- function(b_sit) {
  
  full_out <- lapply(b_sit, function(x) {
    
    if (!is.null(x)) {
      out <- x
      # as above 
      # if (nrow(x) == 64) {
      #   out <- x
      # } else {
      #   out <- NULL
      # } 
    } else { # if is null 
      out <- NULL
    }
    
    out
  }) #%>% bind_rows()
  trimmed_out <- full_out %>% discard(is.null) 
  
  # With incomplete datasets, need to change from automatic typing to allow bind (because some
  # just have numbers to auto-typed as double)  
  type_changed_out <- lapply(full_out, function(x) { 
    x$response <- factor(x$response)
    x
  }) 
  
  type_changed_out <- type_changed_out %>% bind_rows()
  type_changed_out
}


ComparePars <- function(model1_par, model2_par, model_char="", xchar="", ychar="", transparency=1, use_identity_line=1) {
  ### Plots parameters from parameters comparable in some way, with # 
  # an identity line for the model 1 par to facilitate comparison (so 
  # if there's a baseline model in some way should be entered as model 1) ###
  df <- data.frame(model1_par, model2_par)
  
  ctest <- cor.test(model1_par, model2_par)
  r <- round(ctest$estimate, 2)
  p <- round(ctest$p.value, 2)
  if (p == 0) p = "< .01"
  str <- paste("r =", r, "p =", p)
  
  p1 <- ggplot(df, aes(x=model1_par, model2_par)) + 
    geom_point(size=3, alpha=transparency) + 
    ga + ap + tp + 
    stp +
    ggtitle(model_char, subtitle=str) +
    ylab(ychar) + xlab(xchar) 
  
  if (use_identity_line) p1 <- p1 + geom_line(aes(y=model1_par)) 
  
p1
}

CreatePRPlot <- function(simmed_values, recovered_values, parameter_chr, stat_pos=.9) {
  ### Take in some values and create a parameter recovery plot ###
 
  this_eb_pr_df <- NULL
  this_eb_pr_df <- 
    data.frame("simulated"=simmed_values,
               "EB_recovered"=recovered_values)
  
  ggplot(this_eb_pr_df, aes(x=simulated, y=EB_recovered)) +
    geom_line(aes(x=simulated, y=simulated), size=4) +
    geom_point(size=8, pch=21, fill="gray57", alpha=.8) + 
    stat_cor(method="spearman", size=6, label.y=stat_pos) +
    ggtitle(TeX(parameter_chr)) +
    ga + ap + tp  + 
    ylab("Recovered") + xlab("Simulated")
  
}

# PlotTrainOrT1Acc(pilot_train, block=1, label="Empirical", tol=tol)  +
#   PlotTrainOrT1Acc(all_sims_big, block=1, label="Simulated", plot_ind_subjs=0)
# 
# PlotTrainOrT1Acc(pilot_train, block=2, label="Empirical", tol=tol)  +
#   PlotTrainOrT1Acc(all_sims_big, block=2, label="Simulated", plot_ind_subjs=0)

#PlotTrainOrT1Acc(pilot_train, block=1)