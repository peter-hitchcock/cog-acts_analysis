which_model <- 1
cluster <- 1 #^PUT BACK 
which_study <- 1
testing <- 1
# Only relevant if using empirical Bayes
emp_bayes_verbose <- 0
sim_opt <- "opt" 
with_delay <- 1
# Options for optimization only: use a method that doesn't rely on derivatives and if so which opt routine to use?
# For local runs use `chmod +x rlc.sh` first  
non_diff <- 0
which_or <- 3
# Option for parameter recovery only: how many parameters sets to generate and whether to simulate the
# parameters from a multivariate gaussian
if (sim_opt == "par_recov") {
  n_pars <- 105
  sim_multiv_gauss <- 1
  if (sim_multiv_gauss==1) {
    pr_sim_label <- "mv_gauss" 
  } else {
    pr_sim_label <- "uni_gauss" 
  } 
} 
# ^ SETTINGS  
sapply(c(
  "data.table",
  "dplyr", 
  "ggplot2", 
  "stringr", 
  "purrr",  
  "foreach", 
  "doParallel", 
  "truncnorm", 
  "testit",
  "Rsolnp",
  "mvtnorm"#,
  #"nloptr"
), 
require, character=TRUE)
sf <- function() sapply(paste0("./Functions/", list.files("./Functions/", recursive=TRUE)), source) # Source all fxs
sf()
DefPlotPars()

if (sim_opt=="sim") cluster <- 0
if (testing==1) cluster <- 0

if (cluster==1) {
  registerDoParallel(cores=40)  
} else {
  #registerDoParallel(cores=10)
  #registerDoParallel(cores=round(detectCores()*2/3))
  registerDoParallel(cores=round(detectCores()*1/3))
}

# Read in the data, either with or without delay  
if (which_study == 1) {
  if (!with_delay) {
    training_df <- read.csv("../data/cleaned_files/s1_train_deident.csv") %>% rename(ID=deident_ID)  
  } else {
    training_df <- read.csv("../data/cleaned_files/s1_train_with_delay_deident.csv")
  }
  sit_df <- read.csv("../data/cleaned_files/s1_SIT_deident.csv") %>% rename(ID=deident_ID)
} else if (which_study == 2) {
  if (!with_delay) {
    training_df <- read.csv("../data/cleaned_files/s2_train_deident_corrected_names.csv")   
  } else {
    training_df <- read.csv("../data/cleaned_files/s2_train_with_delay_deident.csv")
  }
  sit_df <- read.csv("../data/cleaned_files/s2_sit_deident_corrected_names.csv")
}

IDs <- unique(training_df$ID)
# To prevent overwriting on save  
rand_part <- round(runif(1, 1e4, 9e4), 0)

# Put some routine parameters into a list called helpers to send into functions  
helpers <- list()
# These are for very verbose print statements to track trial-wise dynamics  
helpers$train_verbose <- 0
helpers$test_verbose <- 0
if (helpers$train_verbose || helpers$test_verbose) library("profvis")
helpers$handle_errors <- 0
helpers$verbose <- 0
helpers$par_recov <- 0 # NEVER SET TO 1 (defunct but built into all models)
helpers$sim_opt <- sim_opt
helpers$on_what <- "train_SIT" 
helpers$turn_on_debug_info <- 0
helpers$print_nll <- 0
helpers$this_fit_df <- NULL 
# Fit using empirical bayes?
helpers$empirical_bayes <- 1
# ALWAYS START THIS AT 0 because will always start w running MLE first — then this will get 
# turned on when actually ready to run empirical Bayes  
helpers$run_empirical_bayes <- 0 
helpers$emp_bayes_verbose <- emp_bayes_verbose
# Whether to use non-gradient optimizer and if so which one 
helpers$non_diff <- non_diff
if (helpers$non_diff) helpers$or <- which_or

# Set up save paths
best_path <- "../model_res/opts_mle_clean/BEST_"
all_path <- "../model_res/opts_mle_clean/ALL_"

sim_path <- "../model_res/sims_clean/sims_from_mle/SIM_"
if (helpers$empirical_bayes == 1) sim_path <- paste0(sim_path, "EMPIRICAL_BAYES_")

# Paths for reading in extant files  
bp <- "../model_res/opts_mle_paper_final/best/"
allp <- "../model_res/opts_mle_paper_final/all/"
sp <- "../model_res/sims_clean/sims_from_mle/"
# Read model function 
rm <- function(path, model_str) read.csv(paste0(path, model_str))

if (which_study == 1) {
  best_path <- paste0(best_path, "study_1_", helpers$on_what, "_")
  all_path <- paste0(all_path, "study_1_", helpers$on_what, "_")
  sim_path <- paste0(sim_path, "study_1_", helpers$on_what, "_")
} else if (which_study == 2) {
  best_path <- paste0(best_path, "study_2_", helpers$on_what, "_")
  all_path <- paste0(all_path, "study_2_", helpers$on_what, "_")
  sim_path <- paste0(sim_path, "study_2_", helpers$on_what, "_")
}

par_names <- NULL 

# All models include a block exploration boost so this is not stated in model names
if (which_model == 1) {
  
  helpers$which_model <- "RunMQLearnerDiffDecayToPessPrior" 
  
  # Get prior fits to simulate with..  
  if (sim_opt=="sim" || 
      # .. or to define range for recovery  
      sim_opt == "par_recov") {
    
    if (helpers$empirical_bayes == 1) {
      if (which_study == 1) {
        fit_df <-
          rm(bp, "BEST_study_1_train_SIT_EMPIRICAL_BAYES_RunMQLearnerDiffDecayToPessPrior_merged.csv")
      } else {
        fit_df <-
          rm(bp, "BEST_study_2_train_SIT_EMPIRICAL_BAYES_RunMQLearnerDiffDecayToPessPrior_merged.csv")
      }
    }
    
  }
  
  helpers$par_names <- c(
    "epsilon", 
    "q_LR",
    "cog_phi",
    "overt_phi",
    "choice_LR",
    "explor_scalar",
    "beta"
  )
  
}


# Same as m1 but decay not separated by condition 
if (which_model == 3) {
  helpers$which_model <- "RunMQLearnerDecayToPessPrior" 
  
  helpers$par_names <- c(
    "epsilon", 
    "q_LR",
    "phi",
    "choice_LR",
    "explor_scalar",
    "beta"
  )
  
  if (sim_opt=="sim" || 
      # .. or to define range for recovery  
      sim_opt == "par_recov") {
    
    if (helpers$empirical_bayes == 1) {
      if (which_study == 1) {
        fit_df <-
          rm(bp, "BEST_study_1_train_SIT_EMPIRICAL_BAYES_RunMQLearnerDecayToPessPrior_merged.csv")
      } else {
        fit_df <-
          rm(bp, "BEST_study_2_train_SIT_EMPIRICAL_BAYES_RunMQLearnerDecayToPessPrior_merged.csv")
      }
    }
  }
    
}

# Same as m3 but decays to 0 instead of pessimistic prior 
if (which_model == 4) {
  helpers$which_model <- "RunMQLearnerDecayTo0Inits" 
  
  # Get prior fits to simulate with..  
  if (sim_opt=="sim" || 
      # .. or to define range for recovery  
      sim_opt == "par_recov") {
    
    if (helpers$empirical_bayes == 1) {
      if (which_study == 1) {
        fit_df <-
          rm(bp, "BEST_study_1_train_SIT_EMPIRICAL_BAYES_RunMQLearnerDecayTo0Inits_merged.csv")
      } else {
        fit_df <-
          rm(bp, "BEST_study_2_train_SIT_EMPIRICAL_BAYES_RunMQLearnerDecayTo0Inits_merged.csv")
      }
    }
    
  }
    
  
  
  helpers$par_names <- c(
    "epsilon", 
    "q_LR",
    "phi",
    "choice_LR",
    "explor_scalar",
    "beta"
  )
  
}

if (which_model == 5) {
  
  helpers$which_model <- "RunMQLearnerDecayToPessPriorDiffBeta" 
  
  helpers$par_names <- c(
    "epsilon", 
    "q_LR",
    "phi",
    "choice_LR",
    "explor_scalar",
    "cog_beta",
    "overt_beta"
  )
  
}



if (which_model == 6) {
  
  helpers$which_model <- "RunMQLearnerDecayToPessPriorDiffLR" 
  
  helpers$par_names <- c(
    "epsilon", 
    "cog_q_LR",
    "overt_q_LR",
    "phi",
    "choice_LR",
    "explor_scalar",
    "beta"
  )
  
}

if (which_model == 11) {
  
  helpers$which_model <- "RunMQLearner" 
  
  helpers$par_names <- c(
    "epsilon", 
    "q_LR",
    "choice_LR",
    "explor_scalar",
    "beta"
  )
  
}

if (which_model == 12) {
  
  helpers$which_model <- "RunMBayesLearner" 
  
  helpers$par_names <- c(
    "epsilon", 
    "explor_scalar",
    "choice_LR",
    "beta"
  )
  
}

if (which_model == 13) {
  
  helpers$which_model <- "RunMBayesLearnerDiffBeta" 
  
  helpers$par_names <- c(
    "epsilon", 
    "explor_scalar",
    "choice_LR",
    "cog_beta",
    "overt_beta"
  )
  
}

# Hybrid model with Q-learner that decays quickly (prefrontal) and integrative learning by Bayes,
# with the Bayes contribution weighted by certainty and varied by condition (so one cond can have less integrative info than the other)
if (which_model == 14) {
  
  #helpers$which_model <- "RunMHybridDecayingQLearnerDiffBayes" 
  helpers$which_model <- "RunMHybridDecayingQLearnerDiffBayesAlt" 
  
  helpers$par_names <- c(
    "epsilon", 
    "explor_scalar",
    "q_LR",
    "phi",
    "rho_overt",
    "rho_cog",
    "choice_LR",
    "beta"
  )
  
  
  # Define the maximum amount of uncertainty 
  helpers$max_uncertainty <- CalcBayesStd(1, 1)
  
}

# Same as m14 but the decay rate varies by cond instead of the integrative contribution  
if (which_model == 15) {
  
  #helpers$which_model <- "RunMHybridDiffDecayQLearnerBayes" 
  helpers$which_model <- "RunMHybridDiffDecayQLearnerBayesAlt" 
  
  helpers$par_names <- c(
    "epsilon", 
    "explor_scalar",
    "q_LR",
    "cog_phi",
    "overt_phi",
    "rho",
    "choice_LR",
    "beta"
  )
  
  # Define the maximum amount of uncertainty 
  helpers$max_uncertainty <- CalcBayesStd(1, 1)
  
}

# Epsilon fixed at (mean(m1_study2_eb$epsilon) + mean(m1_study1_eb$epsilon))/2 = .017
if (which_model == 27) {
  
  helpers$which_model <- "RunMQLearnerDiffDecayToPessPriorESAndEpsFixed" 
  
  # Get prior fits to simulate with..  
  if (sim_opt=="sim" || 
      # .. or to define range for recovery  
      sim_opt == "par_recov") {
    
    if (helpers$empirical_bayes == 1) {
      if (which_study == 1) {
        fit_df <-
          rm(bp, "BEST_study_1_train_SIT_EMPIRICAL_BAYES_RunMQLearnerDiffDecayToPessPriorESAndEpsFixed_merged.csv")
      } else {
        fit_df <-
          rm(bp, "BEST_study_2_train_SIT_EMPIRICAL_BAYES_RunMQLearnerDiffDecayToPessPriorESAndEpsFixed_merged.csv")
      }
    }
    
  }
  
  helpers$par_names <- c(
    "q_LR",
    "cog_phi",
    "overt_phi",
    "choice_LR",
    "beta"
  )
  
}

# Same as m1 but with no choice kernel 
if (which_model == 29) {
  
  helpers$which_model <- "RunMQLearnerDiffDecayToPessPriorNoCK" 
  
  helpers$par_names <- c(
    "epsilon", 
    "q_LR",
    "cog_phi",
    "overt_phi",
    "explor_scalar",
    "beta"
  )
  
  # Get prior fits to simulate with..  
  if (sim_opt=="sim" || 
      # .. or to define range for recovery  
      sim_opt == "par_recov") {
    
    if (helpers$empirical_bayes == 1) {
      if (which_study == 1) {
        fit_df <-
          rm(bp, "BEST_study_1_train_SIT_EMPIRICAL_BAYES_RunMQLearnerDiffDecayToPessPriorNoCK_merged.csv")
      } else {
        fit_df <-
          rm(bp, "BEST_study_2_train_SIT_EMPIRICAL_BAYES_RunMQLearnerDiffDecayToPessPriorNoCK_merged.csv")
      }
    }
    
  }
  
}

helpers$eff_n_pars <- length(helpers$par_names)
# Don't count explor par as a free parameter in the models where it only influences 4 trials 
# (it's just used to adjust for the drop in performance right after the block break) .. 
if ("explor_scalar" %in% helpers$par_names) helpers$eff_n_pars <- helpers$eff_n_pars-1  

# Now naming models exactly based on file name  
this_models_name <- helpers$which_model

# Some additional info before we begin running  
if (non_diff == 1) this_models_name <- 
  paste0(this_models_name, paste0("_nondiff", which_or))

#### OPTIMIZATION TEST BED ####  
if (testing == 1) {
  
  i <- 1
  
  # Get empirical data  
  this_train_df <- training_df %>% filter(ID == IDs[i])
  this_sit_df <- sit_df %>% filter(ID == IDs[i]) 
  
  # Run test optimization 
  if (sim_opt == "opt") {
    helpers$print_nll <- 1 # Print nll by default on test runs 
    tmp_opt <- RunOptTrainSIT(this_train_df, helpers, this_sit_df, n_iters=1)
    print(tmp_opt)  
  } 
  
}

#### FOR REAL RUNS AFTER TESTING ####  
if (testing == 0) {
  
  ## BEGIN REAL OPTIMIZATION ### 
  if (sim_opt=="opt") { # Kick off optimization 
    
    # Create random string to append to save to prevent overwriting then create final file name  
    this_best_path <- paste0(best_path, this_models_name, rand_part, ".csv")
    this_EB_path <- paste0(paste0(best_path, "EMPIRICAL_BAYES_"), this_models_name, rand_part, ".csv")
    this_all_path <- paste0(all_path, this_models_name, rand_part, ".csv")
    
    model_res_full <- 
      foreach (i = 1:length(IDs)) %dopar% { # PUT BACK  
      #foreach (i = 1:12) %do% {
      #foreach (i = 1:8) %dopar% {
        # Get empirical data for this pt 
        this_train_df <- NULL
        this_train_df <- training_df %>% filter(ID == IDs[i])
        
        this_sit_df <- NULL
        this_sit_df <- sit_df %>% filter(ID == IDs[i])
        
        out <- NULL
        
        # Hack because now doing some testing in the "real optimization" section when running EB  
        #if (helpers$empirical_bayes == 1 && helpers$emp_bayes_verbose == 1) helpers$print_nll <- 1
        
        # Run optimization, trying to catch errors if it breaks 
        out <- 
          tryCatch( #^PUT BACK
            error=function(cnd) NA,
            RunOptTrainSIT(this_train_df, helpers, this_sit_df, n_iters=50) # PUT BACK 
            #RunOptTrainSIT(this_train_df, helpers, this_sit_df, n_iters=2)
        )
        
        res_df <- NULL
        
        # This generates an if (!is.na(out)) has more than one elem warning when 
        # iter is > 1 but that's okay because it's typically only one-dimension if got 
        # snagged on NA (and even if not will break below anyway)
        # Save out the results if the optimization ran successfully 
        if (!is.na(out)) {
          res_df <- 
            lapply(out, function(x) {
              
              value_vec <- x$values
              # Following RSolnp CRAN documentation the final value is the optimized one
              nll <- value_vec[length(value_vec)]
              # 0 = converged
              convergence <- x$convergence 
              pars <- x$pars 
              
              mini_df_out <- data.table(t(pars), convergence, nll)
              
              mini_df_out
          }) %>% bind_rows()
           
          # See pre ~2/18 code if want to use the non-diff opt again  
          res_df$ID <- unique(this_train_df$ID)
          
        } # End is.na conditional; otherwise res_df is NULL so should still save without issue 
      
      res_df
      } %>% bind_rows() # End foreach 
    
    ## Save out optimization ## 
    # Save out full first in case find best fit breaks — *These are full MLE fits — no empirical bayes yet*   
    write.csv(model_res_full, this_all_path) ## ** Put back!  
    
    # Find the best fit in case of local minima 
    model_res <- data.frame(model_res_full %>% group_by(ID) %>% slice(which.min(nll))) 
    
    # If empirical Bayes is on, then rerun the model with a prior based on the group statistics 
    if (helpers$empirical_bayes == 1) {
      # Turn this on so that the optimization function will get routed to empirical bayes
      helpers$run_empirical_bayes <- 1 
      
      eb_res_full <- 
        foreach (i = 1:length(IDs)) %dopar% { # PUT BACK  
        #foreach (i = 1:5) %do% {
        #foreach (i = 1:5) %dopar% { # PUT BACK  
          
        # Get empirical data for this pt 
        this_train_df <- NULL
        this_train_df <- training_df %>% filter(ID == IDs[i])
        
        this_sit_df <- NULL
        this_sit_df <- sit_df %>% filter(ID == IDs[i])
        
        #helpers$emp_bayes_verbose <- 1
        
        eb_out <- NULL
        
          # Run all subjects again using the emp bayes penalty 
          eb_out <-
            tryCatch( #PUT BACK
              error=function(cnd) NA,
              RunOptTrainSIT(this_train_df, helpers, this_sit_df, n_iters=20, mle_model_res=model_res)
              #RunOptTrainSIT(this_train_df, helpers, this_sit_df, n_iters=1, mle_model_res=model_res)
            )
          # cat("\nPrint eb out from outside")
          # print(eb_out)
          eb_res_df <- 
            lapply(eb_out, function(x) {
              
              value_vec <- x$values
              # Following RSolnp CRAN documentation the final value is the optimized one
              nll <- value_vec[length(value_vec)]
              # Sanity checks 
              #AIC==2*(helpers$eff_n_pars - log(exp(-nll)))
              #2*(helpers$eff_n_pars - log(exp(-nll)))==2*helpers$eff_n_pars - 2*log(exp(-nll))
              #2*(helpers$eff_n_pars+1 - log(exp(-nll)))==2*(nll + helpers$eff_n_pars+1)
              AIC <- 2*(nll + helpers$eff_n_pars)  
              
              convergence <- x$convergence 
              
              pars <- x$pars 
              
              names(pars) <- helpers$par_names 
              mini_eb_df_out <- data.table(t(pars), convergence, nll, AIC)
              
            mini_eb_df_out
            }) %>% bind_rows()
          
        data.frame(eb_res_df, "ID"=unique(this_train_df$ID))
        } %>% bind_rows()
      
      eb_model_res <- data.frame(eb_res_full %>% group_by(ID) %>% slice(which.min(nll))) 
      
    } # End Emp Bayes conditional 
    
    # Save with appropriate label depending on if empirical bayes  
    if (helpers$empirical_bayes == 0) {
      write.csv(model_res, this_best_path)    
    }
    
    if (helpers$empirical_bayes == 1) {
      cat("\n Writing in empirical bayes")
      write.csv(eb_model_res, this_EB_path)    
    }
    
  }
  ## END REAL OPTIMIZATION ### 
  
  ## BEGIN SIMULATION ### 
  if (sim_opt == "sim") { # Kick off sim  
    
    sim_full <- NULL 
    sim_full <- 
      foreach (i = 1:length(IDs)) %dopar% {
      #foreach (i = 1) %do% {
        
        # Get empirical data for this pt 
        this_train_df <- NULL
        this_train_df <- training_df %>% filter(ID == IDs[i])
        
        this_sit_df <- NULL
        this_sit_df <- sit_df %>% filter(ID == IDs[i])
        
        # Get their fit parameters  
        this_fit_df <- NULL
        this_fit_df <- fit_df %>% filter(ID==IDs[i])
        
        sim_out <- NULL 
        sim_out <- 
          RunSimTrainSIT(this_fit_df, this_train_df, helpers, this_sit_df, n_iters=100, with_delay=with_delay)
          #RunSimTrainSIT(this_fit_df, this_train_df, helpers, this_sit_df, n_iters=1, with_delay=with_delay)
        
      sim_out    
      }
    
    full_sim_train <- list()
    full_sim_sit <- list()
    
    # Store sim results in list  
    for (s in 1:length(sim_full)) {
      full_sim_train[[s]] <- sim_full[[s]]$sim_train
      full_sim_sit[[s]] <- sim_full[[s]]$sim_sit
    }
    
    full_sim_train_df <- full_sim_train %>% bind_rows()
    full_sim_sit_df <- full_sim_sit %>% bind_rows()
    
    # Construct paths incl random string  
    sim_path_train <- paste0(sim_path, "_train_", this_models_name, rand_part, ".csv")
    sim_path_sit <- paste0(sim_path, "_sit_", this_models_name, rand_part, ".csv")
    
    ## Save out sim ## 
    write.csv(full_sim_train_df, sim_path_train)
    write.csv(full_sim_sit_df, sim_path_sit)
    
  } # End if sim conditional
  ## END SIMULATION ### 
  
} # End ! test conditional  

#### BEGIN PARAMETER RECOVERY ####  
if (sim_opt == "par_recov") {
  
  # Set up paths  
  pr_sim_path <- "../model_res/par_recov_clean/pr_sims/SIM_"
  pr_opts_all_path <- "../model_res/par_recov_clean/pr_opts_mle/par_recov_ALL_"
  
  if (helpers$empirical_bayes == 0) {
    pr_opts_best_path <- paste0("../model_res/par_recov_clean/pr_opts_mle/best_pr/par_recov_BEST_",
                                pr_sim_label)
  } else if (helpers$empirical_bayes == 1) {
    pr_opts_best_path <- paste0("../model_res/par_recov_clean/pr_opts_mle/best_pr/par_recov_BEST_EMPIRICAL_BAYES_",
                                pr_sim_label)
  }
  
  if (which_model == 1) {
    
    helpers$which_model <- "RunMQLearnerDiffDecayToPessPrior" 
    
    just_pars <- fit_df %>% select(
      "epsilon", 
      "q_LR",
      "cog_phi",
      "overt_phi",
      "choice_LR",
      "explor_scalar",
      # All the non-standard ones down here 
      "beta"
    )
    
  } 
  
  
  if (which_model == 27) {
    
    helpers$which_model <- "RunMQLearnerDiffDecayToPessPriorESAndEpsFixed" 
    
    just_pars <- fit_df %>% select(
      "q_LR",
      "cog_phi",
      "overt_phi",
      "choice_LR",
      "beta"
    )
    
  }
  
  if (sim_multiv_gauss==0) {
    ## Generate pars to sim ## 
    ## Generate some parameters to simulate concentrated in the range of the empirical fits #
    # from truncated gaussian #
    full_pars_to_sim <- foreach (i = 1:ncol(just_pars)) %do% {
      
      # Set up pars for simulation  
      this_par_name <- names(just_pars[i])
      these_pars <- just_pars[, i]
      
      # Set limits of truncated normal 
      a <- NULL; b <- NULL
      a <- 0; b <- 1
      
      if (this_par_name == "beta") {a <- 1; b <- 50}
      if (this_par_name == "epsilon") b <- .3
      
      pars_to_sim <- 
        data.frame("parameter"=rtruncnorm(n_pars, a=a, b=b, mean=median(these_pars), sd=.5*sd(these_pars)))
      names(pars_to_sim) <- names(just_pars)[i]
      
    pars_to_sim 
    } %>% bind_cols()  
  } else if (sim_multiv_gauss==1) {
    
    full_pars_to_sim <- data.frame(rmvnorm(105, mean=colMeans(just_pars), sigma=cov(just_pars)))
    
    # No negative values
    full_pars_to_sim[full_pars_to_sim < 0] <- 0
    
    # Set bounds for any params we need to worry about  
    full_pars_to_sim[which(full_pars_to_sim$beta <= 1), "beta"] <- 1
    full_pars_to_sim[which(full_pars_to_sim$q_LR > 1), "q_LR"] <- 1
    
  }
  
  ## Set paths ##
  if (which_study == 1) {
    pr_sim_path <- paste0(pr_sim_path, "study_1_", helpers$on_what, "_", helpers$which_model, "_", rand_part, ".csv")
    pr_best_path <- paste0(pr_opts_best_path, "study_1_", helpers$on_what, "_", helpers$which_model, "_", rand_part, ".csv")
    pr_all_path <- paste0(pr_opts_all_path, "study_1_", helpers$on_what, "_", helpers$which_model, "_", rand_part, ".csv")
  } else if (which_study == 2) {
    pr_sim_path <- paste0(pr_sim_path, "study_2_", helpers$on_what, "_", helpers$which_model, "_", rand_part, ".csv")
    pr_best_path <- paste0(pr_opts_best_path, "study_2_", helpers$on_what, "_", helpers$which_model, "_",rand_part, ".csv")
    pr_all_path <- paste0(pr_opts_all_path, "study_2_", helpers$on_what, "_", helpers$which_model, "_", rand_part, ".csv")
  }
  
  ## SIMULATE using those params ... ##
  pr_sims_full <- 
    foreach (j = 1:n_pars) %do% {
 
      # Draw an ID to pull a random set of contingencies from the empirical data 
      IDc <- NULL 
      IDc <- sample(unique(training_df$ID), 1)
      
      these_pars <- full_pars_to_sim[j, ]
      
      this_train_df <- NULL
      this_train_df <- training_df %>% filter(ID == IDc)
      
      this_SIT_df <- NULL
      this_SIT_df <- sit_df %>% filter(ID == IDc)

      pr_sim_out_partial <- 
        RunSimTrainSITForPR(these_pars, this_train_df, helpers, this_SIT_df, n_iters=1)
      
      pr_train_sim_out <- data.frame(pr_sim_out_partial$pr_sim_train, "sim"=j, these_pars)
      pr_sit_sim_out <- data.frame(pr_sim_out_partial$pr_sim_sit, "sim"=j, these_pars)
      
  list("pr_train_sims"=pr_train_sim_out, "pr_sit_sims"=pr_sit_sim_out)
  } # End generate simulations 
  
  ## Combine and save out the sims  
  pr_full_sim_train <- list()
  pr_full_sim_sit <- list()
  for (s in 1:length(pr_sims_full)) {
    pr_full_sim_train[[s]] <- pr_sims_full[[s]]$pr_train_sims
    pr_full_sim_sit[[s]] <- pr_sims_full[[s]]$pr_sit_sims
  }
  
  pr_full_sim_train_df <- pr_full_sim_train %>% bind_rows()
  pr_full_sim_sit_df <- pr_full_sim_sit %>% bind_rows()
  
  # Construct paths incl random string  
  pr_sim_path_train <- paste0(pr_sim_path, "_train_", this_models_name, rand_part, ".csv")
  pr_sim_path_sit <- paste0(pr_sim_path, "_sit_", this_models_name, rand_part, ".csv")
  
  ## Save out sim ## # Put this back  
  # write.csv(pr_full_sim_train_df, pr_sim_path_train)
  # write.csv(pr_full_sim_sit_df, pr_sim_path_sit)
  
  ### RECOVER - ie. optimize on these sims ### 
  helpers$sim_opt <- "opt"
  
  pr_model_res_full <-
    #foreach (i = 1:8) %do% {
    foreach (i = unique(pr_full_sim_train_df$sim)) %dopar% {
    # ^PUT BACK AFTER TESTING
      
      # Get empirical data for this pt 
      this_sim_train_df <- NULL
    
      generative_pars <- pr_full_sim_train_df %>% 
        filter(sim==i) %>% select((names(these_pars))) %>% unique()
      
      this_sim_train_df <- pr_full_sim_train_df %>% 
        filter(sim==i) %>% select(-c(names(these_pars)))
      # NULL true response to make sure using simmed data 
      this_sim_train_df$true_response <- NULL 
      
      this_sim_sit_df <- NULL
      this_sim_sit_df <- pr_full_sim_sit_df %>% 
        filter(sim==i) %>% select(-c(names(these_pars)))
      
      # Make sure that the pars stored in both are same  
      assert(generative_pars == pr_full_sim_sit_df %>% 
               filter(sim==i) %>% select(c(names(these_pars))) %>% unique())
      pr_out <- NULL
      
      if (testing==1) helpers$print_nll <- 1
      
      # Run optimization, trying to catch errors if it breaks 
      pr_out <-
        tryCatch(
        error=function(cnd) NA,
          #RunOptTrainSITPR(this_sim_train_df, helpers, this_sim_sit_df, n_iters=1)
          RunOptTrainSITPR(this_sim_train_df, helpers, this_sim_sit_df, n_iters=100)
        ) #^ PUT BACK EVERYTHING UP TO TRYCATCH AFTER TESTING 
        
      
      pr_res_df <- NULL
      
      # Save out the results if the optimization ran successfully 
      if (!is.na(pr_out)) {
          
          these_pars <- NULL
          these_pars <- generative_pars
          
          pr_res_df <- 
            lapply(pr_out, function(x) {
              
              names(these_pars) <- paste0(names(these_pars), "_simmed")
              value_vec <- x$values
              # Following RSolnp CRAN documentation the final value is the optimized one
              nll <- value_vec[length(value_vec)]
              # 0 = converged
              convergence <- x$convergence 
              pars <- x$pars
              names(pars) <- paste0(names(pars), "_recovered")
              
              mini_pr_df_out <- data.table(t(pars), these_pars, convergence, nll)
              
            mini_pr_df_out
            }) %>% bind_rows()
          
      } # End is.na conditional; otherwise res_df is NULL so should still save without issue
      
      # In the param recov context this is not especially meaningful bc just
      # their task contingencies, but saved in case it's useful for later matching
      pr_res_df$ID <- unique(this_sim_train_df$ID)
      pr_res_df$sim <- i
      
    pr_res_df
    } %>% bind_rows() # End optimization loop 
    
    # ## Save out results ## 
    # Save out full first in case find best fit breaks
    #write.csv(pr_model_res_full, pr_all_path) #^PUT BACK 
    
    pr_model_res <- pr_model_res_full %>% group_by(sim) %>% slice(which.min(nll))
    
    if (helpers$empirical_bayes == 1) {
      # Turn this on so that the optimization function will get routed to empirical bayes
      helpers$run_empirical_bayes <- 1 
        
        eb_pr_res_full <- 
          foreach (i = unique(pr_full_sim_train_df$sim)) %dopar% { # PUT BACK  
          #foreach (i = 1:10) %do% { # Needs to be \leq the number from the MLE PR loop
            
            # Get simulated data for this pt 
            this_sim_train_df <- NULL
            
            generative_pars <- pr_full_sim_train_df %>% 
              filter(sim==i) %>% select((names(these_pars))) %>% unique()
            
            this_sim_train_df <- pr_full_sim_train_df %>% 
              filter(sim==i) %>% select(-c(names(these_pars)))
            # NULL true response to make sure using simmed data 
            this_sim_train_df$true_response <- NULL 
            
            this_sim_sit_df <- NULL
            this_sim_sit_df <- pr_full_sim_sit_df %>% 
              filter(sim==i) %>% select(-c(names(these_pars)))
            
            # Make sure that the pars stored in both are same  
            assert(generative_pars == pr_full_sim_sit_df %>% 
                     filter(sim==i) %>% select(c(names(these_pars))) %>% unique())
            
            if (testing==1) helpers$print_nll <- 1
            
            eb_pr_out <- NULL
            
            mle_recovered <- data.frame(pr_model_res %>% select(contains("recovered")))
            names(mle_recovered) <- 
              unlist(strsplit(names(mle_recovered), "_recovered"))
            
            # Run all subjects again using the emp bayes penalty 
            eb_pr_out <-
              tryCatch( #PUT BACK
                error=function(cnd) NA,
                #RunOptTrainSITPR(this_sim_train_df, helpers, this_sim_sit_df, n_iters=2, mle_model_res=mle_recovered)
                RunOptTrainSITPR(this_sim_train_df, helpers, this_sim_sit_df, n_iters=40, mle_model_res=mle_recovered)
            )
            print(eb_pr_out)
            
          if (!is.na(eb_pr_out)) {
            eb_pr_res_df <- 
              lapply(eb_pr_out, function(x) {
                
                value_vec <- x$values
                # Following RSolnp CRAN documentation the final value is the optimized one
                nll <- value_vec[length(value_vec)]
                
                convergence <- x$convergence 
                
                pars <- x$pars 
                
                names(pars) <- helpers$par_names 
                
                names(pars) <- paste0(names(pars), "_EB_recovered")
                
                mini_eb_pr_df_out <- data.table(t(pars), "eb_convergence"=convergence, "eb_nll"=nll)
                print(mini_eb_pr_df_out)
                
                mini_eb_pr_df_out
              }) %>% bind_rows()
          } else {
            eb_pr_res_df <- NA
          }
          
          this_sim_full_recovs_out <- 
            data.frame(eb_pr_res_df, data.frame(pr_model_res %>% filter(sim==i)), "eb_ID"=unique(this_sim_train_df$ID))
          
        this_sim_full_recovs_out   
        } #%>% bind_rows()
      #cat("\n Made it here")
      eb_pr_res_full <- eb_pr_res_full %>% bind_rows()
      eb_pr_model_res <- data.frame(eb_pr_res_full %>% group_by(sim) %>% slice(which.min(nll))) 
      
      write.csv(eb_pr_model_res, pr_best_path)   
      
    } else { # If not EB  
      write.csv(pr_model_res, this_best_path)    
    } 
    
} # End param recover conditional 

