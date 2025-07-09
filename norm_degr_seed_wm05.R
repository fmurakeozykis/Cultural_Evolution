setwd("/home3/s5194326/floras_directory/degr.compare/results/seed/normal")

#############################################################################
############# PARAMETERS & PACKAGES #########################################
#############################################################################

popsize <- 1000
c_r <- 0.9
time_steps <- 10^6
int_prob_other <- 0.5

migration_rates <- seq(from = 0.001, to = 0.1, by = 0.01)
c_i_s <- seq(0, 0.9, 0.1)

required_packages <- c("igraph", "here", "tictoc")
invisible(lapply(required_packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("Package", pkg, "is not installed. Please install before running on Hábrók."))
  }
  library(pkg, character.only = TRUE)
}))

#############################################################################
############# SEED SYSTEM ###################################################
#############################################################################

experiment_number <- 3    # Experiment 3: Mean degree (still used to stay consistent)
sim_type <- 1             # Normal version
interaction_code <- 2     # 0.5 interaction probability
model_code <- 1           # Well-mixed
variant_code <- 0         # No varying variable in this context

#############################################################################
############# POPULATION GENERATION #########################################
#############################################################################

make_wellmixed_pop <- function(popsize, trait) {
  data.frame(
    individual = 1:popsize,
    trait = rep(trait, popsize)
  )
}

#############################################################################
############# SIMULATION FUNCTION ###########################################
#############################################################################

run_wellmixed_simulation01 <- function(output_path, run_number) {
  results_wm <- expand.grid(
    mig_rate = migration_rates,
    c_i = c_i_s
  )
  
  results_wm$resident_fraction <- NA_real_
  results_wm$seed_used <- NA_integer_
  
  base_seed <- experiment_number * 1e6 +
    sim_type         * 1e5 +
    interaction_code * 1e4 +
    model_code       * 1e3 +
    variant_code     * 1e2 +
    run_number
  
  tic(paste("Well-mixed Simulation Run", run_number))
  for (i in seq_len(nrow(results_wm))) {
    set.seed(base_seed + i)
    
    mig_rate <- results_wm$mig_rate[i]
    c_i <- results_wm$c_i[i]
    
    wellmixed_pop <- make_wellmixed_pop(popsize, "resident")
    
    for (t in seq_len(time_steps)) {
      is_migrating <- runif(1) < mig_rate
      focal_ind <- sample(popsize, 1)
      
      if (is_migrating) {
        wellmixed_pop$trait[focal_ind] <- "immigrant"
      } else {
        repeat {
          int_part <- sample(popsize, 1)
          if (int_part != focal_ind) break
        }
        if (wellmixed_pop$trait[focal_ind] != wellmixed_pop$trait[int_part] &&
            runif(1) < int_prob_other) {
          change_prob <- if (wellmixed_pop$trait[focal_ind] == "resident") (1 - c_r) else (1 - c_i)
          if (runif(1) < change_prob) {
            wellmixed_pop$trait[focal_ind] <- ifelse(
              wellmixed_pop$trait[focal_ind] == "resident", "immigrant", "resident"
            )
          }
        }
      }
    }
    
    results_wm$resident_fraction[i] <- sum(wellmixed_pop$trait == "resident") / popsize
    results_wm$seed_used[i] <- base_seed + i
  }
  toc()
  
  saveRDS(results_wm, file = output_path)
}

#############################################################################
############# RUN SIMULATIONS ###############################################
#############################################################################

output_dir <- "s_deg_wm05_normal_r"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

for (i in 1:10) {
  output_file <- file.path(output_dir, paste0("s_norm_wm05_deg_run_", i, ".RDS"))
  run_wellmixed_simulation01(output_path = output_file, run_number = i)
}

