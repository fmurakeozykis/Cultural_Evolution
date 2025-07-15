###############################################################################
# Simulation: Social Net Acculturation Model - Population Social Structure 
# Experiment (Hábrók)
# Version: Final trait frequencies
# Author: Flora Murakeozy-Kis
# Date: July 2025
#
# Description:
# This script runs a simulation to assess how population social structure affects 
# cultural trait retention under migration comparatively in a well-mixed population,
# a population with a homogenous network and a population with a heterogenous 
# (negative binomial) degree distribution, recording final cultural trait frequencies.
#
# To run this script on Hábrók: 
# - Load R 4.4.2 via the appropriate module
# - Assign local working directory
# - Submit via a SLURM batch script
#
# Outputs:
# - .RDS files with  final trait frequencies and seed numbers saved in the specified 
#    results directory.
#
# Part of: Cultural_Evolution repository (habrok branch)
###############################################################################

# Cultural Evolution Simulations - Final Version
setwd("/home3/s5194326/floras_directory/basic/results")

# PARAMETERS & PACKAGES ---------------------------------------------------

pop_size <- 1000
c_r <- 0.9
time_steps <- 10^6
int_prob_other <- 0.1

migration_rates <- seq(from = 0.001, to = 0.1, by = 0.01)
c_i_s <- seq(0, 0.9, 0.1)

mean_degree <- 4
var_degree <- 10

required_packages <- c("igraph", "tictoc", "here")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)

# SEED SYSTEM -------------------------------------------------------------
experiment_number <- 5    # Experiment 1: Pop size
sim_type <- 1             # Normal version
interaction_code <- 1     # 0.1 interaction prob

variant_code <- 0

# POPULATION FUNCTIONS -----------------------------------------------------

make_wellmixed_pop <- function(popsize, trait) {
  data.frame(individual = 1:popsize, trait = rep(trait, popsize))
}

make_neg_binom_network <- function(popsize, mean_degree, var_degree) {
  size <- (mean_degree)^2 / (var_degree - mean_degree)
  degrees <- rnbinom(popsize, size = size, mu = mean_degree)
  degrees[degrees == 0] <- 1
  if (sum(degrees) %% 2 != 0) degrees[which.max(degrees)] <- degrees[which.max(degrees)] + 1
  graph <- sample_degseq(degrees, method = "simple.no.multiple")
  adj_list <- lapply(1:popsize, function(v) as.integer(neighbors(graph, v)))
  list(graph_net = graph, network_pop = rep("resident", popsize), adj_list_net = adj_list)
}

make_homogeneous_network <- function(popsize, mean_degree) {
  graph <- sample_k_regular(n = popsize, k = mean_degree, directed = FALSE, multiple = FALSE)
  adj_list <- lapply(1:popsize, function(v) as.integer(neighbors(graph, v)))
  list(graph = graph, hom_pop = rep("resident", popsize), adj_list_hom = adj_list)
}

# SIMULATION FUNCTIONS -----------------------------------------------------

run_wellmixed_simulation <- function(output_dir, file_name, run_number, variant_code, popsize) {
  model_code <- 1
  base_seed <- experiment_number * 1e6 +
    sim_type         * 1e5 +
    interaction_code * 1e4 +
    model_code       * 1e3 +
    variant_code     * 1e2 +
    run_number
  
  results_wm <- expand.grid(mig_rate = migration_rates, c_i = c_i_s)
  results_wm$resident_fraction <- NA_real_
  results_wm$seed_used <- NA_integer_
  
  tic("Well-mixed Simulation")
  for (i in seq_len(nrow(results_wm))) {
    set.seed(base_seed + i)
    
    mig_rate <- results_wm$mig_rate[i]
    c_i <- results_wm$c_i[i]
    pop <- make_wellmixed_pop(popsize, "resident")
    
    for (t in seq_len(time_steps)) {
      is_migrating <- runif(1) < mig_rate
      focal <- sample(popsize, 1)
      
      if (is_migrating) {
        pop$trait[focal] <- "immigrant"
      } else {
        repeat {
          partner <- sample(popsize, 1)
          if (partner != focal) break
        }
        if (pop$trait[focal] != pop$trait[partner] && runif(1) < int_prob_other) {
          change_prob <- ifelse(pop$trait[focal] == "resident", 1 - c_r, 1 - c_i)
          if (runif(1) < change_prob) {
            pop$trait[focal] <- ifelse(pop$trait[focal] == "resident", "immigrant", "resident")
          }
        }
      }
    }
    
    results_wm$resident_fraction[i] <- sum(pop$trait == "resident") / popsize
    results_wm$seed_used[i] <- base_seed + i
  }
  toc()
  
  saveRDS(results_wm, file = file.path(output_dir, file_name))
}

run_homogeneous_simulation <- function(output_dir, file_name, run_number, variant_code, popsize) {
  model_code <- 2
  base_seed <- experiment_number * 1e6 +
    sim_type         * 1e5 +
    interaction_code * 1e4 +
    model_code       * 1e3 +
    variant_code     * 1e2 +
    run_number
  
  results_hom <- expand.grid(mig_rate = migration_rates, c_i = c_i_s)
  results_hom$resident_fraction <- NA_real_
  results_hom$seed_used <- NA_integer_
  
  tic("Homogeneous Simulation")
  for (i in seq_len(nrow(results_hom))) {
    set.seed(base_seed + i)
    
    mig_rate <- results_hom$mig_rate[i]
    c_i <- results_hom$c_i[i]
    
    data <- make_homogeneous_network(popsize, mean_degree)
    pop <- data$hom_pop
    adj <- data$adj_list_hom
    
    for (t in seq_len(time_steps)) {
      is_migrating <- runif(1) < mig_rate
      focal <- sample.int(popsize, 1)
      
      if (is_migrating) {
        pop[focal] <- "immigrant"
      } else {
        neighbor <- adj[[focal]][sample.int(length(adj[[focal]]), 1)]
        if (pop[focal] != pop[neighbor] && runif(1) < int_prob_other) {
          change_prob <- ifelse(pop[focal] == "resident", 1 - c_r, 1 - c_i)
          if (runif(1) < change_prob) {
            pop[focal] <- ifelse(pop[focal] == "resident", "immigrant", "resident")
          }
        }
      }
    }
    
    results_hom$resident_fraction[i] <- sum(pop == "resident") / popsize
    results_hom$seed_used[i] <- base_seed + i
  }
  toc()
  
  saveRDS(results_hom, file = file.path(output_dir, file_name))
}

run_network_simulation <- function(output_dir, file_name, run_number, variant_code, popsize) {
  model_code <- 3
  base_seed <- experiment_number * 1e6 +
    sim_type         * 1e5 +
    interaction_code * 1e4 +
    model_code       * 1e3 +
    variant_code     * 1e2 +
    run_number
  
  results_net <- expand.grid(mig_rate = migration_rates, c_i = c_i_s)
  results_net$resident_fraction <- NA_real_
  results_net$seed_used <- NA_integer_
  
  tic("Network Simulation")
  for (i in seq_len(nrow(results_net))) {
    set.seed(base_seed + i)
    
    mig_rate <- results_net$mig_rate[i]
    c_i <- results_net$c_i[i]
    
    data <- make_neg_binom_network(popsize, mean_degree, var_degree)
    pop <- data$network_pop
    adj <- data$adj_list_net
    
    for (t in seq_len(time_steps)) {
      is_migrating <- runif(1) < mig_rate
      focal <- sample.int(popsize, 1)
      
      if (is_migrating) {
        pop[focal] <- "immigrant"
      } else {
        neighbor <- adj[[focal]][sample.int(length(adj[[focal]]), 1)]
        if (pop[focal] != pop[neighbor] && runif(1) < int_prob_other) {
          change_prob <- ifelse(pop[focal] == "resident", 1 - c_r, 1 - c_i)
          if (runif(1) < change_prob) {
            pop[focal] <- ifelse(pop[focal] == "resident", "immigrant", "resident")
          }
        }
      }
    }
    
    results_net$resident_fraction[i] <- sum(pop == "resident") / popsize
    results_net$seed_used[i] <- base_seed + i
  }
  toc()
  
  saveRDS(results_net, file = file.path(output_dir, file_name))
  
}

# RUN ALL SIMULATIONS ------------------------------------------------------

output_dir <- "data_popstruc"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

for (i in 1:10) {
  file_name <- paste0("data_popstruc_wm_run", i, ".RDS")
  run_wellmixed_simulation(output_dir, file_name = file_name, run_number = i, variant_code = variant_code)
}

for (i in 1:10) {
  file_name <- paste0("data_popstruc_hom_run", i, ".RDS")
  run_homogeneous_simulation(output_dir, file_name = file_name, run_number = i, variant_code = variant_code)
}

for (i in 1:10) {
  file_name <- paste0("data_popstruc_net_run", i, ".RDS")
  run_network_simulation(output_dir, file_name = file_name, run_number = i, variant_code = variant_code)
}
