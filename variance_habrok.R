###############################################################################
# Simulation: Social Net Acculturation Model - Degree Variance Experiment (Hábrók)
# Version: Final trait frequencies
# Author: Flora Murakeozy-Kis
# Date: July 2025
#
# Description:
# This script runs a simulation to assess how degree variance affects 
# cultural trait retention under migration in a population with a heterogenous 
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

#############################################################################
############# PARAMETERS & PACKAGES #########################################
#############################################################################

# General simulation parameters
popsize <- 1000
c_r <- 0.9
time_steps <- 10^6
int_prob_other <- 0.1

# Sweep ranges
migration_rates <- seq(from = 0.001, to = 0.1, by = 0.01)
c_i_s <- seq(0, 0.9, 0.1)

# Network properties
mean_degree <- 4
var_degree <- c(10, 20, 30)

# Package handling
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

experiment_number <- 2    # Experiment 2: Variance
sim_type <- 1             # Normal version
interaction_code <- 1     # 0.5 interaction probability
model_code <- 3           # Network

# Variant codes: 1 = variance 10, 2 = 20, 3 = 30
variant_codes <- list("10" = 1, "20" = 2, "30" = 3)

#############################################################################
############# POPULATION GENERATION #########################################
#############################################################################

make_neg_binom_network <- function(popsize, mean_degree, var_degree) {
  size <- (mean_degree)^2 / (var_degree - mean_degree)
  degrees <- rnbinom(popsize, size = size, mu = mean_degree)
  degrees[degrees == 0] <- 1
  if (sum(degrees) %% 2 != 0) degrees[which.max(degrees)] <- degrees[which.max(degrees)] + 1
  graph <- sample_degseq(degrees, method = "simple.no.multiple")
  adj_list <- lapply(1:popsize, function(v) as.integer(neighbors(graph, v)))
  list(graph_net = graph, network_pop = rep("resident", popsize), adj_list_net = adj_list)
}

#############################################################################
############# SIMULATION FUNCTION ###########################################
#############################################################################

run_network_simulation <- function(output_dir, file_name, variance, run_number, variant_code) {
  results_net <- expand.grid(mig_rate = migration_rates, c_i = c_i_s)
  results_net$resident_fraction <- NA_real_
  results_net$seed_used <- NA_integer_
  
  base_seed <- experiment_number * 1e6 +
    sim_type         * 1e5 +
    interaction_code * 1e4 +
    model_code       * 1e3 +
    variant_code     * 1e2 +
    run_number
  
  tic(paste("Network Simulation for variance =", variance))
  for (i in seq_len(nrow(results_net))) {
    set.seed(base_seed + i)
    
    mig_rate <- results_net$mig_rate[i]
    c_i <- results_net$c_i[i]
    
    data <- make_neg_binom_network(popsize, mean_degree, variance)
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

#############################################################################
############# RUN SIMULATIONS ###############################################
#############################################################################

output_dir <- "data_var"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

for (var in var_degree) {
  variant_code <- variant_codes[[as.character(var)]]
  for (i in 1:10) {
    run_network_simulation(output_dir, paste0("data_var", var, "_run", i, ".RDS"),
                          var, i, variant_code)
  }
}
