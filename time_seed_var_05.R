###############################################################################
# Simulation: Social Net Acculturation Model - Variance Experiment (Hábrók)
# Version: Timeseries
# Author: Flora Murakeozy-Kis
# Date: July 2025
#
# Description:
# This script runs a simulation to assess how degree variance in a social
# network with a negative binomial degree distribution affects cultural trait 
# retention under migration, recording cultural trait frequencies over time.
#
# To run this script on Hábrók: 
# - Load R 4.4.2 via the appropriate module
# - Assign local working directory
# - Submit via a SLURM batch script
#
# Outputs:
# - .RDS files with time series and seed numbers saved in the specified 
#    results directory.
#
# Usage:
# - Modify parameters under "GENERAL SETTINGS" as needed
#
# Part of: Cultural_Evolution repository (habrok branch)
###############################################################################


#############################################################################
############# PARAMETERS & PACKAGES #########################################
#############################################################################

# GENERAL SETTINGS
popsize <- 1000
c_r <- 0.9
time_steps <- 10^6
int_prob_other <- 0.5

# SWEEPING RANGES
migration_rates <- seq(from = 0.001, to = 0.1, by = 0.01)
c_i_s <- seq(0, 0.9, 0.1)

# NETWORK
mean_degree <- 4
var_degree <- c(10, 20, 30)

# TIME SERIES RECORDING
record_interval <- 1000
num_records <- time_steps / record_interval

# PACKAGES
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
sim_type <- 2             # Time version
interaction_code <- 2     # 0.5 interaction probability
model_code <- 3           # Network model

variant_codes <- list("10" = 1, "20" = 2, "30" = 3)

#############################################################################
############# NETWORK CONSTRUCTION ##########################################
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

save_simulation <- function(output_path, meta_results, ts_data) {
  saveRDS(list(metadata = meta_results, time_series = ts_data), file = output_path)
}

run_network_simulation <- function(output_path, variance, run_number, variant_code) {
  results <- expand.grid(mig_rate = migration_rates, c_i = c_i_s)
  results$seed_used <- NA_integer_
  time_series_list <- vector("list", nrow(results))
  
  base_seed <- experiment_number * 1e6 +
    sim_type         * 1e5 +
    interaction_code * 1e4 +
    model_code       * 1e3 +
    variant_code     * 1e2 +
    run_number
  
  tic("Network Simulation")
  for (i in seq_len(nrow(results))) {
    set.seed(base_seed + i)
    mig_rate <- results$mig_rate[i]
    c_i <- results$c_i[i]
    
    data <- make_neg_binom_network(popsize, mean_degree, variance)
    pop <- data$network_pop
    adj <- data$adj_list_net
    res_frac <- numeric(num_records)
    
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
      
      if (t %% record_interval == 0) {
        res_frac[t / record_interval] <- sum(pop == "resident") / popsize
      }
    }
    
    results$seed_used[i] <- base_seed + i
    time_series_list[[i]] <- data.frame(
      time = seq(record_interval, time_steps, by = record_interval),
      resident_fraction = res_frac,
      mig_rate = mig_rate,
      c_i = c_i,
      sim_id = i
    )
  }
  toc()
  save_simulation(output_path, results, time_series_list)
}

#############################################################################
############# RUN SIMULATIONS ###############################################
#############################################################################

output_dir <- "time_seed_var_net05_r"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

for (var in var_degree) {
  variant_code <- variant_codes[[as.character(var)]]
  for (i in 1:10) {
    output_file <- file.path(output_dir, paste0("s_time_net05_var", var, "_run", i, ".RDS"))
    run_network_simulation(
      output_path = output_file,
      variance = var,
      run_number = i,
      variant_code = variant_code
    )
  }
}
