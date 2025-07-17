###############################################################################
# Simulation: Social Net Acculturation Model - Population Size Experiment (Hábrók)
# Version: Final trait frequencies
# Author: Flora Murakeozy-Kis
# Date: July 2025
#
# Description:
# This script runs a simulation to assess how population size affects cultural 
# trait retention under migration comparatively in a well-mixed, both netowrks 
# with a homogeneous and heterogeneous (negative binomial) degree distributions, 
# recording final trait frequencies.
#
# The timeseries versions record both the final resident trait frequencies, as 
# well as the the frequencies over time with an adjusteable precision. For a faster
# access to data, the final trait frequency version is recommended. 
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
# Part of: Cultural_Evolution repository (habrok branch)
###############################################################################

# PARAMETERS ----------------------------------------------------------

## General settings
pop_sizes <- c(500, 1000, 1500)
c_r <- 0.9
time_steps <- 10^6
## General settings
int_prob_other <- 0.1

## Sweeping ranges
migration_rates <- seq(from = 0.001, to = 0.1, by = 0.01)
c_i_s <- seq(0, 0.9, 0.1)

## Settings for degree distribution of NB network
mean_degree <- 4
var_degree <- 10

# PACKAGES ---------------------------------------------------------------
required_packages <- c("igraph", "here", "tictoc")
invisible(lapply(required_packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("Package", pkg, "is not installed. Please install before running on Hábrók."))
  }
  library(pkg, character.only = TRUE)
}))

# SEED  SYSTEM -------------------------------------------------------------
## Ensures a new random seed is used for every run, throughout all experiments.

experiment_number <- 1    # Experiment 1: Pop size
sim_type <- 1             # Final frequencies version -> 1; Timeseries version -> 2
interaction_code <- 1     # Interaction tendency 0.1 -> 1; 0.5 -> 2

# Variant codes: 1 = popsize 500, 2 = 1000, 3 = 1500
variant_codes <- list("500" = 1, "1000" = 2, "1500" = 3)

# POPULATION FUNCTIONS -----------------------------------------------------
# Generates the well-mixed population
make_wellmixed_pop <- function(popsize, trait) {
  data.frame(individual = 1:popsize, trait = rep(trait, popsize))
}
# Generates the heterogeneous network population
make_neg_binom_network <- function(popsize, mean_degree, var_degree) {
  size <- (mean_degree)^2 / (var_degree - mean_degree)
  degrees <- rnbinom(popsize, size = size, mu = mean_degree)
  degrees[degrees == 0] <- 1
  if (sum(degrees) %% 2 != 0) degrees[which.max(degrees)] <- degrees[which.max(degrees)] + 1
  graph <- sample_degseq(degrees, method = "simple.no.multiple")
  adj_list <- lapply(1:popsize, function(v) as.integer(neighbors(graph, v)))
  list(graph_net = graph, network_pop = rep("resident", popsize), adj_list_net = adj_list)
}
# Generates the homogeneous network population
make_homogeneous_network <- function(popsize, mean_degree) {
  graph <- sample_k_regular(n = popsize, k = mean_degree, directed = FALSE, multiple = FALSE)
  adj_list <- lapply(1:popsize, function(v) as.integer(neighbors(graph, v)))
  list(graph = graph, hom_pop = rep("resident", popsize), adj_list_hom = adj_list)
}

# SIMULATION FUNCTIONS -----------------------------------------------------

##### Well-mixed  
run_wellmixed_simulation <- function(output_dir, file_name, run_number, variant_code, popsize) {
 # Create base seed
  model_code <- 1
  base_seed <- experiment_number * 1e6 +
    sim_type         * 1e5 +
    interaction_code * 1e4 +
    model_code       * 1e3 +
    variant_code     * 1e2 +
    run_number
  
  # Create grid with every combination of the parameter sweeps.
  results_wm <- expand.grid(mig_rate = migration_rates, c_i = c_i_s)
  # Create empty columns where resident fractions and seed numbers will  be stored
  # of every run of every parameter combination.
  results_wm$resident_fraction <- NA_real_
  results_wm$seed_used <- NA_integer_
  
  tic(paste("Well-mixed Simulation for popsize =", popsize))
  # This loops over every parameter combination in the grid.
  for (i in seq_len(nrow(results_wm))) {
    set.seed(base_seed + i)

    # Notes down the currrent parameter values in the results dataframe.
    mig_rate <- results_wm$mig_rate[i]
    c_i <- results_wm$c_i[i]
    # Generate the well-mixed population
    pop <- make_wellmixed_pop(popsize, "resident")

    # The simulation starts here and continues for every parameter combination.
    for (t in seq_len(time_steps)) {
      # Determiens whether migration will take place based on the
      # probability mig_rate.
      is_migrating <- runif(1) < mig_rate
      # Focal individual is sampled from the population.
      focal <- sample(popsize, 1)

      # Migration event: focal individual is replaced by an immigrant.
      if (is_migrating) {
        pop$trait[focal] <- "immigrant"
      } else {
      # Interaction event: focal individual is paired up with an individual is sampled 
      # from the population. Sampling continues until the sampled individual is different
      # from the focal individual.
        repeat {
          partner <- sample(popsize, 1)
          if (partner != focal) break
        }
        # Interaction can only happen if the two differ in culture.
        # Determines if interaction will occur based on interaction tendency.
        if (pop$trait[focal] != pop$trait[partner] && runif(1) < int_prob_other) {
        # Determines if the focal individual will take over its partner's trait
        # based on cultural conservatism (change_prob).
          change_prob <- ifelse(pop$trait[focal] == "resident", 1 - c_r, 1 - c_i)
          if (runif(1) < change_prob) {
            pop$trait[focal] <- ifelse(pop$trait[focal] == "resident", "immigrant", "resident")
          }
        }
      }
    }
     # Results are saved in the results dataframe and saved as an .RDS file.
    results_wm$resident_fraction[i] <- sum(pop$trait == "resident") / popsize
    results_wm$seed_used[i] <- base_seed + i
  }
  toc()
  
  saveRDS(results_wm, file = file.path(output_dir, file_name))
}

##### Homogeneous Network
run_homogeneous_simulation <- function(output_dir, file_name, run_number, variant_code, popsize) {
  model_code <- 2
  base_seed <- experiment_number * 1e6 +
    sim_type         * 1e5 +
    interaction_code * 1e4 +
    model_code       * 1e3 +
    variant_code     * 1e2 +
    run_number

  # Create grid with every combination of the parameter sweeps.
  results_hom <- expand.grid(mig_rate = migration_rates, c_i = c_i_s)
  # Create empty columns where resident fractions and seed numbers will  be stored
  # of every run of every parameter combination.
  results_hom$resident_fraction <- NA_real_
  results_hom$seed_used <- NA_integer_
  
  tic(paste("Homogeneous Simulation for popsize =", popsize))
  # This loops over every parameter combination in the grid.
  for (i in seq_len(nrow(results_hom))) {
    set.seed(base_seed + i)

    # Notes down the currrent parameter values in the results dataframe.
    mig_rate <- results_hom$mig_rate[i]
    c_i <- results_hom$c_i[i]
    # Generate the homogeneous network
    data <- make_homogeneous_network(popsize, mean_degree)
    pop <- data$hom_pop
    adj <- data$adj_list_hom

    # The simulation starts here and continues for every parameter combination.
    for (t in seq_len(time_steps)) {
      # Determiens whether migration will take place based on the
      # probability mig_rate.
      is_migrating <- runif(1) < mig_rate
      # Focal individual is sampled from the population.
      focal <- sample.int(popsize, 1)

      # For every time step, one of two events happen.
      # Migration event: focal individual is replaced by an immigrant.
      if (is_migrating) {
        pop[focal] <- "immigrant"
      } else {
      # Interaction event: focal individual is paired up with an individual is sampled 
      # from the list of the focal individual's neighbours.
        neighbor <- adj[[focal]][sample.int(length(adj[[focal]]), 1)]
        # Interaction can only happen if the two differ in culture.
        # Determines if interaction will occur based on interaction tendency.
        if (pop[focal] != pop[neighbor] && runif(1) < int_prob_other) {
        # Determines if the focal individual will take over its partner's trait
        # based on cultural conservatism (change_prob).
          change_prob <- ifelse(pop[focal] == "resident", 1 - c_r, 1 - c_i)
          if (runif(1) < change_prob) {
            pop[focal] <- ifelse(pop[focal] == "resident", "immigrant", "resident")
          }
        }
      }
    }
    # Results are saved in the results dataframe and saved as an .RDS file.
    results_hom$resident_fraction[i] <- sum(pop == "resident") / popsize
    results_hom$seed_used[i] <- base_seed + i
  }
  toc()
  
  saveRDS(results_hom, file = file.path(output_dir, file_name))
}

##### Heterogeneous Network                     
run_network_simulation <- function(output_dir, file_name, run_number, variant_code, popsize) {
  # Create base seed
  model_code <- 3
  base_seed <- experiment_number * 1e6 +
    sim_type         * 1e5 +
    interaction_code * 1e4 +
    model_code       * 1e3 +
    variant_code     * 1e2 +
    run_number

  # Create grid with every combination of the parameter sweeps.
  results_net <- expand.grid(mig_rate = migration_rates, c_i = c_i_s)
  # Create empty columns where resident fractions and seed numbers will  be stored
  # of every run of every parameter combination.
  results_net$resident_fraction <- NA_real_
  results_net$seed_used <- NA_integer_
  
  tic(paste("Heterogeneous Simulation for popsize =", popsize))
  # This loops over every parameter combination in the grid.
  for (i in seq_len(nrow(results_net))) {
    set.seed(base_seed + i)

    # Notes down the current parameter values in the results dataframe.
    mig_rate <- results_net$mig_rate[i]
    c_i <- results_net$c_i[i]
    # Generate the heterogeneous network
    data <- make_neg_binom_network(popsize, mean_degree, var_degree)
    pop <- data$network_pop
    adj <- data$adj_list_net

    # The simulation starts here and continues for every parameter combination.
    for (t in seq_len(time_steps)) {
      # Determiens whether migration will take place based on the
      # probability mig_rate.
      is_migrating <- runif(1) < mig_rate
      # Focal individual is sampled from the population.
      focal <- sample.int(popsize, 1)

      # For every time step, one of two events happen.
      # Migration event: focal individual is replaced by an immigrant.
      if (is_migrating) {
        pop[focal] <- "immigrant"
      } else {
      # Interaction event: focal individual is paired up with an individual is sampled 
      # from the list of the focal individual's neighbours.
        neighbor <- adj[[focal]][sample.int(length(adj[[focal]]), 1)]
        # Interaction can only happen if the two differ in culture.
        # Determines if interaction will occur based on interaction tendency.
        if (pop[focal] != pop[neighbor] && runif(1) < int_prob_other) {
        # Determines if the focal individual will take over its partner's trait
        # based on cultural conservatism (change_prob). 
          change_prob <- ifelse(pop[focal] == "resident", 1 - c_r, 1 - c_i)
          if (runif(1) < change_prob) {
            pop[focal] <- ifelse(pop[focal] == "resident", "immigrant", "resident")
          }
        }
      }
    }
    # Results are saved in the results dataframe and saved as an .RDS file.
    results_net$resident_fraction[i] <- sum(pop == "resident") / popsize
    results_net$seed_used[i] <- base_seed + i
  }
  toc()
  
  saveRDS(results_net, file = file.path(output_dir, file_name))
  
}

# RUN ALL SIMULATIONS ------------------------------------------------------
# Output directory
output_dir <- "data_popsize"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}
# Loops over every population size setting. 
for (psize in pop_sizes) {
  variant_code <- variant_codes[[as.character(psize)]]
  # Run the simulation for every type of population, with 10 replicates per type.
  for (i in 1:10) {
    run_wellmixed_simulation(output_dir, paste0("data_wm_popsize", psize, "_run", i, ".RDS"), i, variant_code, psize)
    run_homogeneous_simulation(output_dir, paste0("data_hom_popsize", psize, "_run", i, ".RDS"), i, variant_code, psize)
    run_network_simulation(output_dir, paste0("data_net_popsize", psize, "_run", i, ".RDS"), i, variant_code, psize)
  }
}
