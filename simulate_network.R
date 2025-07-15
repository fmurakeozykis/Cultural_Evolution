################################################################################
# Simulation: Social Net Acculturation Model - Simulation of Homogeneous Network
# Population
# Author: Flora Murakeozy-Kis
# Date: July 2025
#
# Description:
# Simulation of acculturation mechanisms in the homogeneous network produced 
# in `make_population.R`, for both in the case of interaction probability being 
# being equal to 0.1 and 0.5
###########################################################################

# INTERACTION PROBABILITY = 0.1 ------------------------------------------

run_network_simulation01 <- function(output_path = "data/network01.RDS") {
  source("parameters.R")
  source("make_population.R")
  library(tictoc)
  
  results_net <- expand.grid(
    mig_rate = migration_rates,
    c_i = c_i_s
  )
  
  results_net$resident_fraction <- NA_real_
  
  tic("Simulation")
  for (i in seq_len(nrow(results_net))) {
    mig_rate <- results_net$mig_rate[i]
    c_i <- results_net$c_i[i]
    
    pop_data <- make_neg_binom_network(popsize, mean_degree, var_degree)
    network_pop <- pop_data$network_pop
    adj_list_net <- pop_data$adj_list_net
    
    for (t in seq_len(time_steps)) {
      is_migrating <- runif(1) < mig_rate
      focal_ind <- sample.int(popsize, 1)
      
      if (is_migrating) {
        network_pop[focal_ind] <- "immigrant"
      } else {
        neighbors <- adj_list_net[[focal_ind]]
        int_part <- neighbors[sample.int(length(neighbors), 1)]
        
        if (network_pop[focal_ind] != network_pop[int_part]) {
          if (runif(1) < int_prob_other) {
            change_prob <- if (network_pop[focal_ind] == "resident") (1 - c_r) else (1 - c_i)
            if (runif(1) < change_prob) {
              network_pop[focal_ind] <- ifelse(
                network_pop[focal_ind] == "resident", "immigrant", "resident"
              )
            }
          }
        }
      }
    }
    
    results_net$resident_fraction[i] <- sum(network_pop == "resident") / popsize
  }
  toc()
  
  saveRDS(results_net, file = output_path)
}

# INTERACTION PROBABILITY = 0.5 ------------------------------------------

run_network_simulation05 <- function(output_path = "data/network05.RDS") {
  source("parameters.R")
  source("make_population.R")
  library(tictoc)
  
  results_net <- expand.grid(
    mig_rate = migration_rates,
    c_i = c_i_s
  )
  
  results_net$resident_fraction <- NA_real_
  
  tic("Simulation")
  for (i in seq_len(nrow(results_net))) {
    mig_rate <- results_net$mig_rate[i]
    c_i <- results_net$c_i[i]
    
    pop_data <- make_neg_binom_network(popsize, mean_degree, var_degree)
    network_pop <- pop_data$network_pop
    adj_list_net <- pop_data$adj_list_net
    
    for (t in seq_len(time_steps)) {
      is_migrating <- runif(1) < mig_rate
      focal_ind <- sample.int(popsize, 1)
      
      if (is_migrating) {
        network_pop[focal_ind] <- "immigrant"
      } else {
        neighbors <- adj_list_net[[focal_ind]]
        int_part <- neighbors[sample.int(length(neighbors), 1)]
        
        if (network_pop[focal_ind] != network_pop[int_part]) {
          if (runif(1) < int_prob_other) {
            change_prob <- if (network_pop[focal_ind] == "resident") (1 - c_r) else (1 - c_i)
            if (runif(1) < change_prob) {
              network_pop[focal_ind] <- ifelse(
                network_pop[focal_ind] == "resident", "immigrant", "resident"
              )
            }
          }
        }
      }
    }
    
    results_net$resident_fraction[i] <- sum(network_pop == "resident") / popsize
  }
  toc()
  
  saveRDS(results_net, file = output_path)
}
