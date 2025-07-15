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

run_homogeneous_simulation01 <- function(output_path = "data/hom01.RDS") {
  source("parameters.R")
  source("make_population.R")
  library(tictoc)
  
  results_hom <- expand.grid(
    mig_rate = migration_rates,
    c_i = c_i_s
  )
  
  results_hom$resident_fraction <- NA_real_
  
  tic("Simulation")
  for (i in seq_len(nrow(results_hom))) {
    mig_rate <- results_hom$mig_rate[i]
    c_i <- results_hom$c_i[i]
    
    pop_data <- make_homogeneous_network(popsize, mean_degree)
    hom_pop <- pop_data$hom_pop
    adj_list_hom <- pop_data$adj_list_hom
    
    for (t in seq_len(time_steps)) {
      is_migrating <- runif(1) < mig_rate
      focal_ind <- sample.int(popsize, 1)
      
      if (is_migrating) {
        hom_pop[focal_ind] <- "immigrant"
      } else {
        neighbors <- adj_list_hom[[focal_ind]]
        int_part <- neighbors[sample.int(length(neighbors), 1)]
        
        if (hom_pop[focal_ind] != hom_pop[int_part]) {
          if (runif(1) < int_prob_other) {
            change_prob <- if (hom_pop[focal_ind] == "resident") (1 - c_r) else (1 - c_i)
            if (runif(1) < change_prob) {
              hom_pop[focal_ind] <- ifelse(
                hom_pop[focal_ind] == "resident", "immigrant", "resident"
              )
            }
          }
        }
      }
    }
    
    results_hom$resident_fraction[i] <- sum(hom_pop == "resident") / popsize
  }
  toc()
  
  saveRDS(results_hom, file = output_path)
}

# INTERACTION PROBABILITY = 0.5 ------------------------------------------
  
run_homogeneous_simulation05 <- function(output_path = "data/hom05.RDS") {
  source("parameters.R")
  source("make_population.R")
  library(tictoc)
  
  results_hom <- expand.grid(
    mig_rate = migration_rates,
    c_i = c_i_s
  )
  
  results_hom$resident_fraction <- NA_real_
  
  tic("Simulation")
  for (i in seq_len(nrow(results_hom))) {
    mig_rate <- results_hom$mig_rate[i]
    c_i <- results_hom$c_i[i]
    
    pop_data <- make_homogeneous_network(popsize, mean_degree)
    hom_pop <- pop_data$hom_pop
    adj_list_hom <- pop_data$adj_list_hom
    
    for (t in seq_len(time_steps)) {
      is_migrating <- runif(1) < mig_rate
      focal_ind <- sample.int(popsize, 1)
      
      if (is_migrating) {
        hom_pop[focal_ind] <- "immigrant"
      } else {
        neighbors <- adj_list_hom[[focal_ind]]
        int_part <- neighbors[sample.int(length(neighbors), 1)]
        
        if (hom_pop[focal_ind] != hom_pop[int_part]) {
          if (runif(1) < int_prob_other) {
            change_prob <- if (hom_pop[focal_ind] == "resident") (1 - c_r) else (1 - c_i)
            if (runif(1) < change_prob) {
              hom_pop[focal_ind] <- ifelse(
                hom_pop[focal_ind] == "resident", "immigrant", "resident"
              )
            }
          }
        }
      }
    }
    
    results_hom$resident_fraction[i] <- sum(hom_pop == "resident") / popsize
  }
  toc()
  
  saveRDS(results_hom, file = output_path)
}
