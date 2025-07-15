################################################################################
# Simulation: Social Net Acculturation Model - Simulation of Well-Mixed Population
# Author: Flora Murakeozy-Kis
# Date: July 2025
#
# Description:
# Simulation of acculturation mechanisms in the well-mixed population produced 
# in `make_population.R`, for both in the case of interaction probability being 
# being equal to 0.1 and 0.5.
###########################################################################

# INTERACTION PROBABILITY = 0.1 ------------------------------------------

run_wellmixed_simulation01 <- function(output_path = "data/wellmixed01.RDS") {
   source("parameters.R")
   source("make_population.R")
   library(tictoc)
   
   results_wm <- expand.grid(
     mig_rate = migration_rates,
     c_i = c_i_s
   )
   
   results_wm$resident_fraction <- NA_real_
   
   tic("Simulation")
   for (i in seq_len(nrow(results_wm))) {
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
         if (wellmixed_pop$trait[focal_ind] != wellmixed_pop$trait[int_part]) {
           if (runif(1) < int_prob_other) {
             change_prob <- if (wellmixed_pop$trait[focal_ind] == "resident") (1 - c_r) else (1 - c_i)
             if (runif(1) < change_prob) {
               wellmixed_pop$trait[focal_ind] <- ifelse(
                 wellmixed_pop$trait[focal_ind] == "resident", "immigrant", "resident"
               )
             }
           }
         }
       }
     }
     
     results_wm$resident_fraction[i] <- sum(wellmixed_pop$trait == "resident") / popsize
   }
   toc()
   
   saveRDS(results_wm, file = output_path)
 }
 
# INTERACTION PROBABILITY = 0.5 ------------------------------------------
 
 run_wellmixed_simulation05 <- function(output_path = "data/wellmixed05.RDS") {
   source("parameters.R")
   source("make_population.R")
   library(tictoc)
   
   results_wm <- expand.grid(
     mig_rate = migration_rates,
     c_i = c_i_s
   )
   
   results_wm$resident_fraction <- NA_real_
   
   tic("Simulation")
   for (i in seq_len(nrow(results_wm))) {
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
         if (wellmixed_pop$trait[focal_ind] != wellmixed_pop$trait[int_part]) {
           if (runif(1) < int_prob_other) {
             change_prob <- if (wellmixed_pop$trait[focal_ind] == "resident") (1 - c_r) else (1 - c_i)
             if (runif(1) < change_prob) {
               wellmixed_pop$trait[focal_ind] <- ifelse(
                 wellmixed_pop$trait[focal_ind] == "resident", "immigrant", "resident"
               )
             }
           }
         }
       }
     }
     
     results_wm$resident_fraction[i] <- sum(wellmixed_pop$trait == "resident") / popsize
   }
   toc()
   
   saveRDS(results_wm, file = output_path)
 }
