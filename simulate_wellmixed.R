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
   library(tictoc) # Allows for the measurement of simulation time

   # This creates a grid of every combination of the two variables.
   results_wm <- expand.grid(
     mig_rate = migration_rates,
     c_i = c_i_s
   )

   # This creates an empty column in the results_hom dataframe
   # where final resident trait frequencies will be stored.
   results_wm$resident_fraction <- NA_real_
   
   tic("Simulation Well-Mixed Int01")
   # This loops over every parameter combination in the grid.
   for (i in seq_len(nrow(results_wm))) {
   # Notes down the currrent parameter values in the results dataframe.
     mig_rate <- results_wm$mig_rate[i]
     c_i <- results_wm$c_i[i]

     # Generates the population.
     wellmixed_pop <- make_wellmixed_pop(popsize, "resident")

    # The simulation with the population starts here, 
    # and stops when all time steps have been looped over.
     for (t in seq_len(time_steps)) {
      # Determiens whether migration will take place based on the
      # probability mig_rate.
       is_migrating <- runif(1) < mig_rate
       # Focal individual is sampled from the population.
       focal_ind <- sample(popsize, 1)

       # Migration event: focal individual is replaced by an immigrant.
       if (is_migrating) {
         wellmixed_pop$trait[focal_ind] <- "immigrant"
      # Interaction event: focal individual is paired up with an individual sampled 
      # from the population.
       } else {
         repeat {
           int_part <- sample(popsize, 1)
           if (int_part != focal_ind) break # Ensures the interaction partner is
            # different from the focal individual.
         }
          # Interaction can only happen if the two differ in culture.
         if (wellmixed_pop$trait[focal_ind] != wellmixed_pop$trait[int_part]) {
             # Determines if interaction will occur based on interaction tendency.
           if (runif(1) < int_prob_other) {
             change_prob <- if (wellmixed_pop$trait[focal_ind] == "resident") (1 - c_r) else (1 - c_i)
              # Determines if the focal individual will take over its partner's trait
              # based on cultural conservatism (change_prob).
             if (runif(1) < change_prob) {
               wellmixed_pop$trait[focal_ind] <- ifelse(
                 wellmixed_pop$trait[focal_ind] == "resident", "immigrant", "resident"
               )
             }
           }
         }
       }
     }

      # Results are saved in the results dataframe and saved as an .RDS file.
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
   
   tic("Simulation Well-Mixed Int05")
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
