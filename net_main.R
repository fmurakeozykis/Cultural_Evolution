source("C:/Users/fmura/Documents/groningen/New folder/network.R")
source("C:/Users/fmura/Documents/groningen/New folder/all_parameters.R")
library(igraph)
library(tidyr)
library(ggplot2)

#We create a dataframe with the nodes of graph g from network.R, where every individual is a resident.
cols <- c("node", "trait")
properties_1 <- data.frame(
  node = as.integer(V(g)), 
  trait = rep("resident", n)
)

#This function starts the simulation.
vary_migration <- function(migrate, num_events, properties_1, g) {
  #'migrate' is  vector containing range of migration rates from all_parameters.R
  #For each migration rate, num_events will give the number of iterations per migration rate.
  for (x in migrate) {
    cat("\n--- Starting migration rate:", x, "---\n")
    
    for (event in 1:num_events) {
    #Sample a random individual i form the network g.
      i <- sample(as.integer(V(g)), 1)
      #Migration is modeled as a binomial process, using the given migration rate as the probability of migration.
      #if migration occurs, i recieves the immigrant cultural trait.
      #If not, a partner is sampled from the network (excluding the focal individual).
      
      migrate_now <- rbinom(1, 1, x)
      if (migrate_now == 1) {
        properties_1$trait[properties_1$node == i] <- "immigrant"
      } else {
        partners <- as.integer(V(g))[as.integer(V(g)) != i]  
        s <- sample(partners, 1)
        
        trait_i <- properties_1$trait[properties_1$node == i]
        trait_s <- properties_1$trait[properties_1$node == s]
        
        #If both are residents, the binomial event of interaction is given by the probability of interaction is given by rwithr = x_rr^2.
        #If they interact, the probability of taking over of the partner's trait is given by c_r
        #This is repeated with every possible combination
        if (trait_i == "resident" && trait_s == "resident") {
          interact <- rbinom(1, 1, rwithr)
          if (interact == 1) {
            take_over <- rbinom(1, 1, 1 - c_r)
            if (take_over == 1) {
              properties_1$trait[properties_1$node == i] <- trait_s
            }
          }
        } else if (trait_i == "resident" && trait_s == "immigrant") {
          interact <- rbinom(1, 1, rwithi)
          if (interact == 1) {
            take_over <- rbinom(1, 1, 1 - c_r)
            if (take_over == 1) {
              properties_1$trait[properties_1$node == i] <- trait_s
            }
          }
        } else if (trait_i == "immigrant" && trait_s == "resident") {
          interact <- rbinom(1, 1, rwithi)
          if (interact == 1) {
            take_over <- rbinom(1, 1, 1 - c_i)
            if (take_over == 1) {
              properties_1$trait[properties_1$node == i] <- trait_s
            }
          }
        } else if (trait_i == "immigrant" && trait_s == "immigrant") {
          interact <- rbinom(1, 1, iwithi)
          if (interact == 1) {
            take_over <- rbinom(1, 1, 1 - c_i)
            if (take_over == 1) {
              properties_1$trait[properties_1$node == i] <- trait_s
            }
          }
        }
      }
    }
    #This adds new columns to the data frame properties_2 for every new migration rate.
    migr <- paste0("mig_", x)
    properties_1[[migr]] <- properties_1$trait
    cat("Saved traits for migration rate:", x, "\n")
    
    properties_1$trait <- rep("resident", n)
  }
  
  return(properties_1)
}
#Run the simulation, check system time.
system.time({
  properties_1 <- vary_migration(migrate, num_events, properties_1, g)
})
