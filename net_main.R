source("C:/Users/fmura/Documents/groningen/New folder/network.R")
source("C:/Users/fmura/Documents/groningen/New folder/all_parameters.R")
library(igraph)
library(tidyr)
library(ggplot2)



cols <- c("node", "trait")
properties_1 <- data.frame(
  node = (V(g)),
  trait = rep("resident", n)
)

properties_1 <- data.frame(
  node = as.integer(V(g)), 
  trait = rep("resident", n)
)

vary_migration <- function(migrate, num_events, properties_1, g) {
  for (x in migrate) {
    cat("\n--- Starting migration rate:", x, "---\n")
    
    for (event in 1:num_events) {
      
      i <- sample(as.integer(V(g)), 1)
      
      migrate_now <- rbinom(1, 1, x)
      if (migrate_now == 1) {
        properties_1$trait[properties_1$node == i] <- "immigrant"
      } else {
        partners <- as.integer(V(g))[as.integer(V(g)) != i]  
        s <- sample(partners, 1)
        
        trait_i <- properties_1$trait[properties_1$node == i]
        trait_s <- properties_1$trait[properties_1$node == s]
        
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
    
    migr <- paste0("mig_", x)
    properties_1[[migr]] <- properties_1$trait
    cat("Saved traits for migration rate:", x, "\n")
    
    properties_1$trait <- rep("resident", n)
  }
  
  return(properties_1)
}

system.time({
  properties_1 <- vary_migration(migrate, num_events, properties_1, g)
})
