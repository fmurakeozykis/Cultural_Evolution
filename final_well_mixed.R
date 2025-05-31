source("all_parameters.R")
library(dplyr)
library(tidyr)
library(ggplot2)

#Create the well-mixed population with size m, consisting only of residents.
properties_2 <- data.frame(
  individual = m,
  trait = rep("resident", length(m))
)

#This function starts the simulation.
vary_migration <- function(migrate, num_events, properties_2, m) {
  #'migrate' is  vector containing range of migration rates from all_parameters.R
  #For each migration rate, num_events will give the number of iterations per migration rate.
  for (x in migrate) {
    cat("\n--- Starting migration rate:", x, "---\n")
    
    for (event in 1:num_events) {
      #sample a random individual i form the population
      i <- sample(m, 1)
      #Migration is modeled as a binomial process, using the given migration rate as the probability of migration.
      #if migration occurs, i recieves the immigrant cultural trait.
      #If not, a partner is sampled from the network (excluding the focal individual).
      migrate_now <- rbinom(1, 1, x)
      if (migrate_now == 1) {
        properties_2$trait[properties_2$individual == i] <- "immigrant"
      } else {
        partners <- m[m != i]
        s <- sample(partners, 1)
        
        trait_i <- properties_2$trait[properties_2$individual == i]
        trait_s <- properties_2$trait[properties_2$individual == s]

        #If both are residents, the binomial event of interaction is given by the probability of interaction is given by rwithr = x_rr^2.
        #If they interact, the probability of taking over of the partner's trait is given by c_r
        #This is repeated with every possible combination
        if (trait_i == "resident" && trait_s == "resident") {
          interact <- rbinom(1, 1, rwithr)
          if (interact == 1) {
            take_over <- rbinom(1, 1, 1 - c_r)
            if (take_over == 1) {
              properties_2$trait[properties_2$individual == i] <- trait_s
            }
          }
        } else if (trait_i == "resident" && trait_s == "immigrant") {
          interact <- rbinom(1, 1, rwithi)
          if (interact == 1) {
            take_over <- rbinom(1, 1, 1 - c_r)
            if (take_over == 1) {
              properties_2$trait[properties_2$individual == i] <- trait_s
            }
          }
        } else if (trait_i == "immigrant" && trait_s == "resident") {
          interact <- rbinom(1, 1, rwithi)
          if (interact == 1) {
            take_over <- rbinom(1, 1, 1 - c_i)
            if (take_over == 1) {
              properties_2$trait[properties_2$individual == i] <- trait_s
            }
          }
        } else if (trait_i == "immigrant" && trait_s == "immigrant") {
          interact <- rbinom(1, 1, iwithi)
          if (interact == 1) {
            take_over <- rbinom(1, 1, 1 - c_i)
            if (take_over == 1) {
              properties_2$trait[properties_2$individual == i] <- trait_s
            }
          }
        }
      }
    }
    #This adds new columns to the data frame properties_2 for every new migration rate.
    migr <- paste0("mig_", x) 
    properties_2[[migr]] <- properties_2$trait
    
    cat("Saved traits for migration rate:", x, "\n")
    properties_2$trait <- rep("resident", length(m))
    
  }
  
  return(properties_2)
}
#Run the simulation, check system time.
system.time({
properties_2 <- vary_migration(migrate, num_events, properties_2, m)})


#Visualize data.
trait_counts <- traits_long %>%
  group_by(migration_rate, trait) %>%
  summarise(count = n(), .groups = "drop")

total_counts <- trait_counts %>%
  group_by(migration_rate) %>%
  summarise(total = sum(count))

immigrant_freq <- trait_counts %>%
  filter(trait == "immigrant") %>%
  left_join(total_counts, by = "migration_rate") %>%
  mutate(frequency = count / total) %>%
  select(migration_rate, frequency)

plot <-ggplot(trait_counts, aes(x = factor(migration_rate), y = count, fill = trait)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Residents vs Immigrants per Migration Rate",
    x = "Migration Rate",
    y = "Number of Individuals",
    fill = "Trait"
  ) +
  theme_minimal()

