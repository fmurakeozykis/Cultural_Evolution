source("C:/Users/fmura/Documents/groningen/New folder/all_parameters.R")
library(dplyr)
library(tidyr)
library(ggplot2)


# Create initial population
properties_2 <- data.frame(
  individual = m,
  trait = rep("resident", length(m))
)

vary_migration <- function(migrate, num_events, properties_2, m) {
  
  for (x in migrate) {
    cat("\n--- Starting migration rate:", x, "---\n")
    
    for (event in 1:num_events) {
      
      # Pick a random individual
      i <- sample(m, 1)
      
      migrate_now <- rbinom(1, 1, x)
      if (migrate_now == 1) {
        properties_2$trait[properties_2$individual == i] <- "immigrant"
        # cat(i, "replaced by immigrant\n")
      } else {
        partners <- m[m != i]
        s <- sample(partners, 1)
        
        trait_i <- properties_2$trait[properties_2$individual == i]
        trait_s <- properties_2$trait[properties_2$individual == s]
        
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
      
      # OPTIONAL: Save intermediate every N steps
      # if (event %% 100000 == 0) cat("Event:", event, "\n")
    }
    
    # After 1 million events for this migration rate, save the result
    migr <- paste0("mig_", x)  # Add a prefix to avoid issues
    properties_2[[migr]] <- properties_2$trait
    
    cat("Saved traits for migration rate:", x, "\n")
    properties_2$trait <- rep("resident", length(m))
    
  }
  
  return(properties_2)
}

system.time({
properties_2 <- vary_migration(migrate, num_events, properties_2, m)})


# Count number of individuals with each trait per migration rate
trait_counts <- traits_long %>%
  group_by(migration_rate, trait) %>%
  summarise(count = n(), .groups = "drop")

# Calculate total individuals per migration rate
total_counts <- trait_counts %>%
  group_by(migration_rate) %>%
  summarise(total = sum(count))

# Join and filter only immigrant rows
immigrant_freq <- trait_counts %>%
  filter(trait == "immigrant") %>%
  left_join(total_counts, by = "migration_rate") %>%
  mutate(frequency = count / total) %>%
  select(migration_rate, frequency)

# Plot
plot <-ggplot(trait_counts, aes(x = factor(migration_rate), y = count, fill = trait)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Residents vs Immigrants per Migration Rate",
    x = "Migration Rate",
    y = "Number of Individuals",
    fill = "Trait"
  ) +
  theme_minimal()

ggplot(immigrant_freq, aes(x = migration_rate, y = frequency, fill = value)) +
  geom_tile() +            # each tile is one cell of the heatmap
  scale_fill_gradient(low = "blue", high = "red") +  # colors from low to high values
  theme_minimal() +
  labs(title = "Heatmap", fill = "Value")


