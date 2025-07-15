################################################################################
# Simulation: Social Net Acculturation Model - Plot Functions
# Author: Flora Murakeozy-Kis
# Date: July 2025
#
# Description:
# Plot functions for heatmaps of resident cultural trait retention 
# 10x10 immigrant cultural conservatism values and migration rates. 
###################################################################
# SOURCES 

library(ggplot2)

###################################################################
# PLOTTING

# WELL-MIXED POPULATION  -----------------------------------------

plot_wellmixed <- function(data, interaction_prob) {
  title <- paste0("Interaction Probability = ", interaction_prob)
  p_wellmixed <- ggplot(data, aes(x = mig_rate, y = c_i, fill = resident_fraction)) +
    geom_tile() +
    scale_fill_gradient2(
      low = "red", mid = "white", high = "blue",
      midpoint = 0.5,
      name = "Resident frequency"
    ) +
    labs(
      x = "Migration rate",
      y = "Immigrant conservatism (c_i)",
      title = title
    ) +
    theme_classic()
  return(p_wellmixed)
}

# HETEROGENEOUS NETWORK POPULATION  -----------------------------------------
plot_network <- function(data, interaction_prob) {
  title <- paste0("Interaction Probability = ", interaction_prob)
  p_network<- ggplot(data, aes(x = mig_rate, y = c_i, fill = resident_fraction)) +
    geom_tile() +
    scale_fill_gradient2(
      low = "red", mid = "white", high = "blue",
      midpoint = 0.5,
      name = "Resident frequency"
    ) +
    labs(
      x = "Migration rate",
      y = "Immigrant conservatism (c_i)",
      title = title
    ) +
    theme_classic()
  return(p_network)
}

# HOMOGENEOUS NETWORK POPULATION  -----------------------------------------
plot_hom <- function(data, interaction_prob) {
  title <- paste0("Interaction Probability = ", interaction_prob)
  p_hom<- ggplot(data, aes(x = mig_rate, y = c_i, fill = resident_fraction)) +
    geom_tile() +
    scale_fill_gradient2(
      low = "red", mid = "white", high = "blue",
      midpoint = 0.5,
      name = "Resident frequency"
    ) +
    labs(
      x = "Migration rate",
      y = "Immigrant conservatism (c_i)",
      title = title
    ) +
    theme_classic()
  return(p_hom)
}



