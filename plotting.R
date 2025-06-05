library(ggplot2)

################################################################################
###### Functions for plotting the results ######################################
################################################################################

#plots are also now in functions, so that it can just take the input from parameters 
## and you dont have to reweite everything yourself
# i have to add the plots here, all of them
# they will also go to a spearatte folder?

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

##############################
###NETWORK####################
#############################
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

##############################
###HOMOGENOUSK####################
#############################
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


