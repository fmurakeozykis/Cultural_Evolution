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

#########################################
####### AVERAGE #########################
#########################################

plot_overlay <- function(list_of_datasets, plot_title = NULL, plot_subtitle = NULL) {
  combined_data <- dplyr::bind_rows(
    lapply(list_of_datasets, readRDS)
  )
  
  if (is.null(plot_title)) {
    plot_title <- "Resident fraction overlay plot"
  }
  
  ggplot(combined_data, aes(x = mig_rate, y = c_i, z = resident_fraction)) +
    geom_tile(aes(fill = resident_fraction), alpha = 0.6) +
    
    geom_contour(
      aes(linetype = as.factor(after_stat(level))),
      breaks = c(1, 0.5, 0.25, 0.1),
      color = "black",
      size = 0.8
    ) +
    
    scale_linetype_manual(
      name = "Resident frequency (contour)",
      values = c("1" = "solid", "0.5" = "dashed", "0.25" = "dotdash", "0.1" = "dotted")
    ) +
    
    scale_fill_gradient2(
      low = "red", mid = "white", high = "blue", midpoint = 0.5,
      name = "Resident frequency"
    ) +
    
    labs(
      x = "Migration rate",
      y = "Immigrant conservatism (c_i)",
      title = plot_title,
      subtitle = plot_subtitle
    ) +
    
    theme_classic()
}

plot_overlay(
  list_of_datasets = c(
    "data/fin_hom05_run1.RDS", "data/fin_hom05_run2.RDS", "data/fin_hom05_run3.RDS",
    "data/fin_hom05_run4.RDS", "data/fin_hom05_run5.RDS", "data/fin_hom05_run6.RDS",
    "data/fin_hom05_run7.RDS", "data/fin_hom05_run8.RDS"
  ),
  plot_title = "Resident fraction for homogenous population\nInteraction probability = 0.5",
  plot_subtitle = "Average over 8 runs"
)


