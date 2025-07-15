################################################################################
# Simulation: Social Net Acculturation Model - Homogeneous Network Figure
# Author: Flora Murakeozy-Kis
# Date: July 2025
#
# Description:
# The final resident trait frequencies after 1 million events in a 
# population with homogeneous network structure for a parameter combination 
# of 10x10 migration rates and immigrant cultural conservatism values is
# plotted in a heatmap. 
###################################################################
# SOURCES  
source("plotting.R")

###################################################################
# PLOTTING 

# INTERACTION PROBABILITY = 0.1 -----------------------------------
results <- readRDS("data/hom01.RDS")
plot01 <- plot_hom(results,0.1)
ggsave("figures/hom_01.pdf", plot = plot01, width = 8, height = 6)

# INTERACTION PROBABILITY = 0.5 -----------------------------------
results <- readRDS("data/hom05.RDS")
plot05 <- plot_hom(results,0.5)
ggsave("figures/hom_05.pdf", plot = plot05, width = 8, height = 6)

###################################################################
# READING DATA 

plot_homogenous_results01 <- function() {
  results <- readRDS("data/hom01.RDS")
  plot01 <- plot_hom(results,0.1)
  ggsave("figures/hom_01.pdf", plot = plot01, width = 8, height = 6)
}

plot_homogenous_results05 <- function() {
  results <- readRDS("data/hom01.RDS")
  plot05<- plot_hom(results,0.1)
  ggsave("figures/hom_05.pdf", plot = plot05, width = 8, height = 6)
}
