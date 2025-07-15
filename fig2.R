################################################################################
# Simulation: Social Net Acculturation Model - Heterogeneous Network Figure
# Author: Flora Murakeozy-Kis
# Date: July 2025
#
# Description:
# The final resident trait frequencies after 1 million events in a 
# population with negative binomial degree distribution for a 
# parameter combination of 10x10 migration rates and immigrant 
# cultural conservatism values is plotted in a heatmap. 
###################################################################
# SOURCES 

source("plotting.R")

###################################################################
# PLOTTING 

# INTERACTION PROBABILITY = 0.1 -----------------------------------
resultsn <- readRDS("data/network05.RDS")
plot05net <- plot_network(resultsn,0.5)
ggsave("figures/net_0.5.pdf", plot = plot05net, width = 8, height = 6)

# INTERACTION PROBABILITY = 0.5 -----------------------------------
resultsn <- readRDS("data/network01.RDS")
plot01net <- plot_network(resultsn,0.1)
ggsave("figures/net_0.1.pdf", plot = plot01net, width = 8, height = 6)

###################################################################
# READING DATA

plot_network_results01 <- function() {
  resultsn <- readRDS("data/network01.RDS")
  plot05net <- plot_network(resultsn,0.5)
  ggsave("figures/net_01.pdf", plot = plot05net, width = 8, height = 6)
}

plot_network_results05 <- function() {
  resultsn <- readRDS("data/network05.RDS")
  plot05net <- plot_network(resultsn,0.5)
  ggsave("figures/net_05.pdf", plot = plot05net, width = 8, height = 6)
}
