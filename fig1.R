################################################################################
# Simulation: Social Net Acculturation Model - Well-mixed Figure
# Author: Flora Murakeozy-Kis
# Date: July 2025
#
# Description:
# The final resident trait frequencies after 1 million events in a well-mixed 
# population for a parameter combination of 10x10 migration rates and immigrant 
# cultural conservatism values is plotted in a heatmap. 
###################################################################
# SOURCES 

source("plotting.R")

###################################################################
# PLOTTING 

# INTERACTION PROBABILITY = 0.1 -----------------------------------
results <- readRDS("data/wellmixed01.RDS")
plot01 <- plot_wellmixed(results,0.1)
ggsave("figures/wm_0.1t.pdf", plot = plot01, width = 8, height = 6)

# INTERACTION PROBABILITY = 0.5 -----------------------------------
results <- readRDS("data/wellmixed05.RDS")
plot05 <- plot_wellmixed(results,0.5)
ggsave("figures/wm_0.5t.pdf", plot = plot05, width = 8, height = 6)

###################################################################
# READING DATA 

plot_wellmixed_results01 <- function() {
  results <- readRDS("data/wellmixed01.RDS")
  plot01 <- plot_wellmixed(results, 0.1)
  ggsave("figures/wm_01.pdf", plot = plot01, width = 8, height = 6)
}

plot_wellmixed_results05 <- function() {
  results <- readRDS("data/wellmixed05.RDS")
  plot01 <- plot_wellmixed(results, 0.1)
  ggsave("figures/wm_05.pdf", plot = plot01, width = 8, height = 6)
}
