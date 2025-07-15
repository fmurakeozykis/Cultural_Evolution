################################################################################
# Simulation: Social Net Acculturation Model - Main
# Author: Flora Murakeozy-Kis
# Date: July 2025
#
# Description:
# Simulations can be run for both interaction rates (0.1 and 0.5) for all 3
# population models (well-mixed, homogeneous and heterogeneous (here, network)). 
# Results will be saved in the 'data' folder. Heatmaps of the results can
# be run directly, and will be saved in 'figures'.
# Parameter adjustments can be made in `parameters.R`.
###################################################################
# SOURCES 

library(here)
source("parameters.R")
source("make_population.R")
source("simulate_wellmixed.R")
source("simulate_network.R")
source("simulate_homogeneous.R")
source("plotting")

###################################################################
# SIMULATIONS 

dir.create(file.path(".", "data"), showWarnings = FALSE)

# simulate well mixed data
run_wellmixed_simulation01("data/wellmixed01_2.RDS")
run_wellmixed_simulation05("data/wellmixed05.RDS")

# simulate network data 
run_network_simulation01("data/network01_2.RDS")
run_network_simulation05("data/network05.RDS")

# simulate homogeneous data
run_homogeneous_simulation01("data/hom01.RDS")
run_homogeneous_simulation05("data/hom05.RDS")

###################################################################
# PLOTS 

dir.create(file.path(".", "figures"), showWarnings = FALSE)

source("fig1.R")
source("fig2.R")
source("fig3.R")

#make well-mixed figure
plot_wellmixed_results01()
plot_wellmixed_results05()

#make network figure
plot_network_results01()
plot_network_results05()

#make homogenous figure
plot_homogenous_results01()
plot_homogenous_results05()
