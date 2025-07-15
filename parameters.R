################################################################################
# Simulation: Social Net Acculturation Model - Parameter Settings
# Author: Flora Murakeozy-Kis
# Date: July 2025
#
# Description:
# Parameter settings that will be used to simulated results in Main can be adjusted
# here.
###############################################################################
# GENERAL 

popsize <- 1000  # number of individuals
c_r <- 0.9 # resident cultural conservatism
time_steps <- 10^6 # simulation time steps
int_prob_other <- 0.1 # interaction probability between the resident and immigrant

###############################################################################
# SWEEPING RANGES

migration_rates <- seq(from = 0.001, to = 0.1, by = 0.01) # migration rates
c_i_s <- seq(0,0.9,0.1) # cultural conservatism of the immigrant

###############################################################################
# NETWORK-SPECIFIC 
mean_degree <- 4
var_degree <- 10
