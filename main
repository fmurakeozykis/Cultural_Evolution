library(here)

# simulation parameters
source("parameters.R")

source("make_population.R")
source("simulate_wellmixed.R")
source("simulate_network.R")
source("simulate_homogeneous.R")
source("plotting")

###### simulate data ###########################################################

dir.create(file.path(".", "data"), showWarnings = FALSE)

# simulate well mixed data
run_wellmixed_simulation01("data/wellmixed01.RDS")
run_wellmixed_simulation05("data/wellmixed05.RDS")

# simulate network data 
run_network_simulation01("data/network01.RDS")
run_network_simulation05("data/network05.RDS")

# simulate homogeneous data
run_homogeneous_simulation01("data/hom01.RDS")
run_homogeneous_simulation05("data/hom05.RDS")

###### plot data ###############################################################

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
