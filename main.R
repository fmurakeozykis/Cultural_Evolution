library(here)
#needed for dir.create

# simulation parameters
source("parameters.R")

# if needed: plotting parameters

# the functions we will use in our simulations -> here once can simply choose which
##to run, without having to select lines of code every time
source("make_population.R")
source("simulate_wellmixed.R")
source("simulate_network.R")
source("simulate_homogeneous.R")
source("plotting")

###### simulate data ###########################################################

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

###### plot data ###############################################################

dir.create(file.path(".", "figures"), showWarnings = FALSE)

# file.path = combines current directory and data into a 
## platform-independent path e,g, ./data
#. = current directory
# data = data
# dir.create = creates d
#showwarnings = false -> not created again if it already exists

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


###################################################################
############ LINES AVERAGE PLOTS ##################################
###################################################################

######### HOMOGENOUS
# Interaction probability = 0.5

plot_overlay(
  list_of_datasets = c(
    "data/fin_hom05_run1.RDS", "data/fin_hom05_run2.RDS", "data/fin_hom05_run3.RDS",
    "data/fin_hom05_run4.RDS", "data/fin_hom05_run5.RDS", "data/fin_hom05_run6.RDS",
    "data/fin_hom05_run7.RDS", "data/fin_hom05_run8.RDS"
  ),
  plot_title = "Resident fraction for homogenous population\nInteraction probability = 0.5",
  plot_subtitle = "Average over 8 runs"
)

# Interaction probability = 0.1

plot_overlay(
  list_of_datasets = c(
    "data/fin_hom01_run1.RDS", "data/fin_hom01_run2.RDS", "data/fin_hom01_run3.RDS",
    "data/fin_hom01_run4.RDS", "data/fin_hom01_run5.RDS", "data/fin_hom01_run6.RDS",
    "data/fin_hom01_run7.RDS", "data/fin_hom01_run8.RDS"
  ),
  plot_title = "Resident fraction for homogenous population\nInteraction probability = 0.1",
  plot_subtitle = "Average over 8 runs"
)

######### WELL- MIXED
# Interaction probability = 0.5

plot_overlay(
  list_of_datasets = c(
    "data/fin_wellmixed05_run1.RDS", "data/fin_wellmixed05_run2.RDS", "data/fin_wellmixed05_run3.RDS",
    "data/fin_wellmixed05_run4.RDS", "data/fin_wellmixed05_run5.RDS", "data/fin_wellmixed05_run6.RDS",
    "data/fin_wellmixed05_run7.RDS", "data/fin_wellmixed05_run8.RDS", "data/fin_wellmixed05_run9.RDS",
    "data/fin_wellmixed05_run10.RDS"
  ),
  plot_title = "Resident fraction for well-mixed population\nInteraction probability = 0.5",
  plot_subtitle = "Average over 10 runs"
)

# Interaction probability = 0.1

plot_overlay(
  list_of_datasets = c(
    "data/fin_wellmixed01_run1.RDS", "data/fin_wellmixed01_run2.RDS", "data/fin_wellmixed01_run3.RDS",
    "data/fin_wellmixed01_run4.RDS", "data/fin_wellmixed01_run5.RDS", "data/fin_wellmixed01_run6.RDS",
    "data/fin_wellmixed01_run7.RDS", "data/fin_wellmixed01_run8.RDS", "data/fin_wellmixed01_run9.RDS",
    "data/fin_wellmixed01_run10.RDS"
  ),
  plot_title = "Resident fraction for well-mixed population\nInteraction probability = 0.1",
  plot_subtitle = "Average over 10 runs"
)

######### NETWORK
# Interaction probability = 0.5

plot_overlay(
  list_of_datasets = c(
    "data/fin_network05_run1.RDS", "data/fin_network05_run2.RDS", "data/fin_network05_run3.RDS",
    "data/fin_network05_run4.RDS", "data/fin_network05_run5.RDS", "data/fin_network05_run6.RDS",
    "data/fin_network05_run7.RDS", "data/fin_network05_run8.RDS", "data/fin_network05_run9.RDS",
    "data/fin_network05_run10.RDS"
  ),
  plot_title = "Resident fraction for network population\nInteraction probability = 0.5",
  plot_subtitle = "Average over 10 runs"
)

# Interaction probability = 0.1

plot_overlay(
  list_of_datasets = c(
    "data/fin_network01_run1.RDS", "data/fin_network01_run2.RDS", "data/fin_network01_run3.RDS",
    "data/fin_network01_run4.RDS", "data/fin_network01_run5.RDS", "data/fin_network01_run6.RDS",
    "data/fin_network01_run7.RDS", "data/fin_network01_run8.RDS", "data/fin_network01_run9.RDS",
    "data/fin_network01_run10.RDS"
  ),
  plot_title = "Resident fraction for network population\nInteraction probability = 0.1",
  plot_subtitle = "Average over 10 runs"
)


