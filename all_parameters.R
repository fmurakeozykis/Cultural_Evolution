
##### NETWORK #####
n <- 100  # number of nodes
mean_deg <- 3
var_deg <- 10

##### WELL-MIXED #####
m <- seq(1, n)
##### MAIN #####
trait <- c("immigrant", "resident")
num_events <- 10^6

#cultural conservatism

###parameters ci and cr 
#(ranging between 0.001 and 1.0 with step size 0.05), 
#and Xir (for values 0.1 and 0.5)
c_r <- 0.1
c_i <- c(0.1, 0.5)
d_c <- c_r - c_i

#interaction tendency
## individual tendency
#x_ir <- 0.5
#x_ri <- 0.5
## actual probability
#rwithi <- x_ir * x_ri
rwithi <- 0.1

#### COMBINATIONS ####
migrate <- c(0.0001, 0.005)


