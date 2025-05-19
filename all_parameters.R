
##### NETWORK #####
n <- 100  # number of nodes
mean_deg <- 6
var_deg <- 10

##### WELL-MIXED #####
m <- seq(1, n)
##### MAIN #####
trait <- c("immigrant", "resident")
num_events <- 10^6

#cultural conservatism
c_r <- 1
c_i <- 0.5
d_c <- c_r - c_i

#interaction tendency
## individual tendency
x_ii <- 0.5
x_rr <- 0.5
x_ir <- 0.1
x_ri <- 0.1
## actual probability
rwithr <- x_rr * x_rr
iwithi <- x_ii * x_ii
rwithi <- x_ir * x_ri

#### COMBINATIONS ####
migrate <- seq(from = 0.001, to = 0.1, by = 0.005)


