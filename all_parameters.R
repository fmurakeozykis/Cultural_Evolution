
##### NETWORK #####
n <- 100  # number of nodes
mean_deg <- 6
var_deg <- 10

##### WELL-MIXED #####
samplesize <- 10
m <- seq(1,100)
##### MAIN #####
length <- 10 # change length to sample size
trait <- c("immigrant", "resident")
##simulte well-mixed model compare

#cultural conservatism
c_r <- 0.4
c_i <- 0.2
d_c <- c_r - c_i
#interaction tendency
rwithr <- 0.6
rwithi <- 0.6
iwithi <- 0.6
iwithr <- 0.6

#### COMBINATIONS ####
#par <- c()
migrate <- c(0.2, 0.4, 0.6, 0.8)
#combi <- ed
