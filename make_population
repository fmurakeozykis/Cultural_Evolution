library(igraph)

################################################################################
###### Functions for simulating different populations ##########################
################################################################################

###### Code for well-mixed population


make_wellmixed_pop <- function(popsize,trait) {
  population <- data.frame(
  individual = 1:popsize,
  trait = rep(trait, popsize))
  return(population)
}

# demo
#wellmixed_pop <- make_wellmixed_pop(100,"resident")

################################################################################

###### Code for the skewed population

# The "size" or dispersion/shape parameter of the negative binomial is 
# a ratio between mean and variance.
# It's given by size = mean_degree^2/(var_degree-mean_degree)

# Hence we can call rnbinom(popsize, size = size, mu=mean_degree)

make_neg_binom_network <- function(popsize, mean_degree, var_degree) {
  # First we calculate the size parameter
  size <- (mean_degree)^2/(var_degree-mean_degree)
  
  # We draw a random negative binomial distribution with mean degree one lower than
  # our target mean degree
  degrees <-  rnbinom(popsize, size = size, mu = mean_degree)
  
  # We fix only the zeroes instead of adding one to every node
  degrees[degrees == 0] <- 1  # just fix the few zeros
  
  # The sum of degrees must be an even number mathematically. 
  # In case it is not, we add one degree to a node in the sequence
  # We pick the one that already has the most connections
  if (sum(degrees) %% 2 != 0) {
    max_index <- which.max(degrees)
    degrees[max_index] <- degrees[max_index] + 1
  }
  
  graph <- sample_degseq(degrees, method = "simple.no.multiple")
  network_pop <- traits <- rep("resident", popsize)
  adj_list <- lapply(1:popsize, function(v) as.integer(neighbors(graph, v)))
  
  return(list(
    graph_net = graph,
    network_pop = network_pop,
    adj_list_net = adj_list
  ))
  }



# demo
# network_population <- make_neg_binom_network(1000,4,10)
#  mean(degree(network_population$graph))
#  var(degree(network_population$graph))
# plot_graph <- plot(network_population$graph, vertex.size = degree(network_population$graph),
#                    vertex.label = NA, layout = layout_with_fr)
# hist <- hist(degree(network_population$graph), breaks = 20, main = "Degree Distribution (Negative Binomial)",
#              xlab = "Degree", ylab = "Frequency", xlim = range(0,20), ylim = range(0,500))

################################################################################

###### Code for the homogeneous population

make_homogeneous_network <- function(popsize, mean_degree) {
  graph <- sample_k_regular(no.of.nodes = popsize, k = mean_degree, directed = FALSE, multiple = FALSE)
  hom_pop <- traits <- rep("resident", popsize)
  adj_list <- lapply(1:popsize, function(v) as.integer(neighbors(graph, v)))
  return(list(
    graph = graph,
    hom_pop = hom_pop,
    adj_list_hom = adj_list
  ))
}

# demo
# network_population <- make_homogeneous_network(1000,4)
# mean(degree(network_population))
# var(degree(network_population))
# plot_graph <- plot(network_population, vertex.size = degree(network_population),
#                    vertex.label = NA, layout = layout_with_fr)

