library(igraph)
source("C:/Users/fmura/Documents/groningen/New folder/all_parameters.R")

# Parameters
#n <- 100  # number of nodes
#mean_deg <- 6
#var_deg <- 10

size <- (mean_deg^2) / (var_deg - mean_deg)

set.seed(123)
deg_seq <- rnbinom(n, size = size, mu = mean_deg)

if (sum(deg_seq) %% 2 != 0) {
  deg_seq[1] <- deg_seq[1] + 1
}
#sum of node degrees has to be even



deg_seq_1 <- deg_seq + rep(1,n)

g <- sample_degseq(deg_seq_1, method = "simple.no.multiple")


graph <- plot(g, vertex.size = deg_seq, vertex.label = NA)

hist <- hist(degree(g), breaks = 20, main = "Degree Distribution (Negative Binomial)",
     xlab = "Degree", ylab = "Frequency")

