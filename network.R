library(igraph)
source("all_parameters.R")

#The size of the negative binomial distribution is a ratio between the mean and variance.
#Thus, degree distirbution of the network can be adjusted by varying the mean and variance.
size <- (mean_deg^2) / (var_deg - mean_deg)

#Using the negative binomial distirbution, we create a sequence of degrees
set.seed(123)
deg_seq <- rnbinom(n, size = size, mu = mean_deg)
#The sum of degrees must be an even number mathematically. 
#In case it is not, we add one degree to the last node in the sequence
if (sum(deg_seq) %% 2 != 0) {
  deg_seq[1] <- deg_seq[1] + 1
}

#To avoid the problem of a few individuals having 0 connections, we add one extra so every individual has at least 1.
deg_seq_1 <- deg_seq + rep(1,n)
#This creates a network g from the given degree sequence.
g <- sample_degseq(deg_seq_1, method = "simple.no.multiple")
#This plots the network g.
graph <- plot(g, vertex.size = deg_seq, vertex.label = NA)
#This plots a histogram of the degree distirbution in graph g.
hist <- hist(degree(g), breaks = 20, main = "Degree Distribution (Negative Binomial)",
     xlab = "Degree", ylab = "Frequency")

