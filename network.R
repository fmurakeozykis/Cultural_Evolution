if (!requireNamespace("igraph", T)) {install.packages("igraph")}
library(igraph)
source("C:/Users/fmura/Downloads/all_parameters (1).R")

size <- (mean_deg^2) / (var_deg - mean_deg)

seed_value <- sample(1:1000, 1)
set.seed(seed_value)
print(seed_value)

deg_seq <- rnbinom(n, size = size, mu = mean_deg)

if (sum(deg_seq) %% 2 != 0) {
  deg_seq[1] <- deg_seq[1] + 1
}
#sum of node degrees has to be even



deg_seq_1 <- deg_seq + rep(1,n)

g <- sample_degseq(deg_seq_1, method = "simple.no.multiple")

repeat {
  g <- sample_degseq(deg_seq_1, method = "simple.no.multiple")
  if (all(degree(g) > 0)) break
}

graph <- plot(g, vertex.size = deg_seq, vertex.label = NA)

hist <- hist(degree(g), breaks = 20, main = "Degree Distribution (Negative Binomial)",
             xlim = range(0,20), ylim = range(0,50),
     xlab = "Degree", ylab = "Frequency")

edges <- as.data.frame(as_edgelist(g))



