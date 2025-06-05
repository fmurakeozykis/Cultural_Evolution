###for network###

resultsn <- readRDS("data/network05.RDS")
plot05net <- plot_network(resultsn,0.5)

ggsave("figures/net_0.5.pdf", plot = plot05net, width = 8, height = 6)



resultsn <- readRDS("data/network01.RDS")
plot01net <- plot_network(resultsn,0.1)

ggsave("figures/net_0.1.pdf", plot = plot01net, width = 8, height = 6)

######################################################################

plot_network_results01 <- function() {
  resultsn <- readRDS("data/network01.RDS")
  plot05net <- plot_network(resultsn,0.5)
  ggsave("figures/net_01.pdf", plot = plot05net, width = 8, height = 6)
}

plot_network_results05 <- function() {
  resultsn <- readRDS("data/network05.RDS")
  plot05net <- plot_network(resultsn,0.5)
  ggsave("figures/net_05.pdf", plot = plot05net, width = 8, height = 6)
}
