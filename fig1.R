source("plotting.R")


results <- readRDS("data/wellmixed01.RDS")
plot01 <- plot_wellmixed(results,0.1)
ggsave("figures/wm_0.1t.pdf", plot = plot01, width = 8, height = 6)



results <- readRDS("data/wellmixed05.RDS")
plot05 <- plot_wellmixed(results,0.5)
ggsave("figures/wm_0.5t.pdf", plot = plot05, width = 8, height = 6)



#reads the data created by the simulations
###################################################################

plot_wellmixed_results01 <- function() {
  results <- readRDS("data/wellmixed01.RDS")
  plot01 <- plot_wellmixed(results, 0.1)
  ggsave("figures/wm_01.pdf", plot = plot01, width = 8, height = 6)
}

plot_wellmixed_results05 <- function() {
  results <- readRDS("data/wellmixed05.RDS")
  plot01 <- plot_wellmixed(results, 0.1)
  ggsave("figures/wm_05.pdf", plot = plot01, width = 8, height = 6)
}
