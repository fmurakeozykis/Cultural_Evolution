source("plotting.R")

results <- readRDS("data/hom01.RDS")
plot01 <- plot_hom(results,0.1)
ggsave("figures/hom_01.pdf", plot = plot01, width = 8, height = 6)

results <- readRDS("data/hom05.RDS")
plot05 <- plot_hom(results,0.5)
ggsave("figures/hom_05.pdf", plot = plot05, width = 8, height = 6)

####################################################

plot_homogenous_results01 <- function() {
  results <- readRDS("data/hom01.RDS")
  plot01 <- plot_hom(results,0.1)
  ggsave("figures/hom_01.pdf", plot = plot01, width = 8, height = 6)
}

plot_homogenous_results05 <- function() {
  results <- readRDS("data/hom01.RDS")
  plot05<- plot_hom(results,0.1)
  ggsave("figures/hom_05.pdf", plot = plot05, width = 8, height = 6)
}
