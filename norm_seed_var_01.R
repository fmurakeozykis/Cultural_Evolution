setwd("/home3/s5194326/floras_directory/var.compare/results/seed/normal")

#############################################################################
############# PARAMETERS & PACKAGES #########################################
#############################################################################

# General simulation parameters
popsize <- 1000
c_r <- 0.9
time_steps <- 10^6
int_prob_other <- 0.1

# Sweep ranges
migration_rates <- seq(from = 0.001, to = 0.1, by = 0.01)
c_i_s <- seq(0, 0.9, 0.1)

# Network properties
mean_degree <- 4
var_degree <- c(10, 20, 30)

# Package handling
required_packages <- c("igraph", "here", "tictoc")
invisible(lapply(required_packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste("Package", pkg, "is not installed. Please install before running on Hábrók."))
  }
  library(pkg, character.only = TRUE)
}))

#############################################################################
############# SEED SYSTEM ###################################################
#############################################################################

experiment_number <- 2    # Experiment 2: Variance
sim_type <- 1             # Normal version
interaction_code <- 1     # 0.1 interaction probability
model_code <- 3           # Network

# Variant codes: 1 = variance 10, 2 = 20, 3 = 30
variant_codes <- list("10" = 1, "20" = 2, "30" = 3)

#############################################################################
############# POPULATION GENERATION #########################################
#############################################################################

make_neg_binom_network <- function(popsize, mean_degree, var_degree) {
  size <- (mean_degree)^2 / (var_degree - mean_degree)
  degrees <- rnbinom(popsize, size = size, mu = mean_degree)
  degrees[degrees == 0] <- 1
  if (sum(degrees) %% 2 != 0) degrees[which.max(degrees)] <- degrees[which.max(degrees)] + 1
  graph <- sample_degseq(degrees, method = "simple.no.multiple")
  adj_list <- lapply(1:popsize, function(v) as.integer(neighbors(graph, v)))
  list(graph_net = graph, network_pop = rep("resident", popsize), adj_list_net = adj_list)
}

#############################################################################
############# SIMULATION FUNCTION ###########################################
#############################################################################

run_network_simulation01 <- function(output_path, variance, run_number, variant_code) {
  results_net <- expand.grid(mig_rate = migration_rates, c_i = c_i_s)
  results_net$resident_fraction <- NA_real_
  results_net$seed_used <- NA_integer_
  
  base_seed <- experiment_number * 1e6 +
    sim_type         * 1e5 +
    interaction_code * 1e4 +
    model_code       * 1e3 +
    variant_code     * 1e2 +
    run_number
  
  tic("Network Simulation")
  for (i in seq_len(nrow(results_net))) {
    set.seed(base_seed + i)
    
    mig_rate <- results_net$mig_rate[i]
    c_i <- results_net$c_i[i]
    
    pop_data <- make_neg_binom_network(popsize, mean_degree, variance)
    network_pop <- pop_data$network_pop
    adj_list_net <- pop_data$adj_list_net
    
    for (t in seq_len(time_steps)) {
      is_migrating <- runif(1) < mig_rate
      focal_ind <- sample.int(popsize, 1)
      
      if (is_migrating) {
        network_pop[focal_ind] <- "immigrant"
      } else {
        neighbors <- adj_list_net[[focal_ind]]
        int_part <- neighbors[sample.int(length(neighbors), 1)]
        
        if (network_pop[focal_ind] != network_pop[int_part]) {
          if (runif(1) < int_prob_other) {
            change_prob <- if (network_pop[focal_ind] == "resident") (1 - c_r) else (1 - c_i)
            if (runif(1) < change_prob) {
              network_pop[focal_ind] <- ifelse(
                network_pop[focal_ind] == "resident", "immigrant", "resident"
              )
            }
          }
        }
      }
    }
    
    results_net$resident_fraction[i] <- sum(network_pop == "resident") / popsize
    results_net$seed_used[i] <- base_seed + i
  }
  toc()
  
  saveRDS(results_net, file = output_path)
}

#############################################################################
############# RUN SIMULATIONS ###############################################
#############################################################################

output_dir <- "norm_seed_var_net01_r"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

for (var in var_degree) {
  variant_code <- variant_codes[[as.character(var)]]
  for (i in 1:10) {
    output_file <- file.path(output_dir, paste0("s_norm_net01_var", var, "_run", i, ".RDS"))
    run_network_simulation01(output_path = output_file, variance = var, run_number = i, variant_code = variant_code)
  }
}
