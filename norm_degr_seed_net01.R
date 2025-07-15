setwd("/home3/s5194326/floras_directory/degr.compare/results/seed/normal")

#############################################################################
############# PARAMETERS & PACKAGES #########################################
#############################################################################

popsize <- 1000
c_r <- 0.9
time_steps <- 10^6
int_prob_other <- 0.1

migration_rates <- seq(from = 0.001, to = 0.1, by = 0.01)
c_i_s <- seq(0, 0.9, 0.1)

mean_degree <- c(2, 8)
var_degree <- 10

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

experiment_number <- 3    # Experiment 3: Mean degree
sim_type <- 1             # Normal version
interaction_code <- 1     # 0.1 interaction probability
model_code <- 3           # Network model

# Variant codes for mean degrees
variant_codes <- list("2" = 1, "8" = 2, "14" = 3, "20" = 4)

#############################################################################
############# NETWORK CONSTRUCTION ##########################################
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

run_network_simulation01 <- function(output_path, degree, run_number, variant_code) {
  results_net <- expand.grid(mig_rate = migration_rates, c_i = c_i_s)
  results_net$resident_fraction <- NA_real_
  results_net$seed_used <- NA_integer_
  
  base_seed <- experiment_number * 1e6 +
    sim_type         * 1e5 +
    interaction_code * 1e4 +
    model_code       * 1e3 +
    variant_code     * 1e2 +
    run_number
  
  tic(paste("Network Simulation for degree =", degree))
  for (i in seq_len(nrow(results_net))) {
    set.seed(base_seed + i)
    
    mig_rate <- results_net$mig_rate[i]
    c_i <- results_net$c_i[i]
    
    pop_data <- make_neg_binom_network(popsize, degree, var_degree)
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

output_dir <- "s_deg_net01_normal_r"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

for (deg in mean_degree) {
  variant_code <- variant_codes[[as.character(deg)]]
  for (i in 1:10) {
    output_file <- file.path(output_dir, paste0("s_norm_net01_deg", deg, "_run", i, ".RDS"))
    run_network_simulation01(
      output_path = output_file,
      degree = deg,
      run_number = i,
      variant_code = variant_code
    )
  }
}
