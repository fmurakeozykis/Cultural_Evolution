setwd("/home3/s5194326/floras_directory/degr.compare/results/seed/normal")

#############################################################################
############# PARAMETERS & PACKAGES #########################################
#############################################################################

popsize <- 1000
c_r <- 0.9
time_steps <- 10^6
int_prob_other <- 0.5

migration_rates <- seq(from = 0.001, to = 0.1, by = 0.01)
c_i_s <- seq(0, 0.9, 0.1)

mean_degree <- c(2, 8, 14, 20)
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
interaction_code <- 2     # 0.5 interaction probability
model_code <- 2           # Homogeneous network

# Variant codes for mean degrees
variant_codes <- list("2" = 1, "8" = 2, "14" = 3, "20" = 4)

#############################################################################
############# POPULATION GENERATION #########################################
#############################################################################

make_homogeneous_network <- function(popsize, mean_degree) {
  graph <- sample_k_regular(n = popsize, k = mean_degree, directed = FALSE, multiple = FALSE)
  hom_pop <- rep("resident", popsize)
  adj_list <- lapply(1:popsize, function(v) as.integer(neighbors(graph, v)))
  list(graph = graph, hom_pop = hom_pop, adj_list_hom = adj_list)
}

#############################################################################
############# SIMULATION FUNCTION ###########################################
#############################################################################

run_homogeneous_simulation01 <- function(output_path, degree, run_number, variant_code) {
  results_hom <- expand.grid(mig_rate = migration_rates, c_i = c_i_s)
  results_hom$resident_fraction <- NA_real_
  results_hom$seed_used <- NA_integer_
  
  base_seed <- experiment_number * 1e6 +
    sim_type         * 1e5 +
    interaction_code * 1e4 +
    model_code       * 1e3 +
    variant_code     * 1e2 +
    run_number
  
  tic(paste("Homogeneous Simulation for degree =", degree))
  for (i in seq_len(nrow(results_hom))) {
    set.seed(base_seed + i)
    
    mig_rate <- results_hom$mig_rate[i]
    c_i <- results_hom$c_i[i]
    
    pop_data <- make_homogeneous_network(popsize, degree)
    hom_pop <- pop_data$hom_pop
    adj_list_hom <- pop_data$adj_list_hom
    
    for (t in seq_len(time_steps)) {
      is_migrating <- runif(1) < mig_rate
      focal_ind <- sample.int(popsize, 1)
      
      if (is_migrating) {
        hom_pop[focal_ind] <- "immigrant"
      } else {
        neighbors <- adj_list_hom[[focal_ind]]
        int_part <- neighbors[sample.int(length(neighbors), 1)]
        
        if (hom_pop[focal_ind] != hom_pop[int_part] && runif(1) < int_prob_other) {
          change_prob <- if (hom_pop[focal_ind] == "resident") (1 - c_r) else (1 - c_i)
          if (runif(1) < change_prob) {
            hom_pop[focal_ind] <- ifelse(hom_pop[focal_ind] == "resident", "immigrant", "resident")
          }
        }
      }
    }
    
    results_hom$resident_fraction[i] <- sum(hom_pop == "resident") / popsize
    results_hom$seed_used[i] <- base_seed + i
  }
  toc()
  
  saveRDS(results_hom, file = output_path)
}

#############################################################################
############# RUN SIMULATIONS ###############################################
#############################################################################

output_dir <- "s_deg_hom05_normal_r"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

for (deg in mean_degree) {
  variant_code <- variant_codes[[as.character(deg)]]
  for (i in 1:10) {
    output_file <- file.path(output_dir, paste0("s_norm_hom05_deg", deg, "_run", i, ".RDS"))
    run_homogeneous_simulation01(output_path = output_file, degree = deg, run_number = i, variant_code = variant_code)
  }
}
