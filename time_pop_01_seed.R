# Cultural Evolution Simulations - Final Version
setwd("/home3/s5194326/floras_directory/pop.size/results/time")

# PARAMETERS & PACKAGES ---------------------------------------------------

c_r <- 0.9
time_steps <- 10^6
int_prob_other <- 0.1

migration_rates <- seq(from = 0.001, to = 0.1, by = 0.01)
c_i_s <- seq(0, 0.9, 0.1)

mean_degree <- 4
var_degree <- 10

required_packages <- c("igraph", "tictoc", "here")
new_packages <- required_packages[!(required_packages %in% installed.packages()[, "Package"])]
if (length(new_packages)) install.packages(new_packages)
lapply(required_packages, library, character.only = TRUE)

# SEED SYSTEM -------------------------------------------------------------
experiment_number <- 1    # Experiment 1: Pop size
sim_type <- 2             # Time version
interaction_code <- 1     # 0.1 interaction prob

# Variant codes: 1 = popsize 500, 2 = 1000, 3 = 1500
variant_codes <- list("500" = 1, "1000" = 2, "1500" = 3)

# POPULATION FUNCTIONS -----------------------------------------------------

make_wellmixed_pop <- function(popsize, trait) {
  data.frame(individual = 1:popsize, trait = rep(trait, popsize))
}

make_neg_binom_network <- function(popsize, mean_degree, var_degree) {
  size <- (mean_degree)^2 / (var_degree - mean_degree)
  degrees <- rnbinom(popsize, size = size, mu = mean_degree)
  degrees[degrees == 0] <- 1
  if (sum(degrees) %% 2 != 0) degrees[which.max(degrees)] <- degrees[which.max(degrees)] + 1
  graph <- sample_degseq(degrees, method = "simple.no.multiple")
  adj_list <- lapply(1:popsize, function(v) as.integer(neighbors(graph, v)))
  list(graph_net = graph, network_pop = rep("resident", popsize), adj_list_net = adj_list)
}

make_homogeneous_network <- function(popsize, mean_degree) {
  graph <- sample_k_regular(n = popsize, k = mean_degree, directed = FALSE, multiple = FALSE)
  adj_list <- lapply(1:popsize, function(v) as.integer(neighbors(graph, v)))
  list(graph = graph, hom_pop = rep("resident", popsize), adj_list_hom = adj_list)
}

# SIMULATION FUNCTIONS -----------------------------------------------------


record_interval <- 1000
num_records <- ceiling(time_steps / record_interval)

save_simulation <- function(output_dir, file_name, results, time_series) {
  saveRDS(list(summary = results, time_series = time_series), file = file.path(output_dir, file_name))
}

run_wellmixed_simulation <- function(output_dir, file_name, run_number, variant_code, popsize) {
    
  model_code <- 1
  base_seed <- experiment_number * 1e6 +
    sim_type         * 1e5 +
    interaction_code * 1e4 +
    model_code       * 1e3 +
    variant_code     * 1e2 +
    run_number
  
  results <- expand.grid(mig_rate = migration_rates, c_i = c_i_s)
  results$seed_used <- NA_integer_
  time_series_list <- vector("list", nrow(results))
  
  tic("Well-mixed Simulation")
  for (i in seq_len(nrow(results))) {
    set.seed(base_seed + i)
    mig_rate <- results$mig_rate[i]
    c_i <- results$c_i[i]
    pop <- make_wellmixed_pop(popsize, "resident")
    res_frac <- numeric(num_records)
    
    for (t in seq_len(time_steps)) {
      is_migrating <- runif(1) < mig_rate
      focal <- sample(popsize, 1)
      
      if (is_migrating) {
        pop$trait[focal] <- "immigrant"
      } else {
        repeat {
          int_part <- sample(popsize, 1)
          if (int_part != focal) break
        }
        if (pop$trait[focal] != pop$trait[int_part] && runif(1) < int_prob_other) {
          change_prob <- ifelse(pop$trait[focal] == "resident", 1 - c_r, 1 - c_i)
          if (runif(1) < change_prob) {
            pop$trait[focal] <- ifelse(pop$trait[focal] == "resident", "immigrant", "resident")
          }
        }
      }
      
      if (t %% record_interval == 0) {
        res_frac[t / record_interval] <- sum(pop$trait == "resident") / popsize
      }
    }
    
    results$seed_used[i] <- base_seed + i
    time_series_list[[i]] <- data.frame(
      time = seq(record_interval, time_steps, by = record_interval),
      resident_fraction = res_frac,
      mig_rate = mig_rate,
      c_i = c_i,
      sim_id = i
    )
  }
  toc()
  save_simulation(output_dir, file_name, results, time_series_list)
  
}

run_homogeneous_simulation <- function(output_dir, file_name, run_number, variant_code, popsize) {
  model_code <- 2
  base_seed <- experiment_number * 1e6 +
    sim_type         * 1e5 +
    interaction_code * 1e4 +
    model_code       * 1e3 +
    variant_code     * 1e2 +
    run_number
  
  results <- expand.grid(mig_rate = migration_rates, c_i = c_i_s)
  results$seed_used <- NA_integer_
  time_series_list <- vector("list", nrow(results))
  
  tic("Homogeneous Simulation")
  for (i in seq_len(nrow(results))) {
    set.seed(base_seed + i)
    mig_rate <- results$mig_rate[i]
    c_i <- results$c_i[i]
    data <- make_homogeneous_network(popsize, mean_degree)
    pop <- data$hom_pop
    adj <- data$adj_list_hom
    res_frac <- numeric(num_records)
    
    for (t in seq_len(time_steps)) {
      is_migrating <- runif(1) < mig_rate
      focal <- sample.int(popsize, 1)
      
      if (is_migrating) {
        pop[focal] <- "immigrant"
      } else {
        neighbor <- adj[[focal]][sample.int(length(adj[[focal]]), 1)]
        if (pop[focal] != pop[neighbor] && runif(1) < int_prob_other) {
          change_prob <- ifelse(pop[focal] == "resident", 1 - c_r, 1 - c_i)
          if (runif(1) < change_prob) {
            pop[focal] <- ifelse(pop[focal] == "resident", "immigrant", "resident")
          }
        }
      }
      
      if (t %% record_interval == 0) {
        res_frac[t / record_interval] <- sum(pop == "resident") / popsize
      }
    }
    
    results$seed_used[i] <- base_seed + i
    time_series_list[[i]] <- data.frame(
      time = seq(record_interval, time_steps, by = record_interval),
      resident_fraction = res_frac,
      mig_rate = mig_rate,
      c_i = c_i,
      sim_id = i
    )
  }
  toc()
  save_simulation(output_dir, file_name, results, time_series_list)
  
}

run_network_simulation <- function(output_dir, file_name, run_number, variant_code, popsize) {
  model_code <- 3
  base_seed <- experiment_number * 1e6 +
    sim_type         * 1e5 +
    interaction_code * 1e4 +
    model_code       * 1e3 +
    variant_code     * 1e2 +
    run_number
  
  results <- expand.grid(mig_rate = migration_rates, c_i = c_i_s)
  results$seed_used <- NA_integer_
  time_series_list <- vector("list", nrow(results))
  
  tic("Network Simulation")
  for (i in seq_len(nrow(results))) {
    set.seed(base_seed + i)
    mig_rate <- results$mig_rate[i]
    c_i <- results$c_i[i]
    data <- make_neg_binom_network(popsize, mean_degree, var_degree)
    pop <- data$network_pop
    adj <- data$adj_list_net
    res_frac <- numeric(num_records)
    
    for (t in seq_len(time_steps)) {
      is_migrating <- runif(1) < mig_rate
      focal <- sample.int(popsize, 1)
      
      if (is_migrating) {
        pop[focal] <- "immigrant"
      } else {
        neighbor <- adj[[focal]][sample.int(length(adj[[focal]]), 1)]
        if (pop[focal] != pop[neighbor] && runif(1) < int_prob_other) {
          change_prob <- ifelse(pop[focal] == "resident", 1 - c_r, 1 - c_i)
          if (runif(1) < change_prob) {
            pop[focal] <- ifelse(pop[focal] == "resident", "immigrant", "resident")
          }
        }
      }
      
      if (t %% record_interval == 0) {
        res_frac[t / record_interval] <- sum(pop == "resident") / popsize
      }
    }
    
    results$seed_used[i] <- base_seed + i
    time_series_list[[i]] <- data.frame(
      time = seq(record_interval, time_steps, by = record_interval),
      resident_fraction = res_frac,
      mig_rate = mig_rate,
      c_i = c_i,
      sim_id = i
    )
  }
  toc()
  save_simulation(output_dir, file_name, results, time_series_list)
}

# RUN ALL SIMULATIONS ------------------------------------------------------
output_dir <- "seed_pop.size_time_r"
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

for (popsize in c(500, 1000, 1500)) {
  variant_code <- variant_codes[[as.character(popsize)]]
  
  for (i in 1:10) {
    run_wellmixed_simulation(output_dir, paste0("s_popsize01_wm_pop", popsize, "_run", i, ".RDS"), i, variant_code, popsize)
    run_homogeneous_simulation(output_dir, paste0("s_popsize01_hom_pop", popsize, "_run", i, ".RDS"), i, variant_code, popsize)
    run_network_simulation(output_dir, paste0("s_popsize01_net_pop", popsize, "_run", i, ".RDS"), i, variant_code, popsize)
  }
}
