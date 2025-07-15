# Cultural_Evolution

This repository contains the simulation used for the Bachelor's thesis project titled:

**Effect of Social Network Structure on Cultural Evolution**  
Author: Flóra Murakeözy-Kis (S5194326)  
Course: Research Project Ecology and Evolution 2b (WBBY911-015)  
Supervisor: Dr. Yagmur Erten  
University of Groningen  
Date: 14 July, 2025

## Project Description

This project investigates how the structure of social networks influences the dynamics of cultural evolution. Specifically, it compares network-based models, in which individuals interact with a fixed set of neighbours, with either a negative binomial (heterogeneous) degree distribution, or a homogeneous degree distribution, to well-mixed model, where individuals can interact with any other member of the population. The project uses R to simulate these dynamics and analyze outcomes such as trait stability, spread, and variability.

### Hábrók Simulations

All simulation scripts used to generate results on the **Hábrók HPC cluster** are located in the `habrok` branch. Each file runs a full set of simulations for a specific experiment:

- `popstructure_habrok.R` – Population structure (final state) - compares the general effect of populations structure
  
- `popsize_habrok.R` – Population size effect (final state) 
- `popsize_time_habrok.R` – Population size effect (time series)

- `meandegr_habrok.R` – Mean degree effect (final state)
- `meandegr_time_habrok.R` – Mean degree effect (time series)
  
- `variance_habrok.R` – Degree variance effect (final state)
- `variance_time_habrok.R` – Degree variance effect (time series)

Each script creates `.RDS` files with the simulation outputs and uses a consistent seeding system for reproducibility.

## Repository Contents

The `main` branch contains all R scripts used to run and visualize the final simulations:

- `main.R`  
  Main script to run simulations and generate figures. Loads parameters, runs model variants, and handles output.
- `parameters.R`  
  Defines all global parameters used across simulations (e.g., population size, interaction probability, cultural conservatim, parameter sweeps).
- `make_population.R`  
  Contains functions to generate well-mixed and network-structured populations.
- `simulate_wellmixed.R`  
  Simulation model for a well-mixed population where individuals interact randomly.
- `simulate_homogenous.R`  
  Simulation model for a homogeneous network with equal degree (e.g., k-regular graph).
- `simulate_network.R`  
  Simulation model for a heterogeneous network with variable degree (e.g., negative binomial distribution).
- `plotting.R`  
  Functions to generate plots and visual summaries of simulation results.
- `fig1.R`  
  Script to generate figures of well-mixed results.
- `fig2.R`  
  Script to generate figures of heterogeneous network results.
- `fig3.R`  
  Script to generate figures of homogeneous network results.


## Requirements

- **R**: This project requires the latest version of R (tested on version 4.4.2).
- **R packages**:
  - `igraph`
  - `ggplot2`
  - `here`
  - `tictoc`

