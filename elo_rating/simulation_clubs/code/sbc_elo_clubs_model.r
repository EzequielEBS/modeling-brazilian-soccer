setwd(paste("C:/Users/ezequiel/OneDrive - Fundacao Getulio Vargas - FGV/",
            "Grad MAp FGV/IC/modeling-brazilian-soccer", sep = ""))

library(cmdstanr)
library(SBC)
library(matrixStats)
library(future)
plan(multisession)

options(mc.cores = parallel::detectCores())
options(SBC.min_chunk_size = 5)

source("elo_rating/simulation_clubs/code/functions.R")

elo_generator_single <- function(N) {
    n_teams <- 20
    mu <- 1000
    sd <- 200
    k <- rgamma(n = 1, shape = 2, rate = 1)
    
    sim_data(n_teams, 
             N,
             mu, 
             sd,
             k)
    ratings <- read.csv("elo_rating/simulation/data/simulated_rating.csv")
    results <- read.csv("elo_rating/simulation/data/simulated_results.csv")
    n_games <- nrow(results)

    results_array <- array(dim = c(n_games, 1))
    clubs_id <- array(dim = c(n_games, 2))

    results_array[, 1] <- results$result
    clubs_id[, 1] <- results$home
    clubs_id[, 2] <- results$away
    
    list(
        variables = list(
        rating = ratings$rating,
        k = k
        ),
        generated = list(
        n_clubs = n_teams,
        n_games = n_games,
        results = results_array,
        clubs_id = clubs_id
        )
    )
}

n_sims <- 250  # Number of SBC iterations to run

elo_generator <- SBC_generator_function(elo_generator_single, N = 1)
elo_dataset <- generate_datasets(
  elo_generator, 
  n_sims)

model <- cmdstan_model("elo_rating/serie_a_brazil/elo_clubs/code/elo_club_model.stan")
elo_backend <- SBC_backend_cmdstan_sample(
    model, iter_warmup = 2000, iter_sampling = 2500)

cache_dir <- "./elo_rating/simulation/SBC_cache"
if(!dir.exists(cache_dir)) {
    dir.create(cache_dir)
}
results <- compute_SBC(elo_dataset, elo_backend, cache_mode = "results", 
                    cache_location = file.path(cache_dir, "results"))

plot_rank_hist(results)
plot_ecdf(results)
plot_ecdf_diff(results)

