setwd(paste("C:/Users/ezequiel/OneDrive - Fundacao Getulio Vargas - FGV/",
            "Grad MAp FGV/IC/modeling-brazilian-soccer", sep = ""))

library(cmdstanr)
library(SBC)
library(matrixStats)
library(future)
plan(multisession)

options(mc.cores = parallel::detectCores())
options(SBC.min_chunk_size = 5)

source("elo_rating/simulation_players/code/functions.R")

elo_generator_single <- function(N) {
    n_teams <- 20
    n_players <- 16
    n_st <- 11
    max_changes <- 5
    mu <- 1000
    sd <- 200
    k <- rgamma(n = 1, shape = 2, rate = 1)
    
    sim_data(n_teams, 
             n_players, 
             N, 
             n_st,
             max_changes,
             mu, 
             sd,
             k)
    rating_players <- read.csv("elo_rating/simulation_players/data/simulated_rating.csv")
    results <- read.csv("elo_rating/simulation_players/data/simulated_results.csv")
    n_games <- max(results$match)

    player_array_home <- array(dim = c(n_games, 16, 2))
    player_array_away <- array(dim = c(n_games, 16, 2))

    for (g in 1:n_games) {
        results_i <- results[results$match == g,]
        player_home_id <- results_i$player_home_id
        player_away_id <- results_i$player_away_id
        player_time_home <- results_i$player_time_home
        player_time_away <- results_i$player_time_away
        player_array_home[g, , 1] <- as.integer(player_home_id)
        player_array_home[g, , 2] <- as.numeric(player_time_home)
        player_array_away[g, , 1] <- as.integer(player_away_id)
        player_array_away[g, , 2] <- as.numeric(player_time_away)

    }
    
    list(
        variables = list(
        rating = rating_players$rating,
        k = k
        ),
        generated = list(
        n_players = n_players*n_teams,
        n_games = n_games,
        players_id = rating_players$player_id,
        results = results[!duplicated(results$match), "result"],
        players_home_array = player_array_home,
        players_away_array = player_array_away
        )
    )
}

set.seed(54882235)
n_sims <- 100  # Number of SBC iterations to run

elo_generator <- SBC_generator_function(elo_generator_single, N = 1)
elo_dataset <- generate_datasets(
  elo_generator, 
  n_sims)

model <- cmdstan_model("elo_rating/serie_a_brazil/elo_players/code/elo_player_model.stan")
elo_backend <- SBC_backend_cmdstan_sample(
    model, iter_warmup = 1000, iter_sampling = 1000, chains = 2)

cache_dir <- "./elo_rating/simulation_players/SBC_cache"
if(!dir.exists(cache_dir)) {
    dir.create(cache_dir)
}

results <- compute_SBC(elo_dataset, elo_backend, cache_mode = "results", 
                    cache_location = file.path(cache_dir, "results"))

results$stats

plot_rank_hist(results)
plot_ecdf(results)
plot_ecdf_diff(results)
