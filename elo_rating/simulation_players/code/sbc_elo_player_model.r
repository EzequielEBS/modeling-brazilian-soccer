setwd(paste("C:/Users/ezequiel/OneDrive - Fundacao Getulio Vargas - FGV/",
            "Grad MAp FGV/IC/modeling-brazilian-soccer", sep = ""))

# Load the necessary libraries
library(cmdstanr)
library(SBC)
library(matrixStats)
library(future)
library(ggplot2)
library(tidyverse)

plan(multisession)

options(mc.cores = parallel::detectCores())
options(SBC.min_chunk_size = 5)

source("elo_rating/simulation_players/code/functions.R")

# Define the generator function
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

n_sims <- 100  # Number of SBC iterations to run

# Generate the datasets
elo_generator <- SBC_generator_function(elo_generator_single, N = 1)
elo_dataset <- generate_datasets(
  elo_generator, 
  n_sims)

# Define the model
model <- cmdstan_model("elo_rating/serie_a_brazil/elo_players/code/elo_player_model.stan")

# Define the backend
elo_backend <- SBC_backend_cmdstan_sample(
    model, iter_warmup = 1000, iter_sampling = 1000, chains = 2)

# Define the cache directory
cache_dir <- "./elo_rating/simulation_players/SBC_cache"
if(!dir.exists(cache_dir)) {
    dir.create(cache_dir)
}

# Compute the SBC
results <- compute_SBC(elo_dataset, elo_backend, cache_mode = "results", 
                    cache_location = file.path(cache_dir, "results"))
results <- readRDS(file.path(cache_dir, "results.rds"))
results <- results$result

write.csv(results$stats, file.path(cache_dir, "stats.csv"), row.names = FALSE)

# Plot the results
plot_rank_hist(results$result, variables = c("k", "rating[1]", "rating[2]", "rating[3]", "rating[4]", "rating[5]"))
plot_ecdf(results$result, variables = c("k", "rating[1]", "rating[2]", "rating[3]", "rating[4]", "rating[5]"))
plot_ecdf_diff(results$result, variables = c("k", "rating[1]", "rating[2]", "rating[3]", "rating[4]", "rating[5]"))


# Compute the proportion of the ECDF that is outside the 95% confidence interval
graph <- plot_ecdf(results)
plot_data <- ggplot_build(graph)$data
confidence_interval <- plot_data[[1]]
ecdf <- plot_data[[2]]
ALPHA <- 0.05

df1 <- merge(select(confidence_interval, - c(colour, fill, group, flipped_aes, linewidth, linetype, alpha, y)),
             select(ecdf, - c(colour, fill, group, linewidth, linetype, alpha)),
             by = c("PANEL", "x"), all.x = TRUE) %>%
       group_by(PANEL, x) %>%
       summarize(ymax = max(ymax, na.rm = TRUE),
                 ymin = max(ymin, na.rm = TRUE),
                 y = max(y, na.rm = TRUE))

df1$out <- (df1$ymax < df1$y) + (df1$ymin > df1$y)
df1 <- df1 %>% group_by(PANEL) %>% summarise(out_ratio = sum(out))
df1$out_ratio <- df1$out_ratio / length(unique(ecdf$x))
df1$out <- df1$out_ratio > ALPHA

# Compute the proportion of the ECDF difference that is outside the 95% confidence interval
graph <- plot_ecdf_diff(results)
plot_data <- ggplot_build(graph)$data
confidence_interval <- plot_data[[1]]
ecdf <- plot_data[[2]]

df2 <- merge(select(confidence_interval, - c(colour, fill, group, flipped_aes, linewidth, linetype, alpha, y)),
             select(ecdf, - c(colour, fill, group, linewidth, linetype, alpha)),
             by = c("PANEL", "x"), all.x = TRUE) %>%
       group_by(PANEL, x) %>%
       summarize(ymax = max(ymax, na.rm = TRUE),
                 ymin = max(ymin, na.rm = TRUE),
                 y = max(y, na.rm = TRUE))

df2$out <- (df2$ymax < df2$y) + (df2$ymin > df2$y)
df2 <- df2 %>% group_by(PANEL) %>% summarise(out_ratio = sum(out))
df2$out_ratio <- df2$out_ratio / length(unique(ecdf$x))
df2$out <- df2$out_ratio > ALPHA

final_df <- merge(df1, df2, by = "PANEL", suffixes = c("", "_diff"))
c(mean(as.numeric(final_df$out)), mean(as.numeric(final_df$out_diff)))
