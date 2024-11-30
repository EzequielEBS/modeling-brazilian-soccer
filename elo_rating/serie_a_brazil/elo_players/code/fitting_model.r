setwd(paste("C:/Users/ezequiel/OneDrive - Fundacao Getulio Vargas - FGV/",
            "Grad MAp FGV/IC/modeling-brazilian-soccer", sep = ""))

library(cmdstanr)
library(shinystan)
library(rstan)
library(bayesplot)
library(xtable)

# Load the data
load("elo_rating/serie_a_brazil/elo_players/data/players_id.RData")
load("elo_rating/serie_a_brazil/elo_players/data/results_array.RData")
load("elo_rating/serie_a_brazil/elo_players/data/player_array_home.RData")
load("elo_rating/serie_a_brazil/elo_players/data/player_array_away.RData")
n_players <- length(players_id)
n_games <- length(results_array)

# Load the model
model <- cmdstan_model("elo_rating/serie_a_brazil/elo_players/code/elo_player_model.stan")

# Fit the model
fit <- model$sample(data = list(n_players = n_players, 
                                n_games = n_games,
                                players_home_array = player_array_home,
                                players_away_array = player_array_away,
                                results = results_array, 
                                players_id = players_id),
                    chains = 4, iter_warmup = 1000, iter_sampling = 1000, parallel_chains = 4)

fit$save_object(file = "elo_rating/serie_a_brazil/elo_players/code/fit.rds")
fit$save_object(file = "elo_rating/serie_a_brazil/elo_players/code/fit2.rds")


fit <- readRDS("elo_rating/serie_a_brazil/elo_players/code/fit.rds")
stan_fit <- rstan::read_stan_csv(fit$output_files())
launch_shinystan(stan_fit)

mcmc_trace(stan_fit, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
stan_fit

fit_summary <- fit$summary("rating")
fit_summary$player_id <- players_id

players_names <- read.csv("elo_rating/serie_a_brazil/elo_players/data/players_names.csv")
perc_wins <- read.csv("elo_rating/serie_a_brazil/elo_players/data/perc_wins.csv")

fit_summary <- merge(fit_summary, perc_wins, by = "player_id")
fit_summary <- merge(fit_summary, players_names, by = "player_id")

ordered_summary_mean <- fit_summary[order(fit_summary$mean, decreasing = TRUE),]
rownames(ordered_summary_mean) <- 1:nrow(ordered_summary_mean)

ordered_summary_mean[1:20,]
fit$summary("k")

# ordered_summary_mean[ordered_summary_mean$player_name == "Player not found",]

print(xtable(ordered_summary_mean[1:20, c("player_name", "mean", "median", "sd", "mad", "q5", "q95", "rhat", "perc_wins")]), type = "latex", file = "elo_rating/serie_a_brazil/elo_players/data/post_mean_rating_best.tex")
print(xtable(ordered_summary_mean[(nrow(ordered_summary_mean)-20):nrow(ordered_summary_mean), c("player_name", "mean", "median", "sd", "mad", "q5", "q95", "rhat", "perc_wins")]), type = "latex", file = "elo_rating/serie_a_brazil/elo_players/data/post_mean_rating_worst.tex")

write.csv(ordered_summary_mean, "elo_rating/serie_a_brazil/elo_players/data/post_mean_rating.csv")

post_mean_rating <- read.csv("elo_rating/serie_a_brazil/elo_players/data/post_mean_rating.csv")

post_mean_rating[1:20,]
