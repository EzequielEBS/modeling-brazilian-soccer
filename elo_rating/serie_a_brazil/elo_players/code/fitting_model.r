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

player_array_home[c(1:5),,]


fit <- readRDS("elo_rating/serie_a_brazil/elo_players/code/fit.rds")
stan_fit <- rstan::read_stan_csv(fit$output_files())
launch_shinystan(stan_fit)

mcmc_trace(stan_fit, facet_args = list(ncol = 1, scales = "free_y", strip.position="left"))
stan_fit

fit_summary <- fit$summary("rating")
fit_summary$player_id <- players_id

head(fit_summary)
fit$summary("k")

ordered_summary_mean <- fit_summary[order(fit_summary$mean, decreasing = TRUE),]

head(ordered_summary_mean)

players_names <- vector()

for (i in 1:nrow(ordered_summary_mean)) {
    player_id <- ordered_summary_mean$player_id[i]
    player_name <- find_player_name(player_id)[1]
    print(player_name)
    players_names[i] <- player_name
}

ordered_summary_mean$player_name <- players_names
ordered_summary_mean <- ordered_summary_mean[,c(c(1, 11, 12), c(2:10))]

ordered_summary_mean[1:20,]
fit$summary("k")

# ordered_summary_mean[ordered_summary_mean$player_name == "Player not found",]

print(xtable(ordered_summary_mean[1:20,]), type = "latex", file = "elo_rating/serie_a_brazil/elo_players/data/post_mean_rating.tex")

write.csv(ordered_summary_mean, "elo_rating/serie_a_brazil/elo_players/data/post_mean_rating.csv")





###############################################################################################
###############################################################################################



load("elo_rating/serie_a_brazil/elo_players/data/results_array_clubs.RData")
