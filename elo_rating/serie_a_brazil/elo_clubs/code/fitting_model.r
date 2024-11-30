setwd(paste("C:/Users/ezequiel/OneDrive - Fundacao Getulio Vargas - FGV/",
            "Grad MAp FGV/IC/modeling-brazilian-soccer", sep = ""))

library(cmdstanr)
library(shinystan)
library(rstan)
library(bayesplot)
library(xtable)

# Load the data
load("elo_rating/serie_a_brazil/elo_players/data/results_array.RData")
load("elo_rating/serie_a_brazil/elo_clubs/data/clubs_id.RData")
n_games <- length(results_array)
n_clubs <- max(results_array_clubs[,1])

# Load the model
model <- cmdstan_model("elo_rating/serie_a_brazil/elo_clubs/code/elo_club_model.stan")

# Fit the model
fit <- model$sample(data = list(n_clubs = n_clubs, 
                                n_games = n_games,
                                clubs_id = results_array_clubs[,1:2],
                                results = results_array),
                    chains = 4, iter_warmup = 1000, iter_sampling = 1000, parallel_chains = 4)

fit$save_object(file = "elo_rating/serie_a_brazil/elo_clubs/code/fit.rds")

fit <- readRDS("elo_rating/serie_a_brazil/elo_clubs/code/fit.rds")

fit$summary()

fit_summary <- fit$summary("rating")

df_clubs <- read.csv("elo_rating/serie_a_brazil/elo_clubs/data/clubs.csv")
fit_summary$club <- df_clubs$club

ordered_summary_mean <- fit_summary[order(fit_summary$mean, decreasing = TRUE),]
ordered_summary_mean <- ordered_summary_mean[,c(c(1, 11), c(2:10))]

ordered_summary_mean

fit$summary("k")

print(xtable(ordered_summary_mean), type = "latex", file = "elo_rating/serie_a_brazil/elo_clubs/data/post_mean_rating.tex")

write.csv(ordered_summary_mean, "elo_rating/serie_a_brazil/elo_clubs/data/post_mean_rating.csv")
