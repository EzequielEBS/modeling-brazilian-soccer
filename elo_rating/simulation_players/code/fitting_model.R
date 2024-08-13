setwd(paste("C:/Users/ezequiel/OneDrive - Fundacao Getulio Vargas - FGV/",
            "Grad MAp FGV/IC/modeling-brazilian-soccer", sep = ""))

library(matrixStats)
library(pracma)

set.seed(03052024)

source("elo_rating/simulation_players/code/functions.R")

n_teams <- 2
n_players <- 16 
n_matches <- 1000
n_st <- 11
max_changes <- 5
mu <- 100 
sd <- 20
sigma <- 400 
k <- 2

sim_data(n_teams, 
         n_players, 
         n_matches, 
         n_st,
         max_changes,
         mu, 
         sd,
         sigma, 
         k)

rating_players <- read.csv("elo_rating/simulation_players/data/simulated_rating.csv")
rating_players[, "rating"] <- rating_players[, "rating"] / rating_players[1, "rating"]
results <- read.csv("elo_rating/simulation_players/data/simulated_results.csv")

rating_players
head(results)

# m <- 1

# initial_values <- c(n_teams*n_players*m - sum(rep(1, n_teams*n_players - 1)),
#                     rep(1, n_teams*n_players - 1),
#                     4)

initial_values <- c(rep(1, n_teams*n_players - 1),
                    4)

opt <- constrOptim(initial_values, 
            lfn,
            grad = grad_lfn,
            # ui=rbind(c(rep(-1/(n_teams*n_players), n_teams*n_players), 0),
            #          c(rep(1/(n_teams*n_players), n_teams*n_players), 0),
            #          diag(n_teams*n_players + 1)),
            # ui=rbind(c(-1, rep(0, n_teams*n_players)),
            #          c(1, rep(0, n_teams*n_players)),
            #          diag(n_teams*n_players + 1)),
            ui = diag(n_teams*n_players),
            # ci=c(-m - 10^(-5),
            #      m - 10^(-5),
            #      rep(0, n_teams*n_players + 1)),
            ci = rep(0, n_teams*n_players),
            results = results)
            # method = "Nelder-Mead")


rating_players$rating_hat <- c(1,opt$par[1:(n_teams*n_players-1)])
k_hat <- opt$par[n_teams*n_players]

rating_players
