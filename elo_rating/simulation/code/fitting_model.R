setwd(paste("C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/",
            "Grad MAp FGV/IC/modeling-brazilian-soccer", sep = ""))

source("elo_rating/simulation/code/functions.R")

set.seed(26122023)


# DOIS TIMES - SEM EMPATES


n_teams <- 2
n_matches <- 10
mu <- 20
sd <- 4
k <- 0

sim_data(n_teams, n_matches, mu, sd, k)

rating <- read.csv("elo_rating/simulation/data/simulated_rating.csv")
results <- read.csv("elo_rating/simulation/data/simulated_results.csv")

initial_values <- c(rep(100, n_teams-1))
opt <- optim(initial_values, fn = lfn_elo_rating, results = results,
             lower= rep(10^(-10), n_teams-1), method="L-BFGS-B")
rating$r_hat <- c(1, opt$par)

rating


# MAIS DE DOIS TIMES - SEM EMPATES


n_teams <- 10
n_matches <- 2
mu <- 20
sd <- 4
k <- 0

sim_data(n_teams, n_matches, mu, sd, k)

rating <- read.csv("elo_rating/simulation/data/simulated_rating.csv")
results <- read.csv("elo_rating/simulation/data/simulated_results.csv")

initial_values <- c(rep(100, n_teams-1))
opt <- optim(initial_values, fn = lfn_elo_rating, results = results,
               lower= rep(10^(-10), n_teams-1), method="L-BFGS-B")
r_hat <- data.frame(team = c(1:n_teams),
                    rating = c(1, opt$par))

r_hat
rating


# DOIS TIMES - COM EMPATES, SEM ESTIMAR K


n_teams <- 2
n_matches <- 100
mu <- 20
sd <- 4
k <- 5

sim_data(n_teams, n_matches, mu, sd, k)

rating <- read.csv("elo_rating/simulation/data/simulated_rating.csv")
results <- read.csv("elo_rating/simulation/data/simulated_results.csv")

initial_values <- c(rep(100, n_teams-1))
opt <- optim(initial_values, fn = lfn_elo_rating_with_draw_k, results = results, 
             k = k, lower= rep(10^(-10), n_teams-1), method="L-BFGS-B")
r_hat <- data.frame(team = c(1:n_teams),
                    rating = c(1, opt$par))

r_hat
rating


# DOIS TIMES - COM EMPATES, ESTIMANDO K


n_teams <- 2
n_matches <- 1000
mu <- 20
sd <- 4
k <- 5

sim_data(n_teams, n_matches, mu, sd, k)

rating <- read.csv("elo_rating/simulation/data/simulated_rating.csv")
results <- read.csv("elo_rating/simulation/data/simulated_results.csv")

initial_values <- c(rep(100, n_teams))
opt <- optim(initial_values, fn = lfn_elo_rating_with_draw, results = results,
             lower= c(rep(10^(-10), n_teams-1), 0), method="L-BFGS-B")
r_hat <- data.frame(team = c(1:n_teams),
                    rating = c(1, opt$par[1:(n_teams-1)]))
k_hat <- opt$par[n_teams]

r_hat
rating
k_hat
k


# MAIS DE DOIS TIMES - COM EMPATES, SEM ESTIMAR K


n_teams <- 10
n_matches <- 10
mu <- 20
sd <- 4
k <- 5

sim_data(n_teams, n_matches, mu, sd, k)

rating <- read.csv("elo_rating/simulation/data/simulated_rating.csv")
results <- read.csv("elo_rating/simulation/data/simulated_results.csv")

initial_values <- c(rep(100, n_teams-1))
opt <- optim(initial_values, fn = lfn_elo_rating_with_draw_k, results = results, 
             k = k, lower= rep(10^(-10), n_teams-1), method="L-BFGS-B")
r_hat <- data.frame(team = c(1:n_teams),
                    rating = c(1, opt$par))

r_hat
rating


# MAIS DE DOIS TIMES - COM EMPATES, ESTIMANDO K


n_teams <- 5
n_matches <- 20
mu <- 20
sd <- 4
k <- 0

sim_data(n_teams, n_matches, mu, sd, k)

rating <- read.csv("elo_rating/simulation/data/simulated_rating.csv")
results <- read.csv("elo_rating/simulation/data/simulated_results.csv")

initial_values <- c(rep(100, n_teams))
opt <- optim(initial_values, fn = lfn_elo_rating_with_draw, results = results,
             lower= c(rep(10^(-10), n_teams-1), 0), method="L-BFGS-B")
r_hat <- data.frame(team = c(1:n_teams),
                    rating = c(1, opt$par[1:(n_teams-1)]))
k_hat <- opt$par[n_teams]

r_hat
rating
k_hat
k