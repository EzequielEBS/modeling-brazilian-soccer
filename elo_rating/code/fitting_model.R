setwd(paste("C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/",
            "Grad MAp FGV/IC/modeling-brazilian-soccer", sep = ""))

source("elo_rating/code/functions.R")

n <- 20
mu <- 50
sd <- 1000
k <- 2
s <- 400

sim_data(n, mu, sd, k, s)

rating <- read.csv("elo_rating/data/simulated_rating.csv")
results <- read.csv("elo_rating/data/simulated_results.csv")
  
initial_values <- c(rep(500, n-1), 10)

# opt_1 <- optim(initial_values, fn = lfn_elo_rating, results = results,
#                lower= rep(0,n), method="L-BFGS-B")
# 
# r_hat_1 <- c(1, opt_1$par[1:(n-1)])
# k_hat_1 <- opt_1$par[n]

opt_2 <- constrOptim(initial_values, lfn_elo_rating, NULL, ui = diag(n), 
                     ci = rep(0, n), results = results)

r_hat_2 <- c(1, opt_2$par[1:(n-1)])
k_hat_2 <- opt_2$par[n]



