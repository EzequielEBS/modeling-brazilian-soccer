setwd(paste("C:/Users/ezequiel/OneDrive - Fundacao Getulio Vargas - FGV/",
            "Grad MAp FGV/IC/modeling-brazilian-soccer", sep = ""))

library(dplyr)

source("elo_rating/serie_a_brazil/code/functions.R")

matches_2023_json <- fromJSON(file="elo_rating/serie_a_brazil/data/Serie_A_2023_games.json")
create_variable(json_to_csv(matches_2023_json))
matches_2023 <- read.csv("elo_rating/serie_a_brazil/data/Serie_A_2023_games.csv")

initial_values <- rep(2, 20)
opt <- optim(initial_values, fn = lfn_elo_rating_with_draw, results = matches_2023,
             lower= rep(10^(-5), 20), method="L-BFGS-B")
r_hat <- data.frame(team = unique(matches_2023[,"home"]),
                    rating = c(1, opt$par[1:19]))
r_hat <- r_hat[order(r_hat$rating, decreasing = TRUE),]
k_hat <- opt$par[20]

r_hat
k_hat
