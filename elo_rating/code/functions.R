setwd(paste("C:/Users/ezequ/OneDrive - Fundacao Getulio Vargas - FGV/",
            "Grad MAp FGV/IC/modeling-brazilian-soccer", sep = ""))

set.seed(03112023)

sigma <- function(r, k, s){
  lresult <- r/s - log10(10^(r/s) + 10^(-r/s) + k)
  return(10^lresult)
}


sim_data <- function(n, mu, sd, k, s){
  r_n <- rnorm(n, mu , sd)
  
  rating <- data.frame(team = c(1:n),
                       rating = r_n)
  
  matches <- data.frame()
  
  for(home_team in 1:n){
    home_i <- rep(home_team, n-1)
    away_i <- c(1:n)[c(1:n) != home_team]
    
    matches_home_i <- data.frame(home = home_i,
                                 away = away_i,
                                 result = rep(0, n-1))
    
    matches <- rbind(matches, matches_home_i)
  }
  
  for(home_team in 1:n){
    r_home <- rating[rating["team"] == home_team, "rating"]
    for(away_team in 1:n){
      if(home_team != away_team){
        r_away <- rating[rating["team"] == away_team, "rating"]
        r <- r_home - r_away
        if(k*((sigma(r, k, s)*sigma(-r, k, s))^(1/2)) > 0.5){
          matches[matches["home"] == home_team & 
                    matches["away"] == away_team, "result"] <- 1/2
        }
        else if(sigma(r, k, s) > 0.5){
          matches[matches["home"] == home_team & 
                    matches["away"] == away_team, "result"] <- 1
        }
      }
    }
  }
  
  write.csv(rating, 
            "elo_rating/data/simulated_rating.csv", 
            row.names=FALSE)
  write.csv(matches, 
            "elo_rating/data/simulated_results.csv", 
            row.names=FALSE)
}


lfn_elo_rating <- function(r, results){
  n_r <- length(r)
  r_n <- c(1, r[1:(n_r-1)])
  rating <- data.frame(team = c(1:(n_r)),
                       rating = r_n)
  
  k <- r[n_r]
  s <- 400
  n_matches <- dim(results)[1]
  
  lfn <- 0
  for(i in 1:n_matches){
    home_team <- results[i, "home"]
    away_team <- results[i, "away"]
    result <- results[i, "result"]
    r_h <- rating[rating["team"] == home_team, "rating"]
    r_a <- rating[rating["team"] == away_team, "rating"]
    r_ah <- r_h - r_a
    
    if(result == 1){
      lfn <- lfn + r_ah/s - log10(10^(r_ah/s) + 10^(-r_ah/s) + k)
    }
    else if(result == 0){
      lfn <- lfn - r_ah/s - log10(10^(r_ah/s) + 10^(-r_ah/s) + k)
    }
    else if(result == 1/2){
      lfn <- lfn + log10(k) - log10(10^(r_ah/s) + 10^(-r_ah/s) + k)
    }
  }
  return(-lfn)
}
