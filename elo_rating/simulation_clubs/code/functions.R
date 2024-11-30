lsigma <- function(ra, rb){
  result <- log(ra) - log(ra + rb)
  return(result)
}

lsigma_with_draw <- function(x, ra, rb, k){
  lden <- logSumExp(c(2*log(ra), 2*log(rb), log(ra)+log(rb)+log(k)))
  if(x == 0){
    result <- 2*log(rb) - lden
  }
  else if(x == 1){
    result <- 2*log(ra) - lden
  }
  else if(x == 1/2) {
    result <- log(ra) + log(rb) + log(k) - lden
  }
  else{
    result <- 0
  }
  return(result)
}

sim_data <- function(n_teams, n_matches, mu, sd, k){
  r_n <- rnorm(n_teams, mu , sd)
  
  rating <- data.frame(team = c(1:n_teams),
                       rating = r_n)
  
  matches <- data.frame()
  
  for(home_team in 1:n_teams){
    home_i <- rep(home_team, n_matches*(n_teams-1))
    away_i <- rep(c(1:n_teams)[c(1:n_teams) != home_team], n_matches)
    
    matches_home_i <- data.frame(home = home_i,
                                 away = away_i,
                                 result = rep(0, n_matches*(n_teams-1)))
    
    matches <- rbind(matches, matches_home_i)
  }
  
  for(i in 1:dim(matches)[1]){
    home_team <- matches[i, "home"]
    away_team <- matches[i, "away"]
    r_home <- rating[rating["team"] == home_team, "rating"]
    r_away <- rating[rating["team"] == away_team, "rating"]
    u <- runif(1)
    
    if(k == 0){
      if( u < exp(lsigma(r_home, r_away))){
        matches[i, "result"] <- 1
      }
    }
    else{
      if((u >= 0) & 
         (u < exp(lsigma_with_draw(0, r_home, r_away, k)))){
        matches[i, "result"] <- 0
      }
      else if( (u >= exp(lsigma_with_draw(0, r_home, r_away, k))) & 
               (u < exp(lsigma_with_draw(0, r_home, r_away, k)) +
                    exp(lsigma_with_draw(1/2, r_home, r_away, k)))
             ){
        matches[i, "result"] <- 1/2
      }
      else if( (u >= exp(lsigma_with_draw(0, r_home, r_away, k)) +
                exp(lsigma_with_draw(1/2, r_home, r_away, k))) & 
               (u < 1)
             ){
        matches[i, "result"] <- 1
      }
    }
  }
  write.csv(rating, 
            "elo_rating/simulation_clubs/data/simulated_rating.csv", 
            row.names=FALSE)
  write.csv(matches, 
            "elo_rating/simulation_clubs/data/simulated_results.csv", 
            row.names=FALSE)
}


lfn_elo_rating <- function(r, results){
  n_r <- length(r)
  r_n <- c(1, r[1:n_r])
  rating <- data.frame(team = c(1:(n_r + 1)),
                       rating = r_n)
  n_matches <- dim(results)[1]
  
  lfn <- 0
  for(i in 1:n_matches){
    home_team <- results[i, "home"]
    away_team <- results[i, "away"]
    result <- results[i, "result"]
    r_h <- rating[rating["team"] == home_team, "rating"]
    r_a <- rating[rating["team"] == away_team, "rating"]
    
    if(result == 1){
      lfn <- lfn + log(r_h) - log(r_h + r_a)
    }
    else if(result == 0){
      lfn <- lfn + log(r_a) - log(r_h + r_a)
    }
  }
  return(-lfn)
}


lfn_elo_rating_with_draw_k <- function(r, results, k){
  n_r <- length(r)
  r_n <- c(1, r[1:n_r])
  rating <- data.frame(team = c(1:(n_r+1)),
                       rating = r_n)
  n_matches <- dim(results)[1]
  
  lfn <- 0
  for(i in 1:n_matches){
    home_team <- results[i, "home"]
    away_team <- results[i, "away"]
    result <- results[i, "result"]
    r_h <- rating[rating["team"] == home_team, "rating"]
    r_a <- rating[rating["team"] == away_team, "rating"]
    
    if(result == 1){
      lfn <- lfn + lsigma_with_draw(1, r_h, r_a, k)
    }
    else if(result == 1/2){
      lfn <- lfn + lsigma_with_draw(1/2, r_h, r_a, k)
    }
    else if(result == 0){
      lfn <- lfn + lsigma_with_draw(0, r_h, r_a, k)
    }
  }
  return(-lfn)
}


lfn_elo_rating_with_draw <- function(r, results){
  n_r <- length(r)
  r_n <- c(1, r[1:(n_r-1)])
  k <- r[n_r]
  rating <- data.frame(team = c(1:n_r),
                       rating = r_n)
  n_matches <- dim(results)[1]
  
  lfn <- 0
  for(i in 1:n_matches){
    home_team <- results[i, "home"]
    away_team <- results[i, "away"]
    result <- results[i, "result"]
    r_h <- rating[rating["team"] == home_team, "rating"]
    r_a <- rating[rating["team"] == away_team, "rating"]
    
    if(result == 1){
      lfn <- lfn + lsigma_with_draw(1, r_h, r_a, k)
    }
    else if(result == 1/2){
      lfn <- lfn + lsigma_with_draw(1/2, r_h, r_a, k)
    }
    else if(result == 0){
      lfn <- lfn + lsigma_with_draw(0, r_h, r_a, k)
    }
  }
  print(rating)
  print(k)
  return(-lfn)
}
