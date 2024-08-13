lF_with_draw <- function(x, ra, rb, k){
  # Returns log-probability
  
  # r <- ra-rb
  # lden <- logSumExp(c(r/sigma, -r/sigma, log(k)))
  # if(x == 1){
  #   result <- r/sigma - lden
  # }
  # else if(x == 0){
  #   result <- -r/sigma - lden
  # }
  # else if(x == 1/2) {
  #   result <- log(k) - lden
  # }
  # else{
  #   result <- 0
  # }
  # return(result)
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


sim_player_rating <- function(n_teams, n_players, mu, sd){
  # Returns simulated player ratings 
  
  teams <- c(1:n_teams)
  rating <- data.frame(team = rep(teams, each = n_players),
                       player_id = c(1:(n_players*n_teams)),
                       rating = rnorm(n_teams*n_players, mu, sd))
  return(rating)
}

sim_player_time <- function(n_players, n_st = 11, max_changes = 5){
  # Returns simulated times for players in a match
  
  player_time <- data.frame(id = 1:n_players,
                             time = rep(0, n_players))
  # sample starting players
  id_starting <- sample(1:n_players, n_st)
  player_time[id_starting, "time"] <- 90
  # sample number of substitutions
  n_changes <- sample(0:max_changes, 1)
  # n_changes <- 0
  if (n_changes > 0) {
    # sample players for substitutions
    changed <- sample(id_starting, n_changes)
    # sample substitutions time
    times_changed <- runif(n_changes, 0, 90)
    player_time[changed, "time"] <- times_changed
    # sample substitute players
    id_zero <- as.integer(rownames(player_time[player_time["time"] == 0,]))
    if (length(id_zero) > 1){
      id_new <- sample(id_zero,
                       n_changes)
      player_time[id_new, "time"] <- 90 - times_changed
    } else {
      player_time$time[match(id_zero, rownames(player_time))] <- 90 - times_changed
    }
  }
  return(player_time)
}

sim_data <- function(n_teams = 2, 
                     n_players = 16, 
                     n_matches = 50, 
                     n_st = 11, 
                     max_changes = 5,
                     mu = 100, 
                     sd = 20, 
                     k = 5){
  # Returns complete simulated data
  
  matches <- data.frame()
  
  # create data structure for matches
  for(home_team in 1:n_teams){
    home_i <- rep(home_team, n_matches*(n_teams-1))
    away_i <- rep(c(1:n_teams)[c(1:n_teams) != home_team], n_matches)
    
    matches_i <- data.frame(home = home_i,
                            away = away_i,
                            result = rep(0, n_matches*(n_teams-1)))
    
    matches <- rbind(matches, matches_i)
  }
  
  matches$match <- c(1:(n_teams*n_matches*(n_teams-1)))
  matches <- matches[, c(4, 1, 2, 3)]
  
  # simulate player ratings
  ratings <- sim_player_rating(n_teams, n_players, mu, sd)
  # ratings <- data.frame(team = rep(c(1:2), each = 16),
  #                      player_id = c(1:32),
  #                      rating = c(c(1, 0.5, 0.005, 0.3, 0.2, 0.4, 0.25, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9), 
  #                                 c(0.2, 0.2, 0.01, 0.1, 0.3, 0.4, 0.25, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)))
  
  # ratings <- data.frame(team = rep(c(1:2), each = 16),
  #                      player_id = c(1:32),
  #                      rating = runif(32, 0, 10))

  long_data <- data.frame()
  
  for(i in matches[,"match"]){
    home_team <- matches[i, "home"]
    away_team <- matches[i, "away"]
    match_i <- rep(i, n_players)
    home_i <- rep(home_team, n_players)
    away_i <- rep(away_team, n_players)
    
    player_home_id <- ratings[ratings[,"team"] == home_team, "player_id"]
    player_away_id <- ratings[ratings[,"team"] == away_team, "player_id"]
    
    rating_home <- ratings[ratings[,"team"] == home_team, "rating"]
    rating_away <- ratings[ratings[,"team"] == away_team, "rating"]
    
    player_time_home <- sim_player_time(n_players, n_st, max_changes)[,"time"]
    player_time_away <- sim_player_time(n_players, n_st, max_changes)[,"time"]
    
    # compute team ratings
    r_home <- weighted.mean(rating_home, 
                            player_time_home)
    r_away <- weighted.mean(rating_away, 
                            player_time_away)
    
    # sample result
    u <- runif(1)
    
    if(u <= exp(lF_with_draw(0, r_home, r_away, k)) ){
      matches[i, "result"] <- 0
    }
    else if( (u > exp(lF_with_draw(0, r_home, r_away, k)) ) &
             (u <= exp(lF_with_draw(0, r_home, r_away, k)) +
              exp(lF_with_draw(1/2, r_home, r_away, k)) )
    ){
      matches[i, "result"] <- 1/2
    }
    else{
      matches[i, "result"] <- 1
    }
    
    
    result_i <- rep(matches[i, "result"], n_players)
    
    # create data in long format
    data_i <- data.frame(match = match_i,
                         home = home_i,
                         away = away_i,
                         player_home_id = player_home_id,
                         player_time_home = player_time_home,
                         rating_home = rating_home, 
                         player_away_id = player_away_id,
                         player_time_away = player_time_away,
                         rating_away = rating_away,
                         result = result_i)
    long_data <- rbind(long_data, data_i)
  }
  
  # write csv files
  write.csv(ratings,
            "elo_rating/simulation_players/data/simulated_rating.csv",
            row.names=FALSE)
  write.csv(long_data,
            "elo_rating/simulation_players/data/simulated_results.csv",
            row.names=FALSE)
}


lfn <- function(r, results){
  # Returns log-likelihood
  
  # set parameters
  n_r <- length(r)
  # r_n <- r[1:(n_r-1)]
  r_n <- c(1, r[1:(n_r-1)])
  k <- r[n_r]
  
  matches <- unique(results[, "match"])
  
  # replace ratings with r
  # for(id in 1:(n_r-1)){
  #   results[results[,"player_home_id"] == id, "rating_home"] <- r_n[id]
  #   results[results[,"player_away_id"] == id, "rating_away"] <- r_n[id]
  # }
  for(id in 1:(n_r)){
    results[results[,"player_home_id"] == id, "rating_home"] <- r_n[id]
    results[results[,"player_away_id"] == id, "rating_away"] <- r_n[id]
  }

  lfn <- 0
  for(i in matches){
    match_i <- results[results[, "match"] == i, ]

    result <- unique(match_i[,"result"])
    
    r_h <- weighted.mean(match_i[,"rating_home"], 
                         match_i[,"player_time_home"])
    r_a <- weighted.mean(match_i[,"rating_away"], 
                         match_i[,"player_time_away"]) 
    

    if(result == 1){
      lfn <- lfn + lF_with_draw(1, r_h, r_a, 400, k)
    }
    else if(result == 1/2){
      lfn <- lfn + lF_with_draw(1/2, r_h, r_a, 400, k)
    }
    else if(result == 0){
      lfn <- lfn + lF_with_draw(0, r_h, r_a, 400, k)
    }
  }
  
  print(r)
  
  return(-lfn)
}

grad_lfn <- function(r, results){
  # Returns gradient of log-likelihood
  
  grad(lfn, r, results = results)
}
