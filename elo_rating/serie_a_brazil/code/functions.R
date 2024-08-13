setwd(paste("C:/Users/ezequiel/OneDrive - Fundacao Getulio Vargas - FGV/",
            "Grad MAp FGV/IC/modeling-brazilian-soccer", sep = ""))

library("rjson")
library("stringr")

json_to_csv <- function(json_file){
  n_matches <- length(json_file)
  
  matches <- data.frame()
  
  for(i in c(1:n_matches)){
    matches_i <- data.frame(home = json_file[[i]]$Home,
                            away = json_file[[i]]$Away,
                            result = json_file[[i]]$Result)
    matches <- rbind(matches, matches_i)
  }
  return(matches)
}

create_variable <- function(df){
  n_matches <- dim(df)[1]
  
  df[c('goals_home', 'goals_away')] <- str_split_fixed(df$result, " X ", 2)
  
  df$goals_home <- as.numeric(as.character(df$goals_home))
  df$goals_away <- as.numeric(as.character(df$goals_away))
  
  y <- vector()
  
  for(i in c(1:n_matches)){
    goals_home <- df[i, "goals_home"]
    goals_away <- df[i, "goals_away"]
    
    if(goals_home > goals_away){
      y[i] <- 1
    }
    else if(goals_home < goals_away){
      y[i] <- 0
    }
    else{
      y[i] <- 1/2
    }
  }
  
  df["y"] <- y
  df <- df[,c("home", "away", "y")]
  write.csv(df, "elo_rating/serie_a_brazil/data/Serie_A_2023_games.csv",
            row.names = FALSE)
}

lsigma_with_draw <- function(x, ra, rb, k){
  if(x == 0){
    result <- 2*log(rb) - log(ra^2 + rb^2 + ra*rb*k)
  }
  else if(x == 1){
    result <- 2*log(ra) - log(ra^2 + rb^2 + ra*rb*k)
  }
  else if(x == 1/2) {
    result <- log(ra) + log(rb) + log(k) - log(ra^2 + rb^2 + ra*rb*k)
  }
  else{
    result <- 0
  }
  return(result)
}

lfn_elo_rating_with_draw <- function(r, results){
  n_r <- length(r)
  r_n <- c(1, r[1:(n_r-1)])
  k <- r[n_r]
  rating <- data.frame(team = unique(results[, "home"]),
                       rating = r_n)
  n_matches <- dim(results)[1]
  
  print(rating)
  
  lfn <- 0
  for(i in 1:n_matches){
    home_team <- results[i, "home"]
    away_team <- results[i, "away"]
    result <- results[i, "y"]
    r_h <- rating[rating["team"] == home_team, "rating"]
    r_a <- rating[rating["team"] == away_team, "rating"]
    lfn <- lfn + lsigma_with_draw(result, r_h, r_a, k)
    
    if(lsigma_with_draw(result, r_h, r_a, k) == -Inf){
      print("--------------")
      print(i)
      print(r_h)
      print(r_a)
      print(k)
      print(result)
    }
  }
  
  return(-lfn)
}
