setwd(paste("C:/Users/ezequiel/OneDrive - Fundacao Getulio Vargas - FGV/",
            "Grad MAp FGV/IC/modeling-brazilian-soccer", sep = ""))

# Load the data
load("elo_rating/serie_a_brazil/elo_players/data/players_id.RData")
load("elo_rating/serie_a_brazil/elo_players/data/results_array.RData")
load("elo_rating/serie_a_brazil/elo_players/data/player_array_home.RData")
load("elo_rating/serie_a_brazil/elo_players/data/player_array_away.RData")
n_players <- length(players_id)
n_games <- length(results_array)

head(player_array_away[,,])
print(dim(player_array_away[results_array == 1,,]))

# Initialize the vector of total time played with zeros
total_time_played <- numeric(length(players_id))
names(total_time_played) <- players_id

# Function to sum the players' times
sum_player_times <- function(player_array) {
  for (game in 1:dim(player_array)[1]) {
    for (player in 1:dim(player_array)[2]) {
      player_id <- as.character(player_array[game, player, 1])
      player_time <- player_array[game, player, 2]
      if (player_id != "0") {
        total_time_played[player_id] <<- total_time_played[player_id] + player_time
      }
    }
  }
}

# Sum the players' times
sum_player_times(player_array_home)
sum_player_times(player_array_away)

# Initialize the vector of total time in games won with zeros
total_time_played_win <- numeric(length(players_id))
names(total_time_played_win) <- players_id

# Function to sum the players' times in games won
sum_player_times_win <- function(player_array) {
  for (game in 1:dim(player_array)[1]) {
    for (player in 1:dim(player_array)[2]) {
      player_id <- as.character(player_array[game, player, 1])
      player_time <- player_array[game, player, 2]
      if (player_id != "0" && results_array[game] == 1) {
        total_time_played_win[player_id] <<- total_time_played_win[player_id] + player_time
      }
    }
  }
}

# Sum the players' times in games won
sum_player_times_win(player_array_home)
sum_player_times_win(player_array_away)

# Initialize the vector of total wins with zeros
total_win <- numeric(length(players_id))
names(total_win) <- players_id

# Function to count the wins of the players
count_win <- function(player_array) {
  for (game in 1:dim(player_array)[1]) {
    for (player in 1:dim(player_array)[2]) {
      player_id <- as.character(player_array[game, player, 1])
      player_time <- player_array[game, player, 2]
      if (results_array[game] == 1 && player_id != "0" && player_time > 0) {
        total_win[player_id] <<- total_win[player_id] + 1
      }
    }
  }
}

# Count the wins of the players
count_win(player_array_home)
count_win(player_array_away)
total_win_sorted <- total_win[order(total_win, decreasing = TRUE)]


# Initialize the vector of total games with zeros
total_games <- numeric(length(players_id))
names(total_games) <- players_id

# Function to count the games of the players
count_games <- function(player_array) {
  for (game in 1:dim(player_array)[1]) {
    for (player in 1:dim(player_array)[2]) {
      player_id <- as.character(player_array[game, player, 1])
      player_time <- player_array[game, player, 2]
      if (player_id != "0" && player_time > 0) {
        total_games[player_id] <<- total_games[player_id] + 1
      }
    }
  }
}

# Count the games of the players
count_games(player_array_home)
count_games(player_array_away)

# Filter the players with more than 50 games
filter_players <- total_games > 50
total_time_played_filtered <- total_time_played[filter_players]
total_time_played_win_filtered <- total_time_played_win[filter_players]
total_games_filtered <- total_games[filter_players]
total_win_filtered <- total_win[filter_players]

n_win_weighted <- total_time_played_win_filtered / (90 * total_games_filtered)
# n_win_weighted <- total_win_filtered / total_games_filtered

n_win_weighted_sorted <- n_win_weighted[order(n_win_weighted, decreasing = TRUE)]
n_win_weighted_sorted[1:10]
head(n_win_weighted_sorted)
players_one <- names(n_win_weighted_sorted[n_win_weighted_sorted == 1])[!is.na(names(n_win_weighted_sorted[n_win_weighted_sorted == 1]))]
total_games[players_one]

total_win_filtered <- total_win_filtered[order(total_win_filtered, decreasing = TRUE)]

df_wins <- data.frame(player_id = names(total_win_filtered),
                      n_win = total_win_filtered)

players_names <- vector()

for (i in 1:nrow(df_wins)) {
    player_id <- df_wins$player_id[i]
    player_name <- find_player_name(player_id)[1]
    print(player_name)
    players_names[i] <- player_name
}

df_wins$player_name <- players_names
df_wins <- df_wins[,c(1, 3, 2)]

head(df_wins)

print(xtable(df_wins[1:20,]), type = "latex", file = "elo_rating/serie_a_brazil/elo_players/data/n_wins.tex")

write.csv(df_wins, "elo_rating/serie_a_brazil/elo_players/data/n_wins.csv")
