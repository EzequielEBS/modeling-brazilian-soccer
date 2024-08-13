setwd(paste("C:/Users/ezequiel/OneDrive - Fundacao Getulio Vargas - FGV/",
            "Grad MAp FGV/IC/modeling-brazilian-soccer", sep = ""))

# load necessary libraries
library("rjson")
library("stringr")

# Load the JSON files
games_2013_json <- fromJSON(file="elo_rating/serie_a_brazil/elo_players/data/Serie_A_2013_games.json")
games_2014_json <- fromJSON(file="elo_rating/serie_a_brazil/elo_players/data/Serie_A_2014_games.json")
games_2015_json <- fromJSON(file="elo_rating/serie_a_brazil/elo_players/data/Serie_A_2015_games.json")
games_2016_json <- fromJSON(file="elo_rating/serie_a_brazil/elo_players/data/Serie_A_2016_games.json")
games_2017_json <- fromJSON(file="elo_rating/serie_a_brazil/elo_players/data/Serie_A_2017_games.json")
games_2018_json <- fromJSON(file="elo_rating/serie_a_brazil/elo_players/data/Serie_A_2018_games.json")
games_2019_json <- fromJSON(file="elo_rating/serie_a_brazil/elo_players/data/Serie_A_2019_games.json")
games_2020_json <- fromJSON(file="elo_rating/serie_a_brazil/elo_players/data/Serie_A_2020_games.json")
games_2021_json <- fromJSON(file="elo_rating/serie_a_brazil/elo_players/data/Serie_A_2021_games.json")
games_2022_json <- fromJSON(file="elo_rating/serie_a_brazil/elo_players/data/Serie_A_2022_games.json")
games_2023_json <- fromJSON(file="elo_rating/serie_a_brazil/elo_players/data/Serie_A_2023_games.json")

squads_2013_json <- fromJSON(file="elo_rating/serie_a_brazil/elo_players/data/Serie_A_2013_squads.json")
squads_2014_json <- fromJSON(file="elo_rating/serie_a_brazil/elo_players/data/Serie_A_2014_squads.json")
squads_2015_json <- fromJSON(file="elo_rating/serie_a_brazil/elo_players/data/Serie_A_2015_squads.json")
squads_2016_json <- fromJSON(file="elo_rating/serie_a_brazil/elo_players/data/Serie_A_2016_squads.json")
squads_2017_json <- fromJSON(file="elo_rating/serie_a_brazil/elo_players/data/Serie_A_2017_squads.json")
squads_2018_json <- fromJSON(file="elo_rating/serie_a_brazil/elo_players/data/Serie_A_2018_squads.json")
squads_2019_json <- fromJSON(file="elo_rating/serie_a_brazil/elo_players/data/Serie_A_2019_squads.json")
squads_2020_json <- fromJSON(file="elo_rating/serie_a_brazil/elo_players/data/Serie_A_2020_squads.json")
squads_2021_json <- fromJSON(file="elo_rating/serie_a_brazil/elo_players/data/Serie_A_2021_squads.json")
squads_2022_json <- fromJSON(file="elo_rating/serie_a_brazil/elo_players/data/Serie_A_2022_squads.json")
squads_2023_json <- fromJSON(file="elo_rating/serie_a_brazil/elo_players/data/Serie_A_2023_squads.json")

# Combine the JSON files into a list
list_games <- list(
                    games_2013_json,
                    games_2014_json, 
                    games_2015_json, 
                    games_2016_json, 
                    games_2017_json, 
                    games_2018_json, 
                    games_2019_json, 
                    games_2020_json, 
                    games_2021_json, 
                    games_2022_json, 
                    games_2023_json)

list_squads <- list(
                    squads_2013_json,
                    squads_2014_json, 
                    squads_2015_json, 
                    squads_2016_json, 
                    squads_2017_json, 
                    squads_2018_json, 
                    squads_2019_json, 
                    squads_2020_json, 
                    squads_2021_json, 
                    squads_2022_json, 
                    squads_2023_json)

# combine the list into a single json file
all_games <- do.call(c, list_games)
all_squads <- do.call(c, list_squads)

# Create a list to store the data
data_games <- list()

for (j in 1:length(all_squads)) {
    # Create a list with all the players in the squads
    players_list_home <- list()
    player_times_list_home <- list()
    players_list_away <- list()
    player_times_list_away <- list()

    for (i in 1:length(all_squads[[j]])) {
        players_list_home[[i]] <- all_squads[[j]][[i]][["Home"]][["Squad"]]
        players_list_away[[i]] <- all_squads[[j]][[i]][["Away"]][["Squad"]]
        player_times_list_home[[i]] <- rep(all_squads[[j]][[i]][["Time"]], length(players_list_home[[i]]))
        player_times_list_away[[i]] <- rep(all_squads[[j]][[i]][["Time"]], length(players_list_away[[i]]))
    }

    # Get the unique players for each team
    unique_players_home <- unique(unlist(players_list_home))
    unique_players_away <- unique(unlist(players_list_away))

    # Combine the players and times into a single list
    all_players <- c(unlist(players_list_home), unlist(players_list_away))
    all_times <- c(unlist(player_times_list_home), unlist(player_times_list_away))

    # Calculate the total time played by each player
    player_total_times <- tapply(all_times, all_players, sum)
    player_total_times

    # Create a list with the total time played by each player
    player_times_home <- as.list(player_total_times[names(player_total_times) %in% unique_players_home])
    player_times_away <- as.list(player_total_times[names(player_total_times) %in% unique_players_away])

    home_team <- all_games[[j]]$Home
    away_team <- all_games[[j]]$Away
    result <- all_games[[j]]$Result

    data_games[[j]] <- list(Home = home_team, Away = away_team, Result = result,
                            Players = list(Home = unique(unlist(players_list_home)), Away = unique(unlist(players_list_away))),
                            Times = list(Home = player_times_home, Away = player_times_away))
}

# Number of games
n_games <- length(data_games)

# Initialize the 3D array for the players
player_array_home <- array(dim = c(n_games, 16, 2))

# Initialize the array for the results
results_array <- array(dim = c(n_games, 1))

# Fill the arrays
for (i in 1:n_games) {
    result <- data_games[[i]][["Result"]]
    goals_home <- as.integer(substr(result, 1, 1))
    goals_away <- as.integer(substr(result, nchar(result) - 1, nchar(result)))  
    if (goals_home > goals_away) {
        results_array[i] <- 1
    } else if (goals_home < goals_away) {
        results_array[i] <- 0
    } else {
        results_array[i] <- 0.5
    }
    for (j in 1:length(data_games[[i]][[5]][["Home"]])) {
        # get the player id and time played
        player_home_id <- names(data_games[[i]][[5]][["Home"]])[j]
        player_time <- data_games[[i]][[5]][["Home"]][[player_home_id]]
        # fill the array
        player_array_home[i, j, 1] <- as.integer(player_home_id)
        player_array_home[i, j, 2] <- as.numeric(player_time)
    }
}

player_array_home[is.na(player_array_home)] <- 0

player_array_away <- array(dim = c(n_games, 16, 2))

for (i in 1:n_games) {
    for (j in 1:length(data_games[[i]][[5]][["Away"]])) {
        player_away_id <- names(data_games[[i]][[5]][["Away"]])[j]
        player_time <- data_games[[i]][[5]][["Away"]][[player_away_id]]
        player_array_away[i, j, 1] <- as.integer(player_away_id)
        player_array_away[i, j, 2] <- as.numeric(player_time)
    }
}

player_array_away[is.na(player_array_away)] <- 0

home_players_id <- unique(player_array_home[, , 1])
away_players_id <- unique(player_array_away[, , 1])

players_id <- unique(c(home_players_id, away_players_id))

players_id <- players_id[players_id != 0]

players_id <- as.array(players_id)

# Save the data
save(player_array_home, file = "elo_rating/serie_a_brazil/elo_players/data/player_array_home.RData")
save(player_array_away, file = "elo_rating/serie_a_brazil/elo_players/data/player_array_away.RData")
save(results_array, file = "elo_rating/serie_a_brazil/elo_players/data/results_array.RData")
save(players_id, file = "elo_rating/serie_a_brazil/elo_players/data/players_id.RData")

j <- 1
for (i in 1:length(games_2023_json)){
    if (games_2023_json[[i]][["Home"]] == "Flamengo / RJ"){
        print(games_2023_json[[i]][["Players"]])
        j <- j + 1
    }
    if (j == 5){
        break
    }
}

find_player_name <- function(id) {
    for (i in 1:length(all_games)){
        for (j in 1:length(all_games[[i]][["Players"]])){
            text <- all_games[[i]][["Players"]][[j]][1]
            if (substr(text, nchar(text) - 5, nchar(text)) == id){
                return(all_games[[i]][["Players"]][[j]])
            }
        }
    }
    return("Player not found")
}

clubs <- vector()

for (i in 1:length(all_games)){
    clubs <- c(clubs, all_games[[i]][["Home"]], all_games[[i]][["Away"]])
}

df_clubs <- data.frame(id = 1:length(unique(clubs)), club = unique(clubs))

write.csv(df_clubs, file = "elo_rating/serie_a_brazil/elo_clubs/data/clubs.csv", row.names = FALSE)

df_clubs <- read.csv("elo_rating/serie_a_brazil/elo_clubs/data/clubs.csv")
clubs_id <- array(dim = c(length(all_games), 2))

for (i in 1:length(all_games)){
    home_club <- all_games[[i]][["Home"]]
    away_club <- all_games[[i]][["Away"]]
    home_id <- df_clubs[df_clubs$club == home_club, "id"]
    away_id <- df_clubs[df_clubs$club == away_club, "id"]
    clubs_id[i, 1] <- as.integer(home_id)
    clubs_id[i, 2] <- as.integer(away_id)
}

save(results_array_clubs, file = "elo_rating/serie_a_brazil/elo_clubs/data/clubs_id.RData")

# BH: 401839
# Gabi: 337830
# Arrascaeta: 521990

# 749306    374835    341959    305973    436545    329524    307772    440261    444618    174997
# 0.8571429 0.8461538 0.8461538 0.8333333 0.8333333 0.8235294 0.8181818 0.8181818 0.7777778 0.7692308

#    141341    168982    155541    310312    308887    694075    160686    305360    340736    328835
# 0.6666667 0.6545455 0.6516854 0.6486486 0.6428571 0.6415094 0.6406250 0.6250000 0.6197183 0.6181818

# 141341    160686    138908    180213    459915    307273    140841    303237    130014    293428 
# 0.6172840 0.6147569 0.6028736 0.5833333 0.5747277 0.5731364 0.5720679 0.5645224 0.5595656 0.5588753