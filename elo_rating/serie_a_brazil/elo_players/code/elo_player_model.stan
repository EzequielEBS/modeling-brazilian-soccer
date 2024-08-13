functions {
    int find_index(array[] real x, real y) {
        int n = size(x);
        int index = 0;
        for (i in 1:n) {
            if (x[i] == y) {
                index = i;
                break;
            }
        }
        return index;
    }
}

data {
    int<lower=0> n_players; // number of players
    int<lower=0> n_games; // number of games
    array[n_players] int<lower=0> players_id; // players id
    array[n_games] real<lower=0, upper=1> results; // results of each game
    array[n_games, 16, 2] real players_home_array; // home players and times for each game
    array[n_games, 16, 2] real players_away_array; // away players and times for each game
}

parameters {
    array[n_players] real<lower=0> rating; // players rating
    real<lower=0> k; // draw factor
}

transformed parameters {
    array[n_games] real<lower=0> home_teams_rating;
    array[n_games] real<lower=0> away_teams_rating;

    for (g in 1:n_games) {
        real home_aux = 0;
        for (i in 1:16) {
            if (players_home_array[g, i, 1] == 0) {
                break;
            }
            home_aux += rating[find_index(players_id, players_home_array[g, i, 1])]*players_home_array[g, i, 2];
        }
        home_teams_rating[g] = home_aux/sum(players_home_array[g, :, 2]);
        real away_aux = 0;
        for (i in 1:16) {
            if (players_away_array[g, i, 1] == 0) {
                break;
            }
            away_aux += rating[find_index(players_id, players_away_array[g, i, 1])]*players_away_array[g, i, 2];
        }
        away_teams_rating[g] = away_aux/sum(players_away_array[g, :, 2]);
    }
}

model {
    // priors
    for (i in 1:n_players) {
        target += normal_lpdf(rating[i] | 1000, 200);  
    }
    target += gamma_lpdf(k | 2, 1);

    // likelihood
    for (g in 1:n_games) {
        real result = results[g];
        real lden = log(home_teams_rating[g]^2 + away_teams_rating[g]^2 + home_teams_rating[g]*away_teams_rating[g]*k);
        if (result == 1) {
            target += 2*log(home_teams_rating[g]) - lden;
        } else if (result == 0) {
            target += 2*log(away_teams_rating[g]) - lden;
        } else {
            target += log(k) + log(home_teams_rating[g]) + log(away_teams_rating[g]) - lden;
        }
    }
}