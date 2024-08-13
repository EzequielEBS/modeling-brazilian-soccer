data {
    int<lower=0> n_clubs; // number of clubs
    int<lower=0> n_games; // number of games
    array[n_games, 1] real<lower=0, upper=1> results; // results of each game
    array[n_games, 2] int clubs_id; // clubs id
}

parameters {
    array[n_clubs] real<lower=0> rating; // clubs rating
    real<lower=0> k; // draw factor
}

model {
    // priors
    for (i in 1:n_clubs) {
        target += normal_lpdf(rating[i] | 1000, 200);  
    }
    target += gamma_lpdf(k | 2, 1);

    // likelihood
    for (g in 1:n_games) {
        int home_id = clubs_id[g, 1];
        int away_id = clubs_id[g, 2];
        real result = results[g, 1];
        real lden = log(rating[home_id]^2 + rating[away_id]^2 + rating[home_id]*rating[away_id]*k);
        if (result == 1) {
            target += 2*log(rating[home_id]) - lden;
        } else if (result == 0) {
            target += 2*log(rating[away_id]) - lden;
        } else {
            target += log(k) + log(rating[home_id]) + log(rating[away_id]) - lden;
        }
    }
}