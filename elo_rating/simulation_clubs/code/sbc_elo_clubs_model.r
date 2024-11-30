setwd(paste("C:/Users/ezequiel/OneDrive - Fundacao Getulio Vargas - FGV/",
            "Grad MAp FGV/IC/modeling-brazilian-soccer", sep = ""))

# Load the necessary libraries
library(cmdstanr)
library(SBC)
library(matrixStats)
library(future)
plan(multisession)

options(mc.cores = parallel::detectCores())
options(SBC.min_chunk_size = 5)

source("elo_rating/simulation_clubs/code/functions.R")

# Define the generator function
elo_generator_single <- function(N) {
    n_teams <- 20
    mu <- 1000
    sd <- 200
    k <- rgamma(n = 1, shape = 2, rate = 1)
    
    sim_data(n_teams, 
             N,
             mu, 
             sd,
             k)
    ratings <- read.csv("elo_rating/simulation_clubs/data/simulated_rating.csv")
    results <- read.csv("elo_rating/simulation_clubs/data/simulated_results.csv")
    n_games <- nrow(results)

    results_array <- array(dim = c(n_games, 1))
    clubs_id <- array(dim = c(n_games, 2))

    results_array[, 1] <- results$result
    clubs_id[, 1] <- results$home
    clubs_id[, 2] <- results$away
    
    list(
        variables = list(
        rating = ratings$rating,
        k = k
        ),
        generated = list(
        n_clubs = n_teams,
        n_games = n_games,
        results = results_array,
        clubs_id = clubs_id
        )
    )
}

n_sims <- 250  # Number of SBC iterations to run

# Generate the dataset
elo_generator <- SBC_generator_function(elo_generator_single, N = 1)
elo_dataset <- generate_datasets(
  elo_generator, 
  n_sims)

# Define the model
model <- cmdstan_model("elo_rating/serie_a_brazil/elo_clubs/code/elo_club_model.stan")

# Define the backend
elo_backend <- SBC_backend_cmdstan_sample(
    model, iter_warmup = 2000, iter_sampling = 2500)

# Define the cache directory
cache_dir <- "./elo_rating/simulation_clubs/SBC_cache"
if(!dir.exists(cache_dir)) {
    dir.create(cache_dir)
}

# Run the SBC
results <- compute_SBC(elo_dataset, elo_backend, cache_mode = "results", 
                    cache_location = file.path(cache_dir, "results"))
results <- readRDS(file.path(cache_dir, "results.rds"))
results <- results$result

write.csv(results$stats, file.path(cache_dir, "stats.csv"), row.names = FALSE)

# Plot the results
plot_rank_hist(results)
plot_ecdf(results)
plot_ecdf_diff(results)

# Compute the proportion of the ECDF that is outside the 95% confidence interval
graph <- plot_ecdf(results)
plot_data <- ggplot_build(graph)$data
confidence_interval <- plot_data[[1]]
ecdf <- plot_data[[2]]

df1 <- merge(select(confidence_interval, - c(colour, fill, group, flipped_aes, linewidth, linetype, alpha, y)),
             select(ecdf, - c(colour, fill, group, linewidth, linetype, alpha)),
             by = c("PANEL", "x"), all.x = TRUE) %>%
       group_by(PANEL, x) %>%
       summarize(ymax = max(ymax, na.rm = TRUE),
                 ymin = max(ymin, na.rm = TRUE),
                 y = max(y, na.rm = TRUE))

df1$out <- (df1$ymax < df1$y) + (df1$ymin > df1$y)
df1 <- df1 %>% group_by(PANEL) %>% summarise(out_ratio = sum(out))
df1$out_ratio <- df1$out_ratio / length(unique(ecdf$x))
df1$out <- df1$out_ratio > ALPHA

# Compute the proportion of the ECDF difference that is outside the 95% confidence interval
graph <- plot_ecdf_diff(results)
plot_data <- ggplot_build(graph)$data
confidence_interval <- plot_data[[1]]
ecdf <- plot_data[[2]]

df2 <- merge(select(confidence_interval, - c(colour, fill, group, flipped_aes, linewidth, linetype, alpha, y)),
             select(ecdf, - c(colour, fill, group, linewidth, linetype, alpha)),
             by = c("PANEL", "x"), all.x = TRUE) %>%
       group_by(PANEL, x) %>%
       summarize(ymax = max(ymax, na.rm = TRUE),
                 ymin = max(ymin, na.rm = TRUE),
                 y = max(y, na.rm = TRUE))

df2$out <- (df2$ymax < df2$y) + (df2$ymin > df2$y)
df2 <- df2 %>% group_by(PANEL) %>% summarise(out_ratio = sum(out))
df2$out_ratio <- df2$out_ratio / length(unique(ecdf$x))
df2$out <- df2$out_ratio > ALPHA

final_df <- merge(df1, df2, by = "PANEL", suffixes = c("", "_diff"))
c(mean(as.numeric(final_df$out)), mean(as.numeric(final_df$out_diff)))
