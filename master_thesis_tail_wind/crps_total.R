library(dplyr)
library(purrr)
setwd("C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/master_thesis_tail_wind")
t_vals <- c(0:20)
obs_vec_complete <- data.frame(obs_vec = numeric(), lead_time = numeric())
fc_data_complete <- list()

lengths <- c()

for (l in 0:21) {
    rm(fc_data, obs_vec, pit_rank)

    load(paste0("pp_forecasts/forecast_data_step", l, ".RData"))
    lengths <- c(lengths, length(obs_vec))
}

crps_scores <- read.csv("C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/master_thesis_tail_wind/scores/crps_scores_new.csv")
crps_scores <- crps_scores %>% filter(method %in% toupper(c("ens", "emos_sea", "mbm_sea", "idr", "emos_bst", "qrf_loc", "bqn", "drn", "hen")))



library(dplyr)
library(tidyr)
library(purrr)

t_vals <- c(11, 13, 18)

# First dataframe
crps_avg_results <- crps_scores %>%
    filter(t == 0) %>%
    group_by(method) %>%
    summarise(CRPS = round(weighted.mean(twcrps, w = lengths[lead_time + 1]), 2), .groups = "drop")

# Second dataframe (twcrps)
twcrps_avg_results <- lapply(t_vals, function(t_val) {
    crps_scores %>%
        filter(t == t_val) %>%
        group_by(method) %>%
        summarise(!!paste0("t = ", t_val) := round(weighted.mean(twcrps, w = lengths[lead_time + 1]), 3), .groups = "drop")
}) %>%
    reduce(full_join, by = "method") %>%
    pivot_longer(-method, names_to = "t_val", values_to = "value") %>%
    mutate(type = "twcrps") %>%
    pivot_wider(names_from = c(type, t_val), values_from = value)

# Third dataframe (owcrps)
owcrps_avg_results <- lapply(t_vals, function(t_val) {
    crps_scores %>%
        filter(t == t_val) %>%
        group_by(method) %>%
        summarise(!!paste0("t = ", t_val) := round(weighted.mean(owcrps, w = lengths[lead_time + 1]), 3), .groups = "drop")
}) %>%
    reduce(full_join, by = "method") %>%
    pivot_longer(-method, names_to = "t_val", values_to = "value") %>%
    mutate(type = "owcrps") %>%
    pivot_wider(names_from = c(type, t_val), values_from = value)

# Combine all
combined_results <- crps_avg_results %>%
    full_join(twcrps_avg_results, by = "method") %>%
    full_join(owcrps_avg_results, by = "method")


# Write to CSV
write.csv(combined_results, "scores/crps_total.csv", row.names = FALSE)
