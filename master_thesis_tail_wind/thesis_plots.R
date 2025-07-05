#%% load libraries ----

# remove all objects from the workspace
rm(list = ls())

# read the score_data RData file
library(dplyr)
library(crch)
library(TailCalibration)
library(rlang)
library(ggplot2)
library(scoringRules)
library(distr)
library(grid)
library(gridExtra)

setwd(r"(C:\Users\rafaelweinert\PycharmProjects\Master_Thesis_Code\masgetwd()ter_thesis_tail_wind)")






#%%


obs_vec_complete <- data.frame(obs_vec = numeric(), lead_time = numeric())
fc_data_complete <- list()

for (l in 0:21) {
    rm(fc_data, obs_vec, pit_rank)

    load(paste0("pp_forecasts/forecast_data_step", l, ".RData"))
    obs_vec_complete <- rbind(obs_vec_complete, data.frame(obs_vec = obs_vec, lead_time = l))
    for (method in names(fc_data)) {
        if (method == "ens") {
            fc_data_complete$ens <- rbind(fc_data_complete$ens, fc_data$ens)
        } else if (method == "hen") {
            fc_data_complete$hen <- list(
                bin_edges = rbind(fc_data_complete$hen$bin_edges, fc_data$hen$bin_edges),
                f = rbind(fc_data_complete$hen$f, fc_data$hen$f)
            )
        } else {
            fc_data_complete[[method]] <- rbind(fc_data_complete[[method]], fc_data[[method]])
        }
    }
}



#%%
library(ggplot2)

# Define percentiles and their values
percentiles <- c(90, 95, 99)
percentile_values <- quantile(obs_vec_complete$obs_vec, probs = percentiles / 100)

colors <- c("#d62728", "#ff7f0e", "#2ca02c")

ordinal_suffix <- function(n) {
    if (n %% 100 %in% 11:13) {
        return("th")
    } else if (n %% 10 == 1) {
        return("st")
    } else if (n %% 10 == 2) {
        return("nd")
    } else if (n %% 10 == 3) {
        return("rd")
    } else {
        return("th")
    }
}

labels <- paste0(
    percentiles,
    sapply(percentiles, ordinal_suffix),
    " percentile: ",
    sprintf("%.2f", percentile_values),
    " m/s"
)

percentile_df <- data.frame(
    percentile = factor(as.character(percentiles), levels = as.character(percentiles)),
    value = as.numeric(percentile_values),
    color = colors,
    label = labels
)

p <- ggplot(obs_vec_complete, aes(x = obs_vec)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", color = "white") +
    labs(
        title = "Distribution of Wind Gust Speeds",
        x = "Wind Gusts (m/s)",
        y = "Relative Frequency"
    ) +
    theme_minimal(base_size = 16) +
    theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)),
        legend.position = "top",
        legend.text = element_text(size = 12)
    ) +
    geom_vline(
        data = percentile_df,
        aes(xintercept = value, color = label),
        linetype = "dashed",
        size = 1.2,
        show.legend = TRUE
    ) +
    scale_color_manual(
        values = setNames(percentile_df$color, percentile_df$label),
        name = NULL,
        guide = guide_legend(override.aes = list(linetype = "dashed", size = 1.2))
    )

p
ggsave("plots/thesis/wind_gusts_histogram.pdf", plot = p, width = 10, height = 6, dpi = 300)
ggsave("plots/thesis/wind_gusts_histogram.png", plot = p, width = 10, height = 6, dpi = 300)
