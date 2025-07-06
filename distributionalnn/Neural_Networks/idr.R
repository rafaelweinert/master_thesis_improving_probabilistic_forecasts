#%%
rm(list = ls())
library(dplyr)
library(TailCalibration)
library(scoringRules)
library(ggplot2)
library(gridExtra)
library(scoringRules)

setwd("C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/distributionalnn") # change to project directory
source("Neural_Networks/utils.R")

# %% tail calibration

F <- function(x, x_vals, cdf_vals) {
    n <- nrow(x_vals)

    if (length(x) == 1) {
        # Broadcast single x to all rows
        x_rep <- rep(x, n)
    } else if (length(x) == n) {
        x_rep <- x
    } else {
        stop("Length of x must be 1 or equal to the number of rows in x_vals.")
    }

    sapply(1:n, function(i) {
        approx(x = x_vals[i, ], y = cdf_vals[i, ], xout = x_rep[i], method = "linear", rule = 2, yleft = 0, yright = 1)$y
    })
}

loss <- 'MAE' # change to 'LOG_PROB' and 'Normal' or 'JSU' for the other distribution
distribution <- 'Point'
seed <- 10

folder <- paste0("Datasets/forecasts/", tolower(distribution), "_idr/", loss, "/seed_", seed, "/paper")

for (side in sides){
    tryCatch({
        df <- read.csv(paste0(folder, "/forecast_test.csv"))
        filename <- paste0(folder, "/calibration_", side, ".png")
        filename_pdf <- paste0(folder, "/calibration_", side, ".pdf")

        points <- df %>% 
            filter(type == 'points')
        ecdf <- df %>% 
            filter(type == 'ecdf')

        y <- points[["y_true"]]
        y <- as.vector(y)

        # Select all columns where the name is a number
        numeric_cols <- grep("^X\\d+", names(points), value = TRUE)
        points_matrix <- as.matrix(points[, numeric_cols])
        ecdf_matrix <- as.matrix(ecdf[, numeric_cols])

        if (side == 'upper'){
            percentile_values <- quantile(y, upper_percentiles)
            percentile_names <- set_percentile_names(upper_percentiles, percentile_values, "upper")
        }
        if (side == 'lower'){
            percentile_values <- quantile(-y, upper_percentiles)
            percentile_names <- set_percentile_names(lower_percentiles, percentile_values, "lower")

            points_matrix <- -points_matrix
            ecdf_matrix <- 1 - ecdf_matrix
            y <- -y
        }

        com <- tc_prob(y, F, t = c(-Inf, percentile_values), x_vals = points_matrix, cdf_vals = ecdf_matrix)
        sev <- tc_prob(y, F, t = c(-Inf, percentile_values), x_vals = points_matrix, cdf_vals = ecdf_matrix, ratio = 'sev')
        occ <- tc_prob(y, F, t = seq(from = round(quantile(y, probs = 0.01)), to = round(quantile(y, probs = 0.99)), by = 2), ratio = "occ", x_vals = points_matrix, cdf_vals = ecdf_matrix)

        com_plot <- plot_ptc(com, names = c("S", percentile_names), title = "Combined Ratio", ylim = c(0, 1.1)) + theme
        sev_plot <- plot_ptc(sev, names = c("S", percentile_names), title = "Severity Ratio", ratio = "sev") + theme + theme(legend.position = "none")
        occ_plot <- plot_ptc(occ, names = c("S", percentile_names), title = "Occurrence Ratio", ratio = "occ", ylim = c(0.7, 1.3), xlab = "t (€)") + theme


        if (side == "lower") {
            occ_plot <- occ_plot +
                scale_x_continuous(labels = function(x) -x, trans = "reverse") +
                xlab("t (€)")
        }
        tryCatch(
            {
                combined_plot <- grid.arrange(com_plot, sev_plot, occ_plot, ncol = 3, padding = unit(1, 'line'))
                #ggsave(filename = filename, plot = combined_plot, width = 18, height = 6)
                ggsave(filename = filename_pdf, plot = combined_plot, width = 18, height = 6)
            },
            error = function(e) {
                message("Error during first try: ", e$message)
                combined_plot <- gridExtra::grid.arrange(com_plot, sev_plot, occ_plot, ncol = 3, padding = unit(1, "line")) 
                #ggsave(filename = filename, plot = combined_plot, width = 18, height = 6)
                ggsave(filename = filename_pdf, plot = combined_plot, width = 18, height = 6)
            }
        )
    }, error = function(e) {
        message("Error during file processing: ", e$message)
    })
}





#%% wcrps values


sample_from_cdf <- function(n_samples, x_vals, cdf_vals) {
    set.seed(123)
    n <- nrow(x_vals)
    u <- matrix(runif(n * n_samples), nrow = n, ncol = n_samples)

    samples <- matrix(NA, nrow = n, ncol = n_samples)

    for (i in 1:n) {
        samples[i, ] <- approx(
            x = cdf_vals[i, ],
            y = x_vals[i, ],
            xout = u[i, ],
            method = "linear",
            rule = 2,
            yleft = min(x_vals[i, ], na.rm = TRUE),
            yright = max(x_vals[i, ], na.rm = TRUE)
        )$y
    }

    samples
}


df <- read.csv(paste0(folder, "/forecast_test.csv"))

points <- df %>%
    filter(type == "points")
ecdf <- df %>%
    filter(type == "ecdf")

y <- points[["y_true"]]
y <- as.vector(y)
numeric_cols <- grep("^X\\d+", names(points), value = TRUE)
points_matrix <- as.matrix(points[, numeric_cols])
ecdf_matrix <- as.matrix(ecdf[, numeric_cols])

samples <- sample_from_cdf(1000, points_matrix, ecdf_matrix)

up_values <- quantile(y, upper_percentiles)
lp_values <- quantile(y, lower_percentiles)

wcrps_values <- data.frame()

for (pair in Map(list, c(0, upper_percentiles), c(-Inf, up_values))) {
    i <- pair[[1]]
    j <- pair[[2]]
    mean_twcrps_up <- mean(twcrps_sample(y, dat = samples, a = j), na.rm = TRUE)
    mean_owcrps_up <- mean(owcrps_sample(y, dat = samples, a = j), na.rm = TRUE)
    print(paste("Upper", i, j, mean_twcrps_up, mean_owcrps_up))
    wcrps_values <- rbind(wcrps_values, data.frame(type = "upper", twcrps = mean_twcrps_up, owcrps = mean_owcrps_up, percentile = i, percentile_value = j), stringsAsFactors = FALSE)
}
for (pair in Map(list, c(1, lower_percentiles), c(Inf, lp_values))) {
    i <- pair[[1]]
    j <- pair[[2]]
    mean_twcrps_lp <- mean(twcrps_sample(y, dat = samples, b = j), na.rm = TRUE)
    mean_owcrps_lp <- mean(owcrps_sample(y, dat = samples, b = j), na.rm = TRUE)
    print(paste("Lower", i, j, mean_twcrps_lp, mean_owcrps_lp))
    wcrps_values <- rbind(wcrps_values, data.frame(type = "lower", twcrps = mean_twcrps_lp, owcrps = mean_owcrps_lp, percentile = i, percentile_value = j), stringsAsFactors = FALSE)
}

wcrps_file <- paste0(folder, "/wcrps_values.csv")
write.csv(wcrps_values, wcrps_file, row.names = FALSE)
 



#%% plot median vs y


Q <- function(x, x_vals, cdf_vals) {
    n <- nrow(x_vals)

    if (length(x) == 1) {
        # Broadcast single x to all rows
        x_rep <- rep(x, n)
    } else if (length(x) == n) {
        x_rep <- x
    } else {
        stop("Length of x must be 1 or equal to the number of rows in x_vals.")
    }

    sapply(1:n, function(i) {
        approx(x = cdf_vals[i, ], y = x_vals[i, ], xout = x_rep[i], method = "linear", rule = 2)$y
    })
}


median <- Q(0.5, points_matrix, ecdf_matrix)
plot(y, median, xlab = "y", ylab = "median", main = "Median vs y", ylim = c(-70, 150))

