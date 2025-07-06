# this script plots the mean values of the parameters for each model

# %% load libraries ----

# remove all objects from the workspace
rm(list = ls())

setwd("C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/distributionalnn") # change to project directory

library(dplyr)
library(crch)
library(TailCalibration)
library(rlang)
library(ggplot2)
library(scoringRules)
library(distr)
library(ExtDist)
library(gamlss.dist)
library(evd)
library(shiny)
library(plotly)
library(gridExtra)
library(ggpubr)

source("Neural_Networks/utils.R")


#%% scatter plots mean values ----



source("Neural_Networks/utils.R")
dev.new()
for (seed in seeds) {
    for (distribution in distributions) {
        for (loss in losses) {
            for (rs in random_sampling) {
                for (new in new_data) {
                    print(paste("Distribution:", distribution, "Loss:", loss, "Random sampling:", rs, "New dataset:", new))

                    path_prefix <- "C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/distributionalnn/Datasets/forecasts/"
                    path <- paste0(tolower(distribution), "/", loss, "/", "seed_", seed, "/")

                    all_folders <- list.dirs(paste0(path_prefix, path), recursive = FALSE)
                    print(all_folders)
                    for (folder in all_folders) {
                        tryCatch(
                            {
                                data <- read.csv(paste0(folder, "/forecast_test.csv"))

                                # Prepare combined data for plotting
                                plot_data <- data.frame()
                                for (i in 1:4) {
                                    # get all columns that end with _i
                                    cols <- grep(paste0("_", i, "$"), names(data), value = TRUE)
                                    df <- data[, c("y_true", cols)]
                                    colnames(df) <- sub(paste0("_", i, "$"), "", colnames(df))
                                    df$Model <- paste0("Model ", i)
                                    plot_data <- rbind(plot_data, df)
                                }


                                # Create the four plots and store them
                                plot1 <- ggplot(data, aes(x = y_true, y = loc)) +
                                    geom_point(alpha = 0.1) +
                                    labs(
                                        title = "Price vs Location Parameter",
                                        x = "Price (EUR/MWh)",
                                        y = "Location Parameter"
                                    ) +
                                    theme_minimal() +
                                    theme(
                                        panel.background = element_rect(fill = "white"),
                                        plot.background = element_rect(fill = "white"),
                                        legend.background = element_rect(fill = "white")
                                    )

                                plot2 <- ggplot(data, aes(x = y_true, y = scale)) +
                                    geom_point(alpha = 0.1) +
                                    labs(
                                        title = "Price vs Scale Parameter",
                                        x = "Price (EUR/MWh)",
                                        y = "Scale Parameter"
                                    ) +
                                    theme_minimal() +
                                    theme(
                                        panel.background = element_rect(fill = "white"),
                                        plot.background = element_rect(fill = "white"),
                                        legend.background = element_rect(fill = "white")
                                    )

                                # Create skewness plot if the column exists
                                if (!is.null(data$skewness)) {
                                    plot3 <- ggplot(data, aes(x = y_true, y = skewness)) +
                                        geom_point(alpha = 0.1) +
                                        labs(
                                            title = "Price vs Skewness Parameter",
                                            x = "Price (EUR/MWh)",
                                            y = "Skewness Parameter"
                                        ) +
                                        theme_minimal() +
                                        theme(
                                            panel.background = element_rect(fill = "white"),
                                            plot.background = element_rect(fill = "white"),
                                            legend.background = element_rect(fill = "white")
                                        )
                                } else {
                                    plot3 <- NULL
                                }

                                # Create tailweight plot if the column exists
                                if (!is.null(data$tailweight)) {
                                    plot4 <- ggplot(data, aes(x = y_true, y = tailweight)) +
                                        geom_point(alpha = 0.1) +
                                        labs(
                                            title = "Price vs Tailweight Parameter",
                                            x = "Price (EUR/MWh)",
                                            y = "Tailweight Parameter"
                                        ) +
                                        theme_minimal() +
                                        theme(
                                            panel.background = element_rect(fill = "white"),
                                            plot.background = element_rect(fill = "white"),
                                            legend.background = element_rect(fill = "white")
                                        )
                                } else {
                                    plot4 <- NULL
                                }

                                # Create a list of available plots
                                plots <- list()
                                if (!is.null(plot1)) plots <- c(plots, list(plot1))
                                if (!is.null(plot2)) plots <- c(plots, list(plot2))
                                if (!is.null(plot3)) plots <- c(plots, list(plot3))
                                if (!is.null(plot4)) plots <- c(plots, list(plot4))

                                # Determine grid layout based on number of available plots
                                ncol <- min(4, length(plots))
                                nrow <- ceiling(length(plots) / ncol)
                                tryCatch(
                                    {
                                        combined_plot <- ggarrange(
                                            plotlist = plots,
                                            ncol = ncol, nrow = nrow,
                                            common.legend = TRUE, legend = "right"
                                        )

                                        # Save the combined plot
                                        ggsave(paste0(folder, "/params_forecast_mean.png"),
                                            combined_plot,
                                            width = 15, height = 5  ,
                                            dpi = 500
                                        )

                                    },
                                    error = function(e) {
                                        message("Error during plot generation: ", e$message, ", trying again...")
                                        combined_plot <- ggarrange(
                                            plotlist = plots,
                                            ncol = ncol, nrow = nrow,
                                            common.legend = TRUE, legend = "right"
                                        )
                                        ggsave(paste0(folder, "/params_forecast_mean.png"),
                                            combined_plot,
                                            width = 15, height = 8,
                                            dpi = 300
                                        )
                                    }
                                )

                            },
                            error = function(e) {
                                message("Error in plot generation: ", e$message)
                            }
                        )
                    }
                }
            }
        }
    }
}
