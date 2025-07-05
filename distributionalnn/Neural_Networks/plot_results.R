# %% load libraries ----

# remove all objects from the workspace
rm(list = ls())

setwd("C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/distributionalnn")

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



#%% Shiny app ----
# UI for Shiny app
# ui <- fluidPage(
#     titlePanel("Distribution CDF Visualization"),
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("obs_index", "Observation Index:", 
#                                     min = 1, max = length(y), value = 1, step = 1),
#             sliderInput("y_value", "Y Value:",
#                                     min = min(y) - 1, max = max(y) + 1, value = y[1], step = 0.1),
#             verbatimTextOutput("param_info")
#         ),
#         mainPanel(
#             plotOutput("cdfPlot")
#         )
#     )
# )

# # Server logic
# server <- function(input, output, session) {
    
#     # Update y_value slider when obs_index changes
#     observeEvent(input$obs_index, {
#         updateSliderInput(session, "y_value", value = y[input$obs_index])
#     })
    
#     # Show parameter information
#     output$param_info <- renderPrint({
#         param_set <- param_sets[[input$obs_index]]
#         cat("Parameter Set Information:\n")
#         for(i in 1:nrow(param_set)) {
#             cat(sprintf("Model %d: loc=%.3f, scale=%.3f, tailweight=%.3f, skewness=%.3f\n", 
#                                  i, param_set[i,1], param_set[i,2], param_set[i,3], param_set[i,4]))
#         }
#     })
    
#     # Plot the CDF with vertical line
#     output$cdfPlot <- renderPlot({
#         param_set <- param_sets[input$obs_index,]
        
#         # Calculate range for x-axis (span around the current y value)
#         range_min <- min(y[input$obs_index] - 40, input$y_value - 40)
#         range_max <- max(y[input$obs_index] + 40, input$y_value + 40)
#         # range_min <- -140
#         # range_max <- 40
#         x_values <- seq(range_min, range_max, length.out = 100)
        
#         # Calculate CDF values
#         cdf_values <- sapply(x_values, function(x) dist(param_sets = param_set, dist_type = params$dist_type)(x))

#         # Calculate PDF values using numerical differentiation
#         dx <- diff(x_values)
#         cdf_vals <- cdf_values
#         pdf_values <- diff(cdf_vals) / dx
#         x_mid <- (x_values[-1] + x_values[-length(x_values)]) / 2  # Midpoints for plotting

#         # For use in the plot later, we'll store these
#         plotting_x <- x_mid
#         plotting_y <- pdf_values

        
#         # Find CDF at the selected y value
#         #cdf_at_y <- dist(param_sets = param_set, dist_type = params$dist_type)(input$y_value)
        
#         # Create the plot
#         # plot(x_values, cdf_values, type = "l", lwd = 2, col = "blue",
#         #      xlab = "Value", ylab = "Cumulative Probability",
#         #      main = sprintf("CDF Plot (Observation %d)", input$obs_index),
#         #      xlim = c(-140, 40), ylim = c(0, 1))
#         plot(x_mid, pdf_values, type = "l", lwd = 2, col = "blue",
#              xlab = "Value", ylab = "Cumulative Probability",
#              main = sprintf("CDF Plot (Observation %d)", input$obs_index),
#              xlim = c(range_min, range_max))
        
#         # Add vertical line at selected y value
#         abline(v = input$y_value, col = "red", lwd = 2, lty = 2)
        
#         # Add horizontal line from y-axis to CDF curve at the selected y value
#         #segments(range_min, cdf_at_y, input$y_value, cdf_at_y, col = "red", lty = 2)
        
#         # Add text annotation for the CDF value
#         # text(input$y_value + 0.1, cdf_at_y, 
#         #          sprintf("CDF = %.3f", cdf_at_y), 
#         #          pos = 4, col = "red")
        
#         # Add a point at the intersection
#         #points(input$y_value, cdf_at_y, col = "red", pch = 19, cex = 1.5)
#     })
# }

# # Run the Shiny app
# shinyApp(ui, server)


#%% Scatter plots ----


# source("Neural_Networks/utils.R")
# dev.new()
# for (seed in seeds){
#     for (distribution in distributions){
#         for (loss in losses){
#             for (rs in random_sampling){
#                 for (new in new_data){
                

                    

#                     print(paste("Distribution:", distribution, "Loss:", loss, "Random sampling:", rs, "New dataset:", new))

#                     # path <- paste0(tolower(distribution), ifelse(new, "_new", ""), ifelse(rs, "_rs", ""), "_", loss)
#                     # data <- read.csv(paste0("C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/distributionalnn/Datasets/forecasts/", path, "/forecast_test.csv"))
#                     path_prefix <- "C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/distributionalnn/Datasets/forecasts/"
#                     path <- paste0(tolower(distribution), "/", loss, "/", "seed_", seed, "/")


#                     all_folders <- list.dirs(paste0(path_prefix, path), recursive = FALSE)

#                     for (folder in all_folders){
#                         tryCatch({
#                             data <- read.csv(paste0(folder, "/forecast_test.csv"))

#                             # Prepare combined data for plotting
#                             plot_data <- data.frame()
#                             for (i in 1:4) {
#                                 # get all columns that end with _i
#                                 cols <- grep(paste0("_", i, "$"), names(data), value = TRUE)
#                                 df <- data[, c('y_true', cols)]
#                                 colnames(df) <- sub(paste0("_", i, "$"), "", colnames(df))
#                                 df$Model <- paste0("Model ", i)
#                                 plot_data <- rbind(plot_data, df)
#                             }


#                             # Create the four plots and store them
#                             plot1 <- ggplot(plot_data, aes(x = y_true, y = loc, color = Model)) +
#                                 geom_point(alpha = 0.1) +
#                                 labs(
#                                     title = "Price vs Location Parameter",
#                                     x = "Price (EUR/MWh)",
#                                     y = "Location Parameter"
#                                 ) +
#                                 theme_minimal() +
#                                 theme(
#                                     panel.background = element_rect(fill = "white"),
#                                     plot.background = element_rect(fill = "white"),
#                                     legend.background = element_rect(fill = "white")
#                                 )

#                             plot2 <- ggplot(plot_data, aes(x = y_true, y = scale, color = Model)) +
#                                 geom_point(alpha = 0.1) +
#                                 labs(
#                                     title = "Price vs Scale Parameter",
#                                     x = "Price (EUR/MWh)",
#                                     y = "Scale Parameter"
#                                 ) +
#                                 theme_minimal() +
#                                 theme(
#                                     panel.background = element_rect(fill = "white"),
#                                     plot.background = element_rect(fill = "white"),
#                                     legend.background = element_rect(fill = "white")
#                                 )

#                             # Create skewness plot if the column exists
#                             if(!is.null(plot_data$skewness)) {
#                                 plot3 <- ggplot(plot_data, aes(x = y_true, y = skewness, color = Model)) +
#                                     geom_point(alpha = 0.1) +
#                                     labs(
#                                         title = "Price vs Skewness Parameter",
#                                         x = "Price (EUR/MWh)",
#                                         y = "Skewness Parameter"
#                                     ) +
#                                     theme_minimal() +
#                                     theme(
#                                         panel.background = element_rect(fill = "white"),
#                                         plot.background = element_rect(fill = "white"),
#                                         legend.background = element_rect(fill = "white")
#                                     )
#                             } else {
#                                 plot3 <- NULL
#                             }

#                             # Create tailweight plot if the column exists
#                             if(!is.null(plot_data$tailweight)) {
#                                 plot4 <- ggplot(plot_data, aes(x = y_true, y = tailweight, color = Model)) +
#                                     geom_point(alpha = 0.1) +
#                                     labs(
#                                         title = "Price vs Tailweight Parameter",
#                                         x = "Price (EUR/MWh)",
#                                         y = "Tailweight Parameter"
#                                     ) +
#                                     theme_minimal() + 
#                                     theme(
#                                         panel.background = element_rect(fill = "white"),
#                                         plot.background = element_rect(fill = "white"),
#                                         legend.background = element_rect(fill = "white")
#                                     )
#                             } else {
#                                 plot4 <- NULL
#                             }

                            
#                             # If ggpubr is not available, use gridExtra with separate legends
#                             # Create a list of available plots
#                             plots <- list()
#                             if(!is.null(plot1)) plots <- c(plots, list(plot1))
#                             if(!is.null(plot2)) plots <- c(plots, list(plot2))
#                             if(!is.null(plot3)) plots <- c(plots, list(plot3))
#                             if(!is.null(plot4)) plots <- c(plots, list(plot4))
                            
#                             # Determine grid layout based on number of available plots
#                             ncol <- min(4, length(plots))
#                             nrow <- ceiling(length(plots) / ncol)
#                             tryCatch({
#                                 combined_plot <- ggarrange(
#                                     plotlist = plots,
#                                     ncol = ncol, nrow = nrow,
#                                     common.legend = TRUE, legend = "right"
#                                 )

#                                 #combined_plot <- do.call(grid.arrange, c(plots, ncol = ncol, nrow = nrow))
#                                 # Save the combined plot
#                                 ggsave(paste0(folder, "/params_forecast.png"), 
#                                    combined_plot, width = 12, height = 10)
#                                 # ggsave(paste0(folder, "/params_forecast.pdf"), 
#                                 #     combined_plot, width = 12, height = 10, dpi = 150)
#                             }, error = function(e) {
#                                 message("Error during plot generation: ", e$message, ", trying again...")
#                                                                 combined_plot <- ggarrange(
#                                                                     plotlist = plots,
#                                                                     ncol = ncol, nrow = nrow,
#                                                                     common.legend = TRUE, legend = "right"
#                                                                 )
#                                 ggsave(paste0(folder, "/params_forecast.png"),
#                                     combined_plot, width = 12, height = 10
#                                 )
#                             })



#                             # ggplot(plot_data, aes(x = skewness, y = tailweight, color = Model)) +
#                             #     geom_point(alpha = 0.1) +
#                             #     labs(
#                             #         title = "Scatter Plot of Scale vs Skewness Parameter",
#                             #         x = "Scale Parameter",
#                             #         y = "Skewness Parameter",
#                             #         color = "Model"
#                             #     ) +
#                             #     theme_minimal() +
#                             #     theme(
#                             #         panel.background = element_rect(fill = "white"),
#                             #         plot.background = element_rect(fill = "white"),
#                             #         legend.background = element_rect(fill = "white")
#                             #     )


#                         }, error = function(e) {
#                             message("Error in plot generation: ", e$message)
#                         })
#                     }
#                 }
#             }
#         }
#     }
# }


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
