#%% load libraries ----

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
library(gridExtra)


dev.new()



#%%
source("Neural_Networks/utils.R")

path_prefix <- "C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/distributionalnn/Datasets/forecasts/"


calculate_crps <- TRUE
calculate_calibration <- FALSE


for (seed in seeds){
    for (distribution in distributions){
        for (loss in losses){
            for (rs in random_sampling){
                for (new in new_data){

                    print(paste('Distribution:', distribution, 'Loss:', loss, 'Random sampling:', rs, 'New dataset:', new, 'Side:', side, 'Seed:', seed))

                    path <- paste0(tolower(distribution), "/", loss, "/", "seed_", seed, "/")
                    all_folders <- list.dirs(paste0(path_prefix, path), recursive = FALSE)
                    print(paste("All folders:", paste(all_folders, collapse = ", ")))

                    for (folder in all_folders){
                        data <- read.csv(paste0(folder, "/forecast_test.csv"))

                        for (side in sides){
                            tryCatch({

                                filename <- paste0(folder, "/calibration_", side, ".png")
                                filename_pdf <- paste0(folder, "/calibration_", side, ".pdf")

                                if (calculate_calibration){
                                
                                    if (!file.exists(filename)){

                                        if (side == 'upper'){
                                            percentile_values <- quantile(data$y_true, upper_percentiles)
                                            percentile_names <- set_percentile_names(upper_percentiles, percentile_values, 'upper')
                                            y <- data$y_true
                                            if (distribution == 'Normal'){ dist <- pnorm; params <- list(mean = data$loc, sd = data$scale)}
                                            if (distribution == 'JSU'){ 
                                                dist <- create_qavg_cdf(
                                                    param_sets = data,
                                                    dist_type = "JSU"
                                                )
                                                params <- list()
                                            }
                                            if (distribution == 'Gumbel'){ dist <- pgumbel; params <- list(loc = data$loc, scale = data$scale)}
                                            if (distribution == 'Logistic'){ dist <- plogis; params <- list(location = data$loc, scale = data$scale)}
                                        }
                                        if (side == 'lower'){
                                            percentile_values <- quantile(-data$y_true, upper_percentiles)
                                            percentile_names <- set_percentile_names(lower_percentiles, percentile_values, 'lower')
                                            y <- -data$y_true
                                            if (distribution == 'Normal'){ dist <- pnorm; params <- list(mean = -data$loc, sd = data$scale)}
                                            if (distribution == "JSU") {
                                                param_sets <- data
                                                for (i in 1:4){
                                                    param_sets[paste0("loc_", i)] <- -param_sets[paste0("loc_", i)]
                                                    param_sets[paste0("skewness_", i)] <- -param_sets[paste0("skewness_", i)]
                                                }

                                                dist <- create_qavg_cdf(
                                                    param_sets = param_sets,
                                                    dist_type = 'JSU'
                                                )
                                                params <- list()

                                            }
                                            if (distribution == 'Gumbel'){ dist <- pgumbel_min; params <- list(loc = data$loc, scale = data$scale)}
                                            if (distribution == 'Logistic'){ dist <- plogis; params <- list(location = -data$loc, scale = data$scale)}
                                        }    
                                        
                                        com <- do.call(tc_prob, c(list(y, dist, t = c(-Inf, percentile_values)), params))
                                        sev <- do.call(tc_prob, c(list(y, dist, t = c(-Inf, percentile_values), ratio = 'sev'), params))
                                        occ <- do.call(tc_prob, c(list(y, dist, t = seq(from = round(quantile(y, probs = 0.01)), to = round(quantile(y, probs = 0.99)), by = 2), ratio = "occ"), params))

                                        com_plot <- plot_ptc(com, names = c("S", percentile_names), title = "Combined Ratio", ylim = c(0, 1.1)) + theme
                                        sev_plot <- plot_ptc(sev, names = c("S", percentile_names), title = "Severity Ratio", ratio = "sev") + theme + theme(legend.position = "none")
                                        occ_plot <- plot_ptc(occ, names = c("S", percentile_names), title = "Occurrence Ratio", ratio = "occ", ylim = c(0.7, 1.3), xlab = "t (€)") + theme

                                        if (side == "lower") {
                                            occ_plot <- occ_plot +
                                                scale_x_continuous(labels = function(x) -x, trans = "reverse") +
                                                xlab("t (€)")
                                        }
                                        
                                        plot_title <- paste("Distribution:", distribution, "| Loss:", loss, "| Seed:", seed, "| Side:", side)
                                        tryCatch({
                                            combined_plot <- grid.arrange(com_plot, sev_plot, occ_plot, ncol = 3, padding = unit(1, "line"))#, top = plot_title)
                                            #ggsave(filename = filename, plot = combined_plot, width = 18, height = 6)
                                            ggsave(filename = filename_pdf, plot = combined_plot, width = 18, height = 6)
                                        }, error = function(e) {
                                            message("Error during first try: ", e$message)
                                            combined_plot <- gridExtra::grid.arrange(com_plot, sev_plot, occ_plot, ncol = 3, padding = unit(1, "line"))#, top = plot_title)
                                            #ggsave(filename = filename, plot = combined_plot, width = 18, height = 6)
                                            ggsave(filename = filename_pdf, plot = combined_plot, width = 18, height = 6)
                                        })
                                    } else {
                                        message(paste("File already exists, skipping:", filename))
                                    }
                                }
                            }, error = function(e) {
                                message("Error in distribution: ", distribution, " with loss: ", loss, " and random sampling: ", rs, " and new data: ", new, " and side: ", side)
                                message("Error message: ", e$message)
                            })
                        }

                        if (!calculate_crps) {
                            next
                        }
                    
                        tryCatch({

                            wcrps_file <- paste0(folder, "/wcrps_values.csv")
                            if (file.exists(wcrps_file)) {
                                message(paste("File already exists, skipping:", wcrps_file))
                                next
                            }
                            set.seed(12345)
                            wcrps_values <- data.frame()
                            up_values <- quantile(data$y_true, upper_percentiles)
                            lp_values <- quantile(data$y_true, lower_percentiles)

                            if (distribution == "Normal") {s <- t(sapply(1:length(data$y_true), function(j) rnorm(1000, mean = data$loc[j], sd = data$scale[j])))}
                            if (distribution == "JSU") {s <- sample_qavg_distribution(1000, param_sets = data, qfunc = NULL, dist_type = "JSU")}
                            if (distribution == "Logistic") {s <- t(sapply(1:length(data$y_true), function(j) rlogis(1000, location = data$loc[j], scale = data$scale[j])))}

                            for (pair in Map(list, c(0, upper_percentiles), c(-Inf, up_values))) {
                                i <- pair[[1]]
                                j <- pair[[2]]
                                mean_twcrps_up <- mean(twcrps_sample(data$y_true, dat = s, a = j), na.rm = TRUE)
                                mean_owcrps_up <- mean(owcrps_sample(data$y_true, dat = s, a = j), na.rm = TRUE)
                                print(paste("Upper", i, j, mean_twcrps_up, mean_owcrps_up))
                                wcrps_values <- rbind(wcrps_values, data.frame(path = path, type = "upper", twcrps = mean_twcrps_up, owcrps = mean_owcrps_up, percentile = i, percentile_value = j), stringsAsFactors = FALSE)
                            }
                            for (pair in Map(list, c(1, lower_percentiles), c(Inf, lp_values))) {
                                i <- pair[[1]]
                                j <- pair[[2]]
                                mean_twcrps_lp <- mean(twcrps_sample(data$y_true, dat = s, b = j), na.rm = TRUE)
                                mean_owcrps_lp <- mean(owcrps_sample(data$y_true, dat = s, b = j), na.rm = TRUE)
                                print(paste("Lower", i, j, mean_twcrps_lp, mean_owcrps_lp))
                                wcrps_values <- rbind(wcrps_values, data.frame(path = path, type = "lower", twcrps = mean_twcrps_lp, owcrps = mean_owcrps_lp, percentile = i, percentile_value = j), stringsAsFactors = FALSE)
                            }
                            write.csv(wcrps_values, wcrps_file, row.names = FALSE)
                            
                        }, error = function(e) {
                            message("Error in distribution: ", distribution, " with loss: ", loss, " and random sampling: ", rs, " and new data: ", new, " and side: ", side)
                            message("Error message: ", e$message)
                        })
                    }   
                }
            }
        }
    }
}
                


