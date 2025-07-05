#%% load libraries ----

# remove all objects from the workspace
rm(list = ls())

library(dplyr)
library(crch)
library(TailCalibration)
library(rlang)
library(ggplot2)
library(scoringRules)
library(distr)
library(ExtDist)
library(SuppDists)

#%%


# ---------- outdated!!!! use nn_tail_calibration.R instead ----

source("Neural_Networks/utils.R")




#wcrps_values <- data.frame(path = character(), type = character(), twcrps = numeric(), owcrps = numeric(), percentile = numeric(), percentile_value = numeric(), stringsAsFactors = FALSE)
for (seed in seeds){
for (distribution in distributions){
    for (loss in losses){
        for (rs in random_sampling){
            for (new in new_data){
                print(paste(distribution, loss, rs, new))

                # path <- paste0(tolower(distribution), ifelse(new, "_new", ""), ifelse(rs, "_rs", ""), "_", loss)

                path <- paste0("C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/distributionalnn/Datasets/forecasts/", tolower(distribution), "/", loss, "/", "seed_", seed, "/")
                all_folders <- list.dirs(path, recursive = FALSE)

                #all_folders <- list.dirs("C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/distributionalnn/Datasets/forecasts/", recursive = FALSE)
                #all_folders <- all_folders[grepl(path, all_folders)]
                for (folder in all_folders){

                    # Check if wcrps_values.csv already exists in the folder
                    wcrps_file <- paste0(folder, "/wcrps_values.csv")
                    if (file.exists(wcrps_file)) {
                        message(paste("File already exists, skipping:", wcrps_file))
                        next
                    }

                    data <- read.csv(paste0(folder, "/forecast_test.csv"))        

                    #data <- read.csv(paste0("C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/distributionalnn/Datasets/forecasts/", path, ".csv"))
                    up_values <- quantile(data$y_true, upper_percentiles)
                    lp_values <- quantile(data$y_true, lower_percentiles)

                    if (distribution == 'Normal'){ s <- t(sapply(1:length(data$y_true), function(j) rnorm(1000, mean = data$loc[j], sd = data$scale[j])))}
                    if (distribution == 'JSU') { s <- sample_qavg_distribution(1000, param_sets = data, qfunc = NULL, dist_type = "JSU")}
                    if (distribution == 'GEV'){ s <- t(sapply(1:length(data$y_true), function(j) rgev(1000, loc = data$loc[j], scale = data$scale[j], shape = data$concentration[j])))}
                    wcrps_values <- data.frame()

                    for (pair in Map(list, c(0, upper_percentiles), c(-Inf, up_values))) {
                        i <- pair[[1]]
                        j <- pair[[2]]
                        mean_twcrps_up <- mean(twcrps_sample(data$y_true, dat = s, a = j), na.rm = TRUE)
                        mean_owcrps_up <- mean(owcrps_sample(data$y_true, dat = s, a = j), na.rm = TRUE)
                        print(paste("Upper", i, j, mean_twcrps_up, mean_owcrps_up))
                        wcrps_values <- rbind(wcrps_values, data.frame(path = path, type = 'upper', twcrps = mean_twcrps_up, owcrps = mean_owcrps_up, percentile = i, percentile_value = j), stringsAsFactors = FALSE)
                    }
                    for (pair in Map(list, c(1, lower_percentiles), c(Inf, lp_values))) {
                        i <- pair[[1]]
                        j <- pair[[2]]
                        mean_twcrps_lp <- mean(twcrps_sample(data$y_true, dat = s, b = j), na.rm = TRUE)
                        mean_owcrps_lp <- mean(owcrps_sample(data$y_true, dat = s, b = j), na.rm = TRUE)
                        print(paste("Lower", i,j, mean_twcrps_lp, mean_owcrps_lp))
                        wcrps_values <- rbind(wcrps_values, data.frame(path = path, type='lower', twcrps = mean_twcrps_lp, owcrps=mean_owcrps_lp, percentile=i, percentile_value=j), stringsAsFactors = FALSE)
                    }
                    # Round all values in the samples matrix to two decimal places
                    #s <- round(s, digits = 1)

                    #write.csv(s, paste0(folder, "/samples.csv"), row.names = FALSE)
                    write.csv(wcrps_values, wcrps_file, row.names = FALSE)
                }
            }
        }
    } 
}
}


# write.csv(wcrps_values, "C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/distributionalnn/Datasets/evaluation/wcrps_values_log_prob_reg_output.csv", row.names = FALSE)


























