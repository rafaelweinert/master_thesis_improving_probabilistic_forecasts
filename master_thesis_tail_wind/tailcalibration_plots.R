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

setwd(r"(C:\Users\rafaelweinert\PycharmProjects\Master_Thesis_Code\master_thesis_tail_wind)")

#%% postprocessed ensemble forecast ----
theme <- theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(hjust = 0.5, size = 15),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
)

plot_postproc <- function(method, obs_vec, fc_data, lead_time, tvals = c(-Inf, 10, 13, 18), crps_weights = NULL, type = 'all', plot_dir = "plots/calibration_plots", percentiles, percentile_values) {

    #' Evaluate and plot forecast calibration metrics for different methods
    #'
    #' This function calculates and plots various tail calibration diagnostics for different
    #' forecasting methods. It computes combined, severity, and occurrence ratios for the
    #' specified thresholds as well as threshold-weighted and outcome-weighted CRPS.
    #'
    #' @param method Character string specifying the forecast method to evaluate (e.g. "ENS", "EMOS", "HEN", etc.)
    #' @param obs_vec Numeric vector of observations
    #' @param fc_data List containing the forecast data for different methods
    #' @param lead_time Numeric value indicating the forecast lead time
    #' @param tvals Numeric vector of threshold values for tail calibration (default: c(-Inf, 10, 13, 18))
    #' @param crps_weights Optional weights for CRPS calculation
    #' @param type Character string specifying whether to plot the calibration, return the (weighted) CRPS values or return the absolute distance of the calibration plots from the diagonal (default: 'all', can be 'all', 'plot', 'crps' or 'abs_dist')
    #'
    #' @return A list containing calibration metrics:
    #' Either:
    #'   \item{com_dist}{Maximum absolute distance between expected and observed probabilities (combined ratio)}
    #'   \item{sev_dist}{Maximum absolute distance between expected and observed probabilities (severity ratio)}
    #' Or:
    #'   \item{twc}{Threshold-weighted CRPS values for each threshold}
    #'   \item{owc}{Outcome-weighted CRPS values for each threshold}
    #'
    #' @details
    #' The function supports various forecast types including ensemble forecasts, parametric
    #' distributions (truncated logistic), and histogram ensemble methods. It calculates tail
    #' calibration metrics and visualization plots to assess forecast performance.
        
    # Define helper function to extract ensemble matrix
    get_ens_matrix <- function(data, key) {
    return(as.matrix(data[[key]]))
    }

    # Define probability function
    get_prob <- function(method, obs_vec, F_x, ens_matrix, tvals, ratio = NULL) {
        if (method == "HEN") {
            return(tc_prob(obs_vec, F_x, t = tvals, ratio = ratio, lower = 0, bins = bins, probs = probs))
        } else if (!is.null(F_x)) {
            return(tc_prob(obs_vec, F_x, t = tvals, ratio = ratio, lower = 0, location = mu, scale = sig, left = 0))
        } else {
            return(tc_prob(obs_vec, ens_matrix, t = tvals, ratio = ratio, lower = 0))
        }
    }


    #### HEN: Evaluation measures for a histogram distribution #### -> copied from fn_eval.R
    # CRPS of a histogram distribution
    crps_hd <- function(y, f, bin_edges){
        ###-----------------------------------------------------------------------------
        ###Input
        #y...........Observations (n vector)
        #f...........Probabilities of corresponding bins (n x n_bins matrix)
        #............or: n list of (different) n_bins vectors
        #bin_edges...Boundaries of bins ((n_bins + 1) vector)
        #............or: n x (n_bins + 1) matrix
        #............or: n list of (different) (n_bins + 1) vectors
        ###-----------------------------------------------------------------------------
        ###Output
        #res...CRPS of n forecasts (n vector)
        ###-----------------------------------------------------------------------------
        
        #### Initiation ####
        # CRPS for a single forecast-observation pair
        fn_single <- function(obs, probs, bin_edges0){
            ## obs.........Wind gust observation (scalar)
            ## probs.......Bin probabilities (n_bins vector)
            ## bin_edges...Boundaries of bins ((n_bins + 1) vector)
            
            # Lower and upper edges of bins
            a <- bin_edges0[-length(bin_edges0)]
            b <- bin_edges0[-1]
            
            # Realizing values for individual uniform distributions
            z <- ((a <= obs) & (obs <= b))*obs + (obs < a)*a + (b < obs)*b
            if(obs < a[1]){ z[1] <- obs }
            if(obs > b[length(b)]){ z[length(b)] <- obs }
            
            # Lower and upper masses of individual uniform distributions
            L <- cumsum(c(0, probs[1:(length(probs)-1)]))
            U <- 1 - cumsum(probs)
            
            # Transform from bin to unit interval ([a_i, b_i] -> [0, 1])
            w <- (z - a)/(b - a)
            
            # Sum of standard uniform with lower mass L and upper mass U
            out <- sum((b - a)*( abs(w - punif(w)) 
                                + (1 - L - U)*punif(w)^2 
                                - punif(w)*(1 - 2*L) 
                                + ((1 - L - U)^2)/3 
                                + (1- L)*U ))
            
            # Output
            return(out)
        }
    
        # Function for apply (identical bin_edges?)
        if(is.list(bin_edges)){
            fn_apply <- function(i){ fn_single(obs = y[i],
                                            probs = f[[i]],
                                            bin_edges0 = bin_edges[[i]])} }
        else if(is.vector(bin_edges)){
            fn_apply <- function(i){ fn_single(obs = y[i],
                                            probs = f[i,],
                                            bin_edges0 = bin_edges)} }
        else if(is.matrix(bin_edges)){
            fn_apply <- function(i){ fn_single(obs = y[i],
                                            probs = f[i,],
                                            bin_edges0 = bin_edges[i,])} }
        
        #### Calculation ####
        # Apply function on all values
        res <- sapply(1:length(y), fn_apply)
        
        # Return
        return(res)
    }

    # Define function for crps
    get_crps <- function(method, obs_vec, F_x, ens_matrix, probs, bins, mu, sig, a = -Inf, type = "twcrps") {
        if (method == "HEN") {
            if (a != -Inf){
                if (type == "twcrps") {
                    res <- mapply(function(y, d, p, a) twcrps_sample(y, dat = d[-1], a = a, w = p), obs_vec, bins, probs, a)
                    return(mean(res, na.rm = TRUE))
                } else if(type == "owcrps") {
                    res <- mapply(function(y, d, p, a) owcrps_sample(y, dat = d[-1], a = a, w = p), obs_vec, bins, probs, a)
                    return(mean(res, na.rm = TRUE))
                }
            }
            else {
                return(mean(crps_hd(obs_vec, probs, bins)))
            }   
        }
        # all methods that return a distribution -> truncated logistic (EMOS, DRN)
        else if (!is.null(F_x)) {
            if (a != -Inf){
                # draw sample of truncated logistic distribution
                s <- t(sapply(1:length(mu), function(i) rtlogis(1000, location = mu[i], scale = sig[i], left = 0)))
                if (type == "twcrps"){
                    return (mean(twcrps_sample(obs_vec, dat = s, a = a)))
                }else if (type == "owcrps"){
                    return (mean(owcrps_sample(obs_vec, dat = s, a = a), na.rm = TRUE))
                }
            }
            else{
                return(mean(crps_tlogis(obs_vec, location = mu, scale = sig, lower = 0)))
            }
        # all methods that return an ensemble matrix -> ensemble forecast (MBM, IDR, QRF, BQN)
        } else {
            if (type == "twcrps") {
                return(mean(twcrps_sample(obs_vec, dat = ens_matrix, a = a), na.rm = TRUE))
            } else if (type == "owcrps") {
                return(mean(owcrps_sample(obs_vec, dat = ens_matrix, a = a), na.rm = TRUE))
            }
        }
    }


    get_F_HEN <- function(q, bins, probs) {
        
        # Function to evaluate F(q) for a single set of q, bins, and probs
        compute_F <- function(q_val, bin_vals, prob_vals) {
            if ((bin_vals[1] == 0) || (length(bin_vals) != length(prob_vals))) {
                bin_vals <- bin_vals[-1]
            }
            if (length(bin_vals) != length(prob_vals)) {
                print(bin_vals)
                print(prob_vals)
                stop("Each bins and probs entry must have the same length")
            }
            
            # Compute cumulative probabilities
            probs_cumsum <- cumsum(prob_vals)

            if (q_val <= bin_vals[1]) {
                return(0)
            }
            if (q_val > bin_vals[length(bin_vals)]) {
                return(1)
            }
            
            for (i in seq_along(bin_vals)[-length(bin_vals)]) {
                if (q_val > bin_vals[i] && q_val <= bin_vals[i + 1]) {
                    return(probs_cumsum[i] + (probs_cumsum[i + 1] - probs_cumsum[i]) * 
                        (q_val - bin_vals[i]) / (bin_vals[i + 1] - bin_vals[i]))
                }
            }
        }

        # Apply the function to each set of (q, bins, probs)
        return(mapply(compute_F, q, bins, probs, SIMPLIFY = TRUE))
    }

    tryCatch({
        # Compute distributions or ensemble matrix based on 'method'
        method_map <- list(
        "ENS" = list(F_x = NULL, ens_matrix = get_ens_matrix(fc_data, "ens")),
        "EMOS" = list(F_x = ptlogis, mu = fc_data$emos[,1], sig = fc_data$emos[,2]),
        "EMOS_SEA" = list(F_x = ptlogis, mu = fc_data$emos_sea[,1], sig = fc_data$emos_sea[,2]),
        "EMOS_BST" = list(F_x = ptlogis, mu = fc_data$emos_bst[,1], sig = fc_data$emos_bst[,2]),
        "MBM" = list(F_x = NULL, ens_matrix = get_ens_matrix(fc_data, "mbm")),
        "MBM_SEA" = list(F_x = NULL, ens_matrix = get_ens_matrix(fc_data, "mbm_sea")),
        "IDR" = list(F_x = NULL, ens_matrix = get_ens_matrix(fc_data, "idr")),
        "QRF_LOC" = list(F_x = NULL, ens_matrix = get_ens_matrix(fc_data, "qrf_loc")),
        "QRF_GLO" = list(F_x = NULL, ens_matrix = get_ens_matrix(fc_data, "qrf_glo")),
        "DRN" = list(F_x = ptlogis, mu = fc_data$drn[,1], sig = fc_data$drn[,2]),
        "BQN" = list(F_x = NULL, ens_matrix = get_ens_matrix(fc_data, "bqn")),
        "HEN" = list(F_x = get_F_HEN, bins = fc_data$hen$bin_edges, probs = fc_data$hen$f)
        )

        if (!method %in% names(method_map)) {
        stop("Invalid method specified")
        }

        F_x <- method_map[[method]]$F_x
        mu <- method_map[[method]]$mu %||% NULL
        sig <- method_map[[method]]$sig %||% NULL
        ens_matrix <- method_map[[method]]$ens_matrix %||% NULL
        bins <- method_map[[method]]$bins %||% NULL
        probs <- method_map[[method]]$probs %||% NULL

        twc <- NULL
        owc <- NULL
        com_abs_dist <- NULL
        sev_abs_dist <- NULL


        # Remove missing values
        if (!is.null(mu)){
            missing_index <- which(is.na(mu) | is.na(sig))
        } else if(!is.null(ens_matrix)){
            missing_index <- which(is.na(ens_matrix))
        } else {
            missing_index <- which(is.na(probs) | is.na(bins))
        }
        if (length(missing_index) > 0) {
            obs_vec <- obs_vec[-missing_index]
            if (!is.null(mu)){
                mu <- mu[-missing_index]
                sig <- sig[-missing_index]
            } else if(!is.null(ens_matrix)){
                ens_matrix <- ens_matrix[-missing_index,]
            } else {
                probs <- probs[-missing_index]
                bins <- bins[-missing_index]
            }
        }
        # percentiles <- c(0.9, 0.95, 0.99)
        # percentile_values <- quantile(obs_vec, probs = percentiles, na.rm = TRUE)
        percentile_names <- setNames(paste0(percentiles*100, "th percentile: ", round(percentile_values, 1), " m/s"), percentile_values)

        # Store previous par settings
        old_par <- par(no.readonly = TRUE)
        on.exit(par(old_par))  # Ensure par is reset even if an error occurs

        par(mfrow = c(3, 1))


        if (type == 'plot' || type == 'all' || type == 'abs_dist') {
            com <- get_prob(method, obs_vec, F_x, ens_matrix, c(0, percentile_values))
            sev <- get_prob(method, obs_vec, F_x, ens_matrix, c(0, percentile_values), ratio = "sev")
            occ <- get_prob(method, obs_vec, F_x, ens_matrix, seq(0, ceiling(max(percentile_values)), by = 1), ratio = "occ")

            if (type == 'plot' || type == 'all') {
                if (!dir.exists(plot_dir)) {
                    dir.create(plot_dir, recursive = TRUE)
                }

                # Create a subdirectory for each method if it doesn't exist
                method_dir <- file.path(plot_dir, method)
                if (!dir.exists(method_dir)) {
                    dir.create(method_dir, recursive = TRUE)
                }

                # Plot combined ratio
                com_plot <- plot_ptc(com, ylims = c(0, 1.2),  names = c("S", unname(percentile_names)), title = "Combined Ratio") + theme
                # Plot severity ratio
                sev_plot <- plot_ptc(sev, ratio = "sev",  names = c("S", percentile_names), ylims = c(0, 1.02), title = "Severity Ratio") + theme + theme(legend.position = "none")
                # Plot occurrence ratio
                occ_plot <- plot_ptc(occ, ratio = "occ", ylims = c(0.7, 1.3), xlab = "t (m/s)", title = "Occurrence Ratio") + theme

                filename <- paste0(plot_dir, "/", method, "/lead", lead_time, ".png")
                filname_pdf <- paste0(plot_dir, "/", method, "/lead", lead_time, ".pdf")

                # Combine plots into a 1x3 grid and save
                tryCatch({
                    combined_plot <- grid.arrange(
                        textGrob(
                            paste0("Method: ", method, ", Lead Time: ", lead_time, " h"),
                            gp = gpar(fontsize = 16),
                            y = unit(0.6, "npc")
                        ),
                        arrangeGrob(com_plot, sev_plot, occ_plot, ncol = 3),
                        ncol = 1,
                        heights = c(0.05, 1)
                    )
                    # title <- textGrob(
                    #     paste("Method:", method, ", Lead Time:", lead_time, "h"),
                    #     gp = gpar(fontsize = 16),
                    #     y = unit(0.95, "npc")
                    # )

                    # combined_plot <- arrangeGrob(
                    #     grobs = list(title, arrangeGrob(com_plot, sev_plot, occ_plot, ncol = 3)),
                    #     ncol = 1,
                    #     heights = c(0.1, 1)
                    # )
                    #combined_plot <- gridExtra::grid.arrange(com_plot, sev_plot, occ_plot, ncol = 3, top = textGrob(paste("Method:", method, ", Lead Time:", lead_time, "h"), gp = gpar(fontsize = 16), y = unit(0.95, "npc")))
                    ggsave(filename = filename, plot = combined_plot, width = 18, height = 6)
                    ggsave(filename = filname_pdf, plot = combined_plot, width = 18, height = 6)
                    print(paste0("Plot saved successfully at ", filename))
                }, error = function(e) {
                    message("Error during first try: ", e$message)

                    # title <- textGrob(
                    #     paste("Method:", method, ", Lead Time:", lead_time, "h"),
                    #     gp = gpar(fontsize = 16),
                    #     y = unit(0.95, "npc")
                    # )

                    # combined_plot <- arrangeGrob(
                    #     grobs = list(title, arrangeGrob(com_plot, sev_plot, occ_plot, ncol = 3)),
                    #     ncol = 1,
                    #     heights = c(0.1, 1)
                    # )
                    combined_plot <- grid.arrange(
                        textGrob(
                            paste0("Method: ", method, ", Lead Time: ", lead_time, " h"),
                            gp = gpar(fontsize = 16),
                            y = unit(0.6, "npc")
                        ),
                        arrangeGrob(com_plot, sev_plot, occ_plot, ncol = 3),
                        ncol = 1,
                        heights = c(0.05, 1)
                    )
                    #combined_plot <- gridExtra::grid.arrange(com_plot, sev_plot, occ_plot, ncol = 3, top = textGrob(paste("Method:", method, ", Lead Time:", lead_time, "h"), gp = gpar(fontsize = 16), y = unit(0.95, "npc")))
                    ggsave(filename = filename, plot = combined_plot, width = 18, height = 6)
                    ggsave(filename = filname_pdf, plot = combined_plot, width = 18, height = 6)
                    print(paste0("Plot saved successfully at ", filename))
                })
            }

            if (type == 'abs_dist' || type == 'all') {
                com_abs_dist <- sapply(names(com), function(x) max(abs(com[[x]]['u'] - com[[x]]['rat'])))
                sev_abs_dist <- sapply(names(sev), function(x) max(abs(sev[[x]]['u'] - sev[[x]]['rat'])))
            }
        }

        if (type == 'crps' || type == 'all') {
            #compute (weighted) CRPS

            if (method == 'HEN'){
                # Split bins to have finer resolution
                # Create more detailed bin edges and corresponding probabilities
                # for each forecast instance
                refined_bins <- lapply(bins, function(bin_edges) {
                    if (length(bin_edges) <= 1) return(bin_edges)
                    
                    # For each pair of adjacent bin edges, create intermediate points
                    new_edges <- numeric(0)
                    for (i in 1:(length(bin_edges)-1)) {
                        # Create 10 sub-bins within each original bin
                        step_size <- (bin_edges[i+1] - bin_edges[i]) * 0.1
                        # Add intermediate points (excluding the last to avoid duplicates)
                        if (i < length(bin_edges)-1) {
                            new_edges <- c(new_edges, seq(bin_edges[i], bin_edges[i+1]-step_size, by=step_size))
                        } else {
                            # For the last bin, include the final point
                            new_edges <- c(new_edges, seq(bin_edges[i], bin_edges[i+1], by=step_size))
                        }
                    }
                    return(new_edges)
                })

                # Redistribute probabilities to match the refined bins
                refined_probs <- lapply(1:length(probs), function(i) {
                    old_probs <- probs[[i]]
                    old_bins <- bins[[i]]
                    new_bins <- refined_bins[[i]]
                    
                    # Initialize new probabilities vector
                    new_probs <- numeric(length(new_bins) - 1)
                    
                    # For each original bin, distribute its probability among the corresponding refined bins
                    bin_count <- 0
                    for (j in 1:(length(old_bins)-1)) {
                        # Count how many refined bins fall within this original bin
                        sub_bins <- sum((new_bins >= old_bins[j]) & (round(new_bins, 10) < round(old_bins[j+1], 10)))
                        if (sub_bins > 0) {
                            # Distribute probability evenly among sub-bins
                            new_probs[(bin_count + 1):(bin_count+sub_bins)] <- old_probs[j] / sub_bins
                            bin_count <- bin_count + sub_bins
                        }
                    }
                    
                    return(new_probs)
                })

                # Update bins and probs with refined versions for calculations
                bins <- refined_bins
                probs <- refined_probs
            }

            twc <- sapply(tvals, function(t) get_crps(method, obs_vec, F_x, ens_matrix, probs, bins, mu, sig, a = t, type = "twcrps"))
            owc <- sapply(tvals, function(t) get_crps(method, obs_vec, F_x, ens_matrix, probs, bins, mu, sig, a = t, type = "owcrps"))
        }

        return (list(twc = twc, owc = owc, com_dist = com_abs_dist, sev_dist = sev_abs_dist))

    }, error = function(e) {
        cat("Error in method", method, "at lead time", lead_time, ":", e$message, "\n")
    })
}


#%% whole dataset ----

t_vals <- c(0:20)
obs_vec_complete <- data.frame(obs_vec = numeric(), lead_time = numeric())
fc_data_complete <- list()

for (l in 0:21) {
    rm(fc_data, obs_vec, pit_rank)

    load(paste0("pp_forecasts/forecast_data_step", l, ".RData"))
    obs_vec_complete <- rbind(obs_vec_complete, data.frame(obs_vec = obs_vec, lead_time = l))
    # print(paste0("Processing lead time ", l))
    # print(paste0("Number of observations: ", length(obs_vec)))
    for (method in names(fc_data)){
        if (method == 'ens') {
            fc_data_complete$ens <- rbind(fc_data_complete$ens, fc_data$ens)
        } else if (method == 'hen') {
            fc_data_complete$hen <- list(bin_edges = c(fc_data_complete$hen$bin_edges, fc_data$hen$bin_edges),
                                         f = c(fc_data_complete$hen$f, fc_data$hen$f))
        } else {
            fc_data_complete[[method]] <- rbind(fc_data_complete[[method]], fc_data[[method]])
        }
    }
}
# method <- "ENS"
for (method in toupper(names(fc_data_complete))) {
    print(paste0("Processing ", method, " for all lead times"))
    percentiles <- c(0.9, 0.95, 0.99)
    percentile_values <- quantile(obs_vec_complete$obs_vec, probs = percentiles, na.rm = TRUE)

    plot_postproc(method, obs_vec_complete$obs_vec, fc_data_complete, lead_time = '0-21', tvals = t_vals, type = 'plot')
}

crps_df_all <- data.frame(method = character(), t = numeric(), twcrps = numeric(), owcrps = numeric())
for(method in toupper(names(fc_data_complete)[2:(length(names(fc_data_complete)))])) {
    print(paste0("Processing ", method, " for all lead times"))
    c <- plot_postproc(method, obs_vec_complete$obs_vec, fc_data_complete, lead_time = '0-21', tvals = t_vals, type = 'crps')
    crps_df_all <- rbind(crps_df_all, data.frame(method = method, t = t_vals, twcrps = c$twc, owcrps = c$owc))
}
write.csv(crps_df_all, file = "scores/crps_scores_all.csv", row.names = FALSE)


#%% each lead time separately ----

# data frame for crps scores
crps_df <- data.frame(method = character(), lead_time = numeric(), t = numeric(), twcrps = numeric(), owcrps = numeric())
# data frame for maximum absolute distance of calibration plots from diagonal (not used in thesis)
#calibration_abs_dist <- data.frame(method = character(), lead_time = numeric(), t = numeric(), com = numeric(), sev = numeric())


percentiles <- c(0.9, 0.95, 0.99)
percentile_values <- quantile(obs_vec_complete$obs_vec, probs = percentiles, na.rm = TRUE)


# threshold values for crps
t_vals <- c(0:20)
for (l in 0:21) {
    print(paste0("Processing lead time ", l))

    #remove the fc_data from the workspace
    rm(fc_data, obs_vec, pit_rank)

    load(paste0("pp_forecasts/forecast_data_step", l, ".RData"))
    for (method in toupper(names(fc_data)[2:(length(names(fc_data)))])) {
        print(paste0("Processing ", method, " at lead time ", l))
        c <- plot_postproc(method, obs_vec, fc_data, lead_time = l, tvals = t_vals, type = 'plot', percentiles = percentiles, percentile_values = percentile_values)
        #crps_df <- rbind(crps_df, data.frame(method = method, lead_time = l, t = t_vals, twcrps = c$twc, owcrps = c$owc))
        #calibration_abs_dist <- rbind(calibration_abs_dist, data.frame(method = method, lead_time = l, t = t_vals, com = c$com_dist, sev = c$sev_dist))

    }
}
# write.csv(crps_df, file = "scores/crps_scores.csv", row.names = FALSE)
# write.csv(calibration_abs_dist, file = "scores/calibration_abs_dist.csv", row.names = FALSE)


for (t_val in t_vals){
    # for each method, plot crps scores for each lead time for fixed t
    crps_df_t <- crps_df %>% filter(t == t_val)

    p_twcrps <- ggplot(crps_df_t, aes(x = lead_time, y = twcrps, color = method)) +
        geom_line() +
        geom_point() +
        labs(title = paste("TWCRPS for t =", t_val), x = "Lead Time", y = "TWCRPS") +
        theme_minimal() +
        theme(panel.background = element_rect(fill = "white", color = "white"),
              plot.background = element_rect(fill = "white", color = "white"))
    
    ggsave(filename = paste0("plots/crps_plots/twcrps_t_", t_val, ".png"), plot = p_twcrps, width = 10, height = 6)

    p_owcrps <- ggplot(crps_df_t, aes(x = lead_time, y = owcrps, color = method)) +
        geom_line() +
        geom_point() +
        labs(title = paste("OWCRPS for t =", t_val), x = "Lead Time", y = "OWCRPS") +
        theme_minimal() + 
        theme(panel.background = element_rect(fill = "white", color = "white"),
              plot.background = element_rect(fill = "white", color = "white"))

    ggsave(filename = paste0("plots/crps_plots/owcrps_t_", t_val, ".png"), plot = p_owcrps, width = 10, height = 6)
}

