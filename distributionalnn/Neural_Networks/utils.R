# General function for quantile-averaged distribution functions
qavg_cdf <- function(q, param_sets, qfunc = NULL, dist_type = NULL, p_grid_length = 1000, p_min = 0.0001, p_max = 0.9999, ...) {
    # Arguments:
    #   q: numeric vector of quantiles
    #   param_sets: dataframe with parameters for the distribution, each suffix (_1, _2, ...) indicating the respective set
    #   qfunc: custom quantile function (overrides dist_type if provided)
    #   dist_type: distribution type ("jsu" or "gumbel")
    #   p_grid_length: length of probability grid for approximation
    #   p_min, p_max: bounds for probability grid
    #   ...: additional arguments passed to quantile function

    # Input validation
    if (is.null(param_sets)) {
        stop("param_sets must be provided")
    }

    # Ensure param_sets is a list
    if (!is.data.frame(param_sets)) {
        stop("param_sets must be a data frame")
    }

    # Select appropriate quantile function
    if (!is.null(qfunc)) {
        quantile_func <- qfunc
    } else if (!is.null(dist_type)) {
        if (dist_type == "JSU") {
            quantile_func <- function(p, params) {
                loc <- params$loc
                scale <- params$scale
                tailweight <- params$tailweight
                skewness <- params$skewness
                loc + scale * sinh((qnorm(p) - skewness) / tailweight)
            }
        } else if (dist_type == "Gumbel") {
            quantile_func <- function(p, params) {
                loc <- params[1]
                scale <- params[2]
                loc - scale * log(-log(p))
            }
        } else {
            stop("Unsupported distribution type. Currently built-in: 'JSU', 'Gumbel'")
        }
    } else {
        stop("Either qfunc or dist_type must be provided")
    }


    np <- dim(param_sets)[1]


    # Build probability grid
    p_grid <- seq(p_min, p_max, length.out = p_grid_length)

    # Initialize 3D matrix for storing quantile values
    # Dimensions: np (parameter sets) x m (sets of distributions) x p_grid_length (probability grid)   
    m <- 4 

    # Create the 3D array
    quantiles_array <- array(NA_real_, dim = c(m, np, p_grid_length))

    # Vectorized calculation of quantiles_array
    for (i in 1:m) {
        cols <- grep(paste0("_", i, "$"), names(param_sets), value = TRUE)
        param_subset <- param_sets[, cols, drop = FALSE]
        # Remove the suffix "_i" from column names
        colnames(param_subset) <- sub(paste0("_", i, "$"), "", colnames(param_subset))
        quantiles_array[i,, ] <- sapply(p_grid, function(p) quantile_func(p, param_subset))
    }
    # Average across the first dimension (m distributions)
    dim(quantiles_array) <- c(m, np * p_grid_length)
    avg_flat <- colMeans(quantiles_array)
    avg_quantiles <- matrix(avg_flat, nrow = np, ncol = p_grid_length)

    # Ensure monotonicity along the probability grid for each parameter set
    for (i in 1:np) {
        avg_quantiles[i, ] <- cummax(avg_quantiles[i, ])
    }

    # Process input quantiles
    q_vec <- as.numeric(q)
    nq <- length(q_vec)
    # Check for one-to-one correspondence
    one_to_one <- (nq == np)
 
    if (one_to_one){
        result <- sapply(1:np, function(i) {
            cdf_func <- approxfun(
                x = avg_quantiles[i, ], y = p_grid,
                method = "linear",
                yleft = 0, yright = 1,
                ties = "ordered",
                rule = 2
            )
            cdf_func(q_vec[i])
        })
        return(result)
    }
    else if (nq == 1) {
        result <- sapply(1:np, function(i) {
            cdf_func <- approxfun(
                x = avg_quantiles[i, ], y = p_grid,
                method = "linear",
                yleft = 0, yright = 1,
                ties = "ordered",
                rule = 2
            )
            cdf_func(q_vec)
        })
        return(result)
    }
    else {
       stop("Invalid input: nq must be 1 or np must be equal to nq")
    }
}


create_qavg_cdf <- function(param_sets, qfunc = NULL, dist_type = NULL, p_grid_length = 1000, p_min = 0.0001, p_max = 0.9999, ...) {
    if (is.null(param_sets) || !is.data.frame(param_sets)) {
        stop("param_sets must be a data frame and not NULL")
    }

    if (!is.null(qfunc)) {
        quantile_func <- qfunc
    } else if (!is.null(dist_type)) {
        if (dist_type == "JSU") {
            quantile_func <- function(p, params) {
                loc <- params$loc
                scale <- params$scale
                tailweight <- params$tailweight
                skewness <- params$skewness
                loc + scale * sinh((qnorm(p) - skewness) / tailweight)
            }
        } else if (dist_type == "Gumbel") {
            quantile_func <- function(p, params) {
                loc <- params[1]
                scale <- params[2]
                loc - scale * log(-log(p))
            }
        } else {
            stop("Unsupported distribution type. Currently built-in: 'JSU', 'Gumbel'")
        }
    } else {
        stop("Either qfunc or dist_type must be provided")
    }

    np <- nrow(param_sets)
    p_grid <- seq(p_min, p_max, length.out = p_grid_length)

    # Determine the number of distributions (suffixes like _1, _2, etc.)
    # Find all numeric suffixes in the column names
    all_suffixes <- gsub(".*_", "", names(param_sets))
    suffixes <- all_suffixes[grepl("^\\d+$", all_suffixes)]
    suffixes <- unique(suffixes)
    m <- length(suffixes)

    quantiles_array <- array(NA_real_, dim = c(m, np, p_grid_length))

    for (i in seq_along(suffixes)) {
        suffix <- suffixes[i]
        cols <- grep(paste0("_", suffix, "$"), names(param_sets), value = TRUE)
        param_subset <- param_sets[, cols, drop = FALSE]
        colnames(param_subset) <- sub(paste0("_", suffix, "$"), "", colnames(param_subset))
        quantiles_array[i, , ] <- sapply(p_grid, function(p) quantile_func(p, param_subset))
    }

    dim(quantiles_array) <- c(m, np * p_grid_length)
    avg_flat <- colMeans(quantiles_array)
    avg_quantiles <- matrix(avg_flat, nrow = np, ncol = p_grid_length)

    for (i in 1:np) {
        avg_quantiles[i, ] <- cummax(avg_quantiles[i, ])
    }

    # Precompute CDF functions once
    cdf_funcs <- lapply(1:np, function(i) {
        approxfun(
            x = avg_quantiles[i, ], y = p_grid,
            method = "linear",
            yleft = 0, yright = 1,
            ties = "ordered",
            rule = 2
        )
    })

    # Return the callable function
    function(q) {
        q_vec <- as.numeric(q)
        nq <- length(q_vec)
        one_to_one <- (nq == np)

        if (one_to_one) {
            mapply(function(f, q_val) f(q_val), cdf_funcs, q_vec)
        } else if (nq == 1) {
            vapply(cdf_funcs, function(f) f(q_vec), numeric(1))
        } else {
            stop("Invalid input: nq must be 1 or np must be equal to nq")
        }
    }
}

# Function to sample from the averaged distribution
sample_qavg_distribution <- function(n, param_sets, qfunc = NULL, dist_type = NULL, ...) {
    # First, create the CDF function
    cdf_func <- create_qavg_cdf(param_sets, qfunc, dist_type, ...)

    # Generate uniform random values
    u <- runif(n)

    x_grid <- seq(from = -150, to = 800, length.out = 5000)
    p_grid <- sapply(x_grid, function(x) cdf_func(x))
    q_funcs <- lapply(1:dim(param_sets)[1], function(i) {
        approxfun(
            x = p_grid[i,],
            y = x_grid,
            method = "linear",
            yleft = min(x_grid),
            yright = max(x_grid),
            rule = 2
        )
    })

    # Sample using inverse transform sampling
    samples <- t(vapply(q_funcs, function(q_func) q_func(u), numeric(n)))

    return(samples)
}

set_percentile_names <- function(percentiles, percentile_values, side) {
    if (side == 'upper') {
        percentile_names <- setNames(
            sapply(percentiles, function(p) {
                per <- p * 100
                suffix <- if (per %in% c(1, 21, 31, 41, 51, 61, 71, 81, 91)) {
                    "st"
                } else if (per %in% c(2, 22, 32, 42, 52, 62, 72, 82, 92)) {
                    "nd"
                } else if (per %in% c(3, 23, 33, 43, 53, 63, 73, 83, 93)) {
                    "rd"
                } else {
                    "th"
                }
                idx <- which(percentiles == p)
                paste0(per, suffix, " percentile: ", round(percentile_values[idx], 2), "€")
            }),
            percentile_values
        )
    } else if (side == 'lower') {
        percentile_names <- setNames(
            sapply(percentiles, function(p) {
                per <- p * 100
                suffix <- if (per %in% c(1, 21, 31, 41, 51, 61, 71, 81, 91)) {
                    "st"
                } else if (per %in% c(2, 22, 32, 42, 52, 62, 72, 82, 92)) {
                    "nd"
                } else if (per %in% c(3, 23, 33, 43, 53, 63, 73, 83, 93)) {
                    "rd"
                } else {
                    "th"
                }
                idx <- which(percentiles == p)
                paste0(per, suffix, " percentile: ", round(-percentile_values[idx], 2), "€")
            }),
            -percentile_values
        )
    }
    return(percentile_names)
}

        





# distribution function for the left-sided Gumbel distribution
pgumbel_min <- function(x, loc = 0, scale = 1) {
    1 - exp(-exp((x + loc) / scale))
}


theme <- theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
)




# Config

distributions <- c(
    'Normal',
    'JSU'
    # 'Logistic'
    # 'StudentT',
    # 'SinhArcsinh',
    # 'TwoPieceNormal'
)
losses <- c(
    # "CRPS",
    # 'CRPS_reg_output',
    # 'LOG_PROB_reg_output',
    # #"LOG_PROB_twCRPS_lower"
    'LOG_PROB'
    # 'wCRPS',
    # 'wCRPS_negative_price'
) 
random_sampling <- c(
    TRUE
)
new_data <- c(
    FALSE
    # TRUE
)
sides <- c(
    "upper",
    "lower"
)

seeds <- c(
    #42,
    # 43,
    10
)

upper_percentiles <- c(0.9, 0.95, 0.975, 0.99)
lower_percentiles <- c(0.1, 0.05, 0.025, 0.01)
