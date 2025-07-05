# Load required packages
library(scoringRules)
library(ggplot2)
library(gridExtra)
setwd("C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/mt_theory")

# Simulate data from a right-skewed distribution (e.g., Gamma)
set.seed(123)

shape <- 5
rate <- 4

# Compute CRPS and Log Score for a range of values using density functions
x_vals <- seq(0.01, 6, length.out = 1000)
g <- dgamma(x_vals, shape = shape,rate = rate) # density of the Gamma distribution
log_scores <- logs(y = x_vals, family = "gamma", shape = shape,rate = rate)
crps_scores <- crps(y = x_vals, family = "gamma", shape = shape,rate = rate)

# Create a combined plot with all three metrics
combined_data <- data.frame(
    x = rep(x_vals, 3),
    value = c(g, log_scores, crps_scores),
    metric = factor(rep(c("Density", "Log Score", "CRPS"), each = length(x_vals)))
)

# Or use dual y-axis approach
# Find minimum points
min_log_idx <- which.min(log_scores)
min_crps_idx <- which.min(crps_scores)
min_points <- data.frame(
    x = c(x_vals[min_log_idx], x_vals[min_crps_idx]),
    y = c(log_scores[min_log_idx], crps_scores[min_crps_idx]),
    metric = factor(c("Log Score", "CRPS"))
)

ggplot(data.frame(x = rep(x_vals, 2), 
                  value = c(log_scores, crps_scores),
                  metric = factor(rep(c("Log Score", "CRPS"), each = length(x_vals)))), 
       aes(x = x, y = value, color = metric)) +
    geom_area(data = data.frame(x = x_vals, density = g * max(c(log_scores, crps_scores)) / max(g)), 
              aes(x = x, y = density), fill = "#3f883f", alpha = 0.3, inherit.aes = FALSE) +
    geom_line(size = 1.2) +
    # geom_point(data = min_points, aes(x = x, y = y, color = metric), 
    #            shape = 1, size = 4, stroke = 2) +
    scale_y_continuous(
        name = "Score Value",
        sec.axis = sec_axis(~ . * max(g) / max(c(log_scores, crps_scores)), name = "Density")
    ) +
    labs(title = "Gamma Distribution: Density, Log Score, and CRPS",
         x = "y", color = "Metric") +
    scale_color_manual(values = c("Log Score" = "blue", "CRPS" = "red")) +
    theme_minimal(base_size = 16) +
    theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 20)),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14),
        axis.title.y = element_text(margin = margin(r = 15)),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y.right = element_text(margin = margin(l = 15)),
        legend.position = "right",
        legend.text = element_text(size = 12),
    )


ggsave("plots/crps_vs_log_score_plot.pdf", width = 10, height = 6, dpi = 300)
ggsave("plots/crps_vs_log_score_plot.png", width = 10, height = 6, dpi = 300)

crps