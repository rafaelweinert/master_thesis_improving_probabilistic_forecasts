library(scoringRules)
library(ggplot2)
library(gridExtra)
setwd("C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/mt_theory")


shape <- 5
rate <- 4

a = 1.5

# Compute CRPS and Log Score for a range of values using density functions
x_vals <- seq(0.01, 4, length.out = 1000)
g <- dgamma(x_vals, shape = shape, rate = rate) # density of the Gamma distribution
s <- rgamma(10000, shape = shape, rate = rate) # sample from the Gamma distribution
crps_scores <- crps(y = x_vals, family = "gamma", shape = shape, rate = rate)
twcrps_scores <- lapply(x_vals, function(x) twcrps_sample(y = x, dat = s, a = a))
owcrps_scores <- lapply(x_vals, function(x) owcrps_sample(y = x, dat = s, a = a))

# Create data frame for ggplot
plot_data <- data.frame(
    x = rep(x_vals, 3),
    score = c(crps_scores, unlist(twcrps_scores), unlist(owcrps_scores)),
    type = rep(c("CRPS", "twCRPS", "owCRPS"), each = length(x_vals))
)

# Scale density values
max_score <- max(c(crps_scores, unlist(twcrps_scores), unlist(owcrps_scores)))
scaled_density <- g * max_score / max(g)

ggplot(plot_data, aes(x = x, y = score, color = type)) +
    geom_area(data = data.frame(x = x_vals, density = scaled_density), 
                        aes(x = x, y = density), fill = "#3f883f", alpha = 0.3, inherit.aes = FALSE) +
    geom_line(linewidth = 1.2) +
    geom_vline(xintercept = a, linetype = "dashed", color = "black") +
    scale_color_manual(values = c("CRPS" = "blue", "twCRPS" = "red", "owCRPS" = "green")) +
    labs(x = "x", y = "(weighted) CRPS", title = "CRPS vs twCRPS vs owCRPS", color = "Score Type") +
    scale_x_continuous(breaks = c(seq(0, 6, by = 1), a), labels = c(as.character(seq(0, 6, by = 1)), "t")) +
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


ggsave("plots/crps_vs_owcrps_vs_twcrps.pdf", width = 10, height = 6, dpi = 300)
