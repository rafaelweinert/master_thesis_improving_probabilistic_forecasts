# Example of the unfocused forecaster that is probabilistic but not tail calibrated
# Set seed for reproducibility
library(TailCalibration)
library(gridExtra)
library(ggplot2)
setwd("C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/mt_theory")

set.seed(123)

# Parameters
n_samples <- 100000
tau_prob <- 0.5

G <- function(y) {
    # CDF of U(0,1)
    #pmax(0, pmin(1, y))
    pnorm(y, mean = 0, sd = 0.5) # Using a normal distribution for demonstration
}

F <- function(y, tau) {
    # Forecaster F = 0.5 * (G(y) + G(y + tau))
    0.5 * (G(y) + G(y + tau))
}

# Draw 10000 samples from U(0,1)
# y <- runif(n_samples, min = 0, max = 1)
y <- rnorm(n_samples, mean = 0, sd = 0.5) # Transforming to a normal distribution for demonstration

# Generate tau values: 1 or -1 with probability 0.5 each
tau_values <- sample(c(-1, 1), size = n_samples, replace = TRUE, prob = c(tau_prob, tau_prob))

# Define the forecaster F = 0.5 * (G(y) + G(y + tau))
# Since G ~ U(0,1), G(y) = y for y in [0,1] and 0 or 1 outside
# We need to define y values to evaluate the forecaster

F_values <- F(y, tau_values)


theme <- theme(
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    plot.title = element_text(hjust = 0.5, size = 16),
    legend.text = element_text(size = 14),
    axis.text.x = element_text(size = 13),
    axis.text.y = element_text(size = 13)
)

com <- tc_prob(y, F, tau = tau_values, t = c(-Inf, -1, 0, 1), ratio = "com")
sev <- tc_prob(y, F, tau = tau_values, t = c(-Inf, -1, 0, 1), ratio = 'sev')
occ <- tc_prob(y, F, tau = tau_values, t = seq(-4, 4, 0.01), ratio = 'occ')

com_plot <- plot_ptc(com, title = "Combined Ratio" ) + theme
sev_plot <- plot_ptc(sev, ratio = 'sev', title = "Severity Ratio" ) + theme + theme(legend.position = "none")
occ_plot <- plot_ptc(occ, ratio = 'occ', title = "Occurrence Ratio") + theme



combined_plot <- grid.arrange(com_plot, sev_plot, occ_plot, ncol = 3, padding = unit(1, "line")) # , top = plot_title)
# ggsave(filename = filename, plot = combined_plot, width = 18, height = 6)

filename_pdf <- "plots/calibration_not_tailcalibration.pdf"
ggsave(filename = filename_pdf, plot = combined_plot, width = 18, height = 6)
ggsave(filename = "plots/calibration_not_tailcalibration.png", plot = combined_plot, width = 18, height = 6)
