
setwd("C:/Users/rafaelweinert/PycharmProjects/Master_Thesis_Code/mt_theory")

x <- seq(-5, 5, length.out = 1000)
f <- dnorm(x)
F <- pnorm(x)
t <- 0

# owcrps
F_w <- function(x, t) {
    ifelse(x <= t, 0, (pnorm(x) - pnorm(t)) / (1 - pnorm(t)))
}

# twcrps
F_c <- function(x, t) {
    ifelse(x <= t, 0, pnorm(x))
}


#%%
# dev.new()
y <- 1

#first plot
pdf("plots/owcrps_twcrps_cdf_plot.pdf", width = 12, height = 4)
    # Set up 1x3 grid layout
par(mfrow = c(1, 3), mar = c(4, 5, 3, 2), oma = c(3.5, 0, 0, 0)) # oma bottom margin increased
# Plot 1: CRPS CDF
plot(x, F, type = "l", main = "CRPS CDF", col = "blue", lwd = 2, ylim = c(0, 1), cex.lab = 1.3, ylab = expression(F(x)))
abline(v = y, col = "green", lwd = 2, lty = 2)
abline(v = t, col = "red", lwd = 2, lty = 2)
polygon(c(x[x <= y], rev(x[x <= y])), c(rep(0, sum(x <= y)), rev(F[x <= y])),
    col = rgb(0, 0, 1, 0.3), border = NA
)
polygon(c(x[x >= y], rev(x[x >= y])), c(F[x >= y], rep(1, sum(x >= y))),
    col = rgb(0, 0, 1, 0.3), border = NA
)

# Plot 2: Weighted CDF
plot(x, F_w(x, t), type = "l", main = "owCRPS CDF", col = "orange", lwd = 2, ylim = c(0, 1), cex.lab = 1.3, ylab = expression(F[w](x, t)))
abline(v = y, col = "green", lwd = 2, lty = 2)
abline(v = t, col = "red", lwd = 2, lty = 2)
polygon(c(x[x <= y], rev(x[x <= y])), c(rep(0, sum(x <= y)), rev(F_w(x[x <= y], t))),
    col = rgb(1, 0.5, 0, 0.3), border = NA
)
polygon(c(x[x >= y], rev(x[x >= y])), c(F_w(x[x >= y], t), rep(1, sum(x >= y))),
    col = rgb(1, 0.5, 0, 0.3), border = NA
)

# Plot 3: Censored CDF
plot(x, F_c(x, t), type = "l", main = "twCRPS CDF", col = "purple", lwd = 2, ylim = c(0, 1), cex.lab = 1.3, ylab = expression(F[c](x, t)))
abline(v = y, col = "green", lwd = 2, lty = 2)
abline(v = t, col = "red", lwd = 2, lty = 2)
polygon(c(x[x <= y], rev(x[x <= y])), c(rep(0, sum(x <= y)), rev(F_c(x[x <= y], t))),
    col = rgb(0.5, 0, 0.5, 0.3), border = NA
)
polygon(c(x[x >= y], rev(x[x >= y])), c(F_c(x[x >= y], t), rep(1, sum(x >= y))),
    col = rgb(0.5, 0, 0.5, 0.3), border = NA
)

# Add combined legend at the bottom with increased margin
par(fig = c(0, 1, 0, 0.5), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom",
    legend = c(expression(F(x)), expression(F[w](x, t == 0)), expression(F[c](x, t == 0)), expression(y == 1), expression(t == 0)),
    col = c("blue", "orange", "purple", "green", "red"),
    lty = c(1, 1, 1, 2, 2), lwd = 2, ncol = 5, xpd = TRUE,
    y.intersp = 2, inset = c(0, 0.05), bty = "n", cex = 1.2
)

dev.off()



#%%
#second plot 
y <- -0.5
# first plot
pdf("plots/owcrps_twcrps_cdf_plot_2.pdf", width = 12, height = 4)
# Set up 1x3 grid layout
par(mfrow = c(1, 3), mar = c(4, 5, 3, 2), oma = c(3.5, 0, 0, 0)) # oma bottom margin increased
# Plot 1: CRPS CDF
plot(x, F, type = "l", main = "CRPS CDF", col = "blue", lwd = 2, ylim = c(0, 1), cex.lab = 1.3, ylab = expression(F(x)))
abline(v = y, col = "green", lwd = 2, lty = 2)
abline(v = t, col = "red", lwd = 2, lty = 2)
polygon(c(x[x <= y], rev(x[x <= y])), c(rep(0, sum(x <= y)), rev(F[x <= y])),
    col = rgb(0, 0, 1, 0.3), border = NA
)
polygon(c(x[x >= y], rev(x[x >= y])), c(F[x >= y], rep(1, sum(x >= y))),
    col = rgb(0, 0, 1, 0.3), border = NA
)

# Plot 2: Weighted CDF
plot(x, F_w(x, t), type = "l", main = "owCRPS CDF", col = "orange", lwd = 2, ylim = c(0, 1), cex.lab = 1.3, ylab = expression(F[w](x, t)))
abline(v = y, col = "green", lwd = 2, lty = 2)
abline(v = t, col = "red", lwd = 2, lty = 2)
# polygon(c(x[x <= y], rev(x[x <= y])), c(rep(0, sum(x <= y)), rev(F_w(x[x <= y], t))),
#     col = rgb(1, 0.5, 0, 0.3), border = NA
# )
# polygon(c(x[x >= y], rev(x[x >= y])), c(F_w(x[x >= y], t), rep(1, sum(x >= y))),
#     col = rgb(1, 0.5, 0, 0.3), border = NA
# )

# Plot 3: Censored CDF
plot(x, F_c(x, t), type = "l", main = "twCRPS CDF", col = "purple", lwd = 2, ylim = c(0, 1), cex.lab = 1.3, ylab = expression(F[c](x, t)))
abline(v = y, col = "green", lwd = 2, lty = 2)
abline(v = t, col = "red", lwd = 2, lty = 2)
polygon(c(x[x <= t], rev(x[x <= y])), c(rep(0, sum(x <= t)), rev(F_c(x[x <= t], t))),
    col = rgb(0.5, 0, 0.5, 0.3), border = NA
)
polygon(c(x[x >= t], rev(x[x >= t])), c(F_c(x[x >= t], t), rep(1, sum(x >= t))),
    col = rgb(0.5, 0, 0.5, 0.3), border = NA
)

# Add combined legend at the bottom with increased margin
par(fig = c(0, 1, 0, 0.5), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom",
    legend = c(expression(F(x)), expression(F[w](x, t == 0)), expression(F[c](x, t == 0)), expression(y == -0.5), expression(t == 0)),
    col = c("blue", "orange", "purple", "green", "red"),
    lty = c(1, 1, 1, 2, 2), lwd = 2, ncol = 5, xpd = TRUE,
    y.intersp = 2, inset = c(0, 0.05), bty = "n", cex = 1.2
)

dev.off()






#################################################### PNG ####################################################


y <- 1

# High-resolution PNG with specified resolution
png("plots/owcrps_twcrps_cdf_plot.png", width = 12 * 300, height = 4 * 300, res = 300)

# Set up 1x3 layout with proper margins
par(mfrow = c(1, 3), mar = c(4, 5, 3, 2), oma = c(3.5, 0, 0, 0))

# Plot 1: CRPS CDF
plot(x, F,
    type = "l", main = "CRPS CDF", col = "blue", lwd = 2, ylim = c(0, 1),
    ylab = expression(F(x)), cex.lab = 1.5, cex.axis = 1.3, cex.main = 1.6
)
abline(v = y, col = "green", lwd = 2, lty = 2)
abline(v = t, col = "red", lwd = 2, lty = 2)
polygon(c(x[x <= y], rev(x[x <= y])), c(rep(0, sum(x <= y)), rev(F[x <= y])),
    col = rgb(0, 0, 1, 0.3), border = NA
)
polygon(c(x[x >= y], rev(x[x >= y])), c(F[x >= y], rep(1, sum(x >= y))),
    col = rgb(0, 0, 1, 0.3), border = NA
)

# Plot 2: Weighted CDF
plot(x, F_w(x, t),
    type = "l", main = "owCRPS CDF", col = "orange", lwd = 2, ylim = c(0, 1),
    ylab = expression(F[w](x, t)), cex.lab = 1.5, cex.axis = 1.3, cex.main = 1.6
)
abline(v = y, col = "green", lwd = 2, lty = 2)
abline(v = t, col = "red", lwd = 2, lty = 2)
polygon(c(x[x <= y], rev(x[x <= y])), c(rep(0, sum(x <= y)), rev(F_w(x[x <= y], t))),
    col = rgb(1, 0.5, 0, 0.3), border = NA
)
polygon(c(x[x >= y], rev(x[x >= y])), c(F_w(x[x >= y], t), rep(1, sum(x >= y))),
    col = rgb(1, 0.5, 0, 0.3), border = NA
)

# Plot 3: Censored CDF
plot(x, F_c(x, t),
    type = "l", main = "twCRPS CDF", col = "purple", lwd = 2, ylim = c(0, 1),
    ylab = expression(F[c](x, t)), cex.lab = 1.5, cex.axis = 1.3, cex.main = 1.6
)
abline(v = y, col = "green", lwd = 2, lty = 2)
abline(v = t, col = "red", lwd = 2, lty = 2)
polygon(c(x[x <= y], rev(x[x <= y])), c(rep(0, sum(x <= y)), rev(F_c(x[x <= y], t))),
    col = rgb(0.5, 0, 0.5, 0.3), border = NA
)
polygon(c(x[x >= y], rev(x[x >= y])), c(F_c(x[x >= y], t), rep(1, sum(x >= y))),
    col = rgb(0.5, 0, 0.5, 0.3), border = NA
)

# Add legend
par(fig = c(0, 1, 0, 0.5), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom",
    legend = c(
        expression(F(x)), expression(F[w](x, t == 0)), expression(F[c](x, t == 0)),
        expression(y == 1), expression(t == 0)
    ),
    col = c("blue", "orange", "purple", "green", "red"),
    lty = c(1, 1, 1, 2, 2), lwd = 2, ncol = 5, xpd = TRUE,
    y.intersp = 2, inset = c(0, 0.05), bty = "n", cex = 1.4
)

dev.off()



# %%

y <- -0.5

# High-resolution PNG with specified resolution
png("plots/owcrps_twcrps_cdf_plot_2.png", width = 12 * 300, height = 4 * 300, res = 300)

# Set up 1x3 layout with proper margins
par(mfrow = c(1, 3), mar = c(4, 5, 3, 2), oma = c(3.5, 0, 0, 0))

# Plot 1: CRPS CDF
plot(x, F,
    type = "l", main = "CRPS CDF", col = "blue", lwd = 2, ylim = c(0, 1),
    ylab = expression(F(x)), cex.lab = 1.5, cex.axis = 1.3, cex.main = 1.6
)
abline(v = y, col = "green", lwd = 2, lty = 2)
abline(v = t, col = "red", lwd = 2, lty = 2)
polygon(c(x[x <= y], rev(x[x <= y])), c(rep(0, sum(x <= y)), rev(F[x <= y])),
    col = rgb(0, 0, 1, 0.3), border = NA
)
polygon(c(x[x >= y], rev(x[x >= y])), c(F[x >= y], rep(1, sum(x >= y))),
    col = rgb(0, 0, 1, 0.3), border = NA
)

# Plot 2: Weighted CDF (no polygons)
plot(x, F_w(x, t),
    type = "l", main = "owCRPS CDF", col = "orange", lwd = 2, ylim = c(0, 1),
    ylab = expression(F[w](x, t)), cex.lab = 1.5, cex.axis = 1.3, cex.main = 1.6
)
abline(v = y, col = "green", lwd = 2, lty = 2)
abline(v = t, col = "red", lwd = 2, lty = 2)

# Plot 3: Censored CDF
plot(x, F_c(x, t),
    type = "l", main = "twCRPS CDF", col = "purple", lwd = 2, ylim = c(0, 1),
    ylab = expression(F[c](x, t)), cex.lab = 1.5, cex.axis = 1.3, cex.main = 1.6
)
abline(v = y, col = "green", lwd = 2, lty = 2)
abline(v = t, col = "red", lwd = 2, lty = 2)
polygon(c(x[x <= t], rev(x[x <= t])), c(rep(0, sum(x <= t)), rev(F_c(x[x <= t], t))),
    col = rgb(0.5, 0, 0.5, 0.3), border = NA
)
polygon(c(x[x >= t], rev(x[x >= t])), c(F_c(x[x >= t], t), rep(1, sum(x >= t))),
    col = rgb(0.5, 0, 0.5, 0.3), border = NA
)

# Add combined legend at the bottom with larger text
par(fig = c(0, 1, 0, 0.5), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("bottom",
    legend = c(
        expression(F(x)), expression(F[w](x, t == 0)), expression(F[c](x, t == 0)),
        expression(y == -0.5), expression(t == 0)
    ),
    col = c("blue", "orange", "purple", "green", "red"),
    lty = c(1, 1, 1, 2, 2), lwd = 2, ncol = 5, xpd = TRUE,
    y.intersp = 2, inset = c(0, 0.05), bty = "n", cex = 1.4
)

dev.off()








