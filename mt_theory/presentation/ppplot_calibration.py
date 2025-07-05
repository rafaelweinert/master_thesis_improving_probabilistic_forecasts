
#%%

import numpy as np
import matplotlib.pyplot as plt
import scipy.stats as stats

# Generate true values from a standard normal distribution
n = 10000
true_values = np.random.normal(loc=0, scale=1, size=n)

# Generate forecast samples that are underdispersed (too narrow variance)
# For example, forecast is Normal with the same mean but smaller std dev
forecast_mean = true_values
forecast_std = 2  # smaller than true std=1, causing underdispersion

# Calculate Probability Integral Transform (PIT) values
# PIT = CDF of forecast evaluated at the true values
pit_values = stats.norm.cdf(true_values, loc=0, scale=forecast_std)

# Plot PIT values on a PP-plot (against uniform)
plt.figure(figsize=(6,6))
plt.plot(np.linspace(0,1, n), np.sort(pit_values), linestyle='-')
plt.plot([0, 1], [0, 1], 'r--', label='Perfect calibration')
plt.xlabel('Theoretical quantiles')
plt.ylabel('Sample quantiles')
plt.legend()


# stats.probplot(pit_values, dist="uniform", plot=plt)
plt.title("PP-plot of miscalibrated forecast PIT values")
plt.savefig('presentation/plots/ppplot_calibration.png', bbox_inches='tight', dpi=300)

# %%
