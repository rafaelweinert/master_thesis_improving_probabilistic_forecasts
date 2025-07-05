#%%
import numpy as np
import matplotlib.pyplot as plt
import scipy.stats as stats

x = np.linspace(-5, 5, 1000)
y = stats.norm.cdf(x, loc=0, scale=1)
y2 = stats.norm.cdf(x, loc=0, scale=2)

plt.figure(figsize=(8, 6))
plt.plot(x, y, label='N(0,1)', color='blue')
plt.plot(x, y2, label='N(0,2)', color='orange')

plt.title('Comparison of Two Normal Distributions')
plt.xlabel('Value')
plt.ylabel('Probability Density')
plt.legend()
plt.grid()
plt.savefig('presentation/plots/sharpness_plot.png', bbox_inches='tight', dpi=300)
plt.show()
# %%
