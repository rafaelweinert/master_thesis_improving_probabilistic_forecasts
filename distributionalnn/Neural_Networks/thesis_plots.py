#%%

import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import os
wd = os.getcwd()

#%% histogram of day-ahead electricity prices

data_path = os.path.join(wd, 'Datasets', 'DE.csv')
data = pd.read_csv(data_path)
data = data.loc[168:, :]

percentiles = [1, 2.5, 5, 10, 90, 95, 97.5, 99]  # Use percent values for labeling
percentile_values = np.percentile(data['Price'], percentiles)

# Calculate counts below and above percentiles
below_counts = []
above_counts = []

for p, val in zip(percentiles, percentile_values):
    if p < 50:  # Lower percentiles
        count_below = np.sum(data['Price'] <= val)
        below_counts.append(count_below)
        print(f"Values below {p}th percentile ({val:.2f} EUR/MWh): {count_below}")
    else:  # Upper percentiles
        count_above = np.sum(data['Price'] >= val)
        above_counts.append(count_above)
        print(f"Values above {p}th percentile ({val:.2f} EUR/MWh): {count_above}")


plt.figure(figsize=(8, 4.5))  # 16:9 aspect ratio for thesis
plt.hist(data['Price'], bins=50, color='#1f77b4', alpha=0.8, edgecolor='black', linewidth=0.5)
plt.title('Day-Ahead Electricity Prices', fontsize=14, pad=15)
plt.xlabel('Price (EUR/MWh)', fontsize=12)
plt.ylabel('Frequency', fontsize=12)
plt.xticks(fontsize=10)
plt.yticks(fontsize=10)
plt.grid(axis='y', alpha=0.3, linestyle='--', linewidth=0.7)

# Add horizontal lines and annotate percentiles
ax = plt.gca()
ymin, ymax = ax.get_ylim()

colors = ['#d62728', '#ff7f0e', '#2ca02c', '#1f77b4', '#9467bd', '#8c564b', '#e377c2', '#7f7f7f']
handles = []
labels = []
for idx, (p, val) in enumerate(zip(percentiles, percentile_values)):
    color = colors[idx % len(colors)]
    line = ax.axvline(val, color=color, linestyle='--', linewidth=1.5)
    handles.append(line)
    # Add proper ordinal suffix (1st, 2nd, 3rd, etc.)
    if p == 1:
        suffix = 'st'
    elif p == 2:
        suffix = 'nd'
    elif p == 3:
        suffix = 'rd'
    else:
        suffix = 'th'
    labels.append(f"{p}{suffix} percentile: {val:.2f} €")

# Add legend for percentile lines
ax.legend(handles=handles, labels=labels, loc='upper right', fontsize=9, frameon=True)

plt.tight_layout(pad=1.5)
plt.savefig(os.path.join(wd, 'plots', 'thesis', 'price_histogram.png'), dpi=600, bbox_inches='tight')
plt.savefig(os.path.join(wd, 'plots', 'thesis', 'price_histogram.pdf'), bbox_inches='tight', dpi = 300)
plt.show()

# %% line plot of day-ahead electricity prices


data_path = os.path.join(wd, 'Datasets', 'DE.csv')
data = pd.read_csv(data_path)
data = data.loc[168:, :]
data[data.columns[0]] = pd.to_datetime(data[data.columns[0]], format='%Y-%m-%d %H:%M:%S')
data = data.rename(columns={data.columns[0]: 'Time'})
plt.figure(figsize=(12, 4.5))  # 16:9 aspect ratio for thesis
plt.plot(data['Time'], data['Price'], color='#1f77b4', linewidth=1.5)
plt.title('Day-Ahead Electricity Prices Over Time', fontsize=14, pad=15)
plt.xlabel('Time (Hourly)', fontsize=12)
plt.ylabel('Price (EUR/MWh)', fontsize=12)
plt.xticks(fontsize=10)
plt.yticks(fontsize=10)
plt.grid(axis='y', alpha=0.3, linestyle='--', linewidth=0.7)
plt.tight_layout(pad=1.5)
plt.savefig(os.path.join(wd, 'plots', 'thesis', 'price_line_plot.png'), dpi=300, bbox_inches='tight')
plt.savefig(os.path.join(wd, 'plots', 'thesis', 'price_line_plot.pdf'), bbox_inches='tight', dpi = 300)



# %%
data_path = os.path.join(wd, 'Datasets', 'DE.csv')
data = pd.read_csv(data_path)
data = data.loc[168:, :]

negative_prices = np.sum(data['Price'] <= 0)
print(f"Number of prices below 0 EUR/MWh: {negative_prices}")
print(f"Percentage of prices below 0 EUR/MWh: {negative_prices / len(data) * 100:.2f}%")
# %%
