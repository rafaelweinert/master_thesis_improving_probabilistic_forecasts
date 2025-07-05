#%%

import numpy as np
import matplotlib.pyplot as plt
import plotly.graph_objects as go
from scipy.ndimage import zoom


np.random.seed(42)
n = 20
x_vals = np.linspace(10, 100, n)
y_vals = np.random.poisson(lam=50, size=n) + np.linspace(0, 10, n)
data = list(zip(x_vals, y_vals))



# Given data (x_i, y_i)
data = [(10, 1), (20, 3), (30, 2), (40, 4)]
x_vals, y_vals = zip(*data)
n = len(y_vals)

# Define range of z values for CDF evaluation
z_vals = np.linspace(min(y_vals) - 1, max(y_vals) + 1, 1000)

# Store previous estimated CDFs
estimated_cdfs = []

# Loop through each i
for i in range(1, n + 1):
    fig, ax = plt.subplots(figsize=(8, 5))
    max_cdf_vals = np.zeros_like(z_vals)

    # Step functions for j from i to n
    for j in range(i - 1, n):
        subseq = y_vals[i-1:j+1]
        ecdf_vals = np.array([np.mean(np.array(subseq) <= z) for z in z_vals])
        ax.step(z_vals, ecdf_vals, where='post', color='black', alpha=0.6)
        max_cdf_vals = np.maximum(max_cdf_vals, ecdf_vals)

    # Plot maximum over all j (in red)
    ax.step(z_vals, max_cdf_vals, where='post', color='red', label='max over j')

    # Plot previous estimated CDF if i > 1 (in blue)
    if i > 1:
        ax.step(z_vals, np.min(np.array(estimated_cdfs), axis = 0), where='post', color='blue', linestyle='--', label=r'$\hat{F}_{%d}(z)$' % (i - 1))

    # Save current estimated CDF for future reference (this is max over j for fixed k = i)
    estimated_cdfs.append(max_cdf_vals)

    ax.set_title(f'Empirical CDFs for i = {i}')
    ax.set_xlabel('z')
    ax.set_ylabel('Empirical CDF')
    ax.legend(fontsize=14)
    ax.grid(True)
    plt.tight_layout()
    ax.tick_params(axis='both', which='major', labelsize=14)
    ax.set_xlabel('z', fontsize=14)
    ax.set_ylabel('Empirical CDF', fontsize=14)
    ax.set_title(f'Empirical CDFs for i = {i}', fontsize=16)
    plt.savefig(f'plots/empirical_cdf_i_{i}.pdf', bbox_inches='tight', dpi=300)
    plt.savefig(f'plots/empirical_cdf_i_{i}.png', bbox_inches='tight', dpi=300)
    plt.show()
    

# Initialize storage for full IDR estimates
idr_surface = []

# For each i = 1 to n
for i in range(1, n + 1):
    hat_F_i = []
    for z in z_vals:
        min_over_k = float('inf')
        for k in range(1, i + 1):
            max_over_j = 0
            for j in range(k - 1, n):
                subseq = y_vals[k - 1:j + 1]
                if len(subseq) > 0:
                    ecdf = np.mean(np.array(subseq) <= z)
                    max_over_j = max(max_over_j, ecdf)
            min_over_k = min(min_over_k, max_over_j)
        hat_F_i.append(min_over_k)
    idr_surface.append(hat_F_i)

idr_surface = np.array(idr_surface)

# Create meshgrid for plotting
Z, I = np.meshgrid(z_vals, np.arange(1, n + 1))
I = I*10

# Surface plot with Plotly
fig_surface = go.Figure(data=[go.Surface(
    x=Z,
    y=I,
    z=idr_surface,
    colorscale='Viridis',
    opacity=0.7,
    contours={
        "z": {"show": True, "start": 0, "end": 1, "size": 0.1}
    }
)])

fig_surface.update_layout(
    title='Isotonic Distributional Regression Surface',
    scene=dict(
        xaxis_title='z / y',
        yaxis_title='x',
        zaxis_title=r'$\hat{F}_i(z)$',
        yaxis=dict(tickvals=I[:,0])
    ),
    autosize=True,
    margin=dict(l=65, r=50, b=65, t=90)
)

fig_surface.add_trace(go.Scatter3d(
    x=[y for y in y_vals],
    y=[x for x in x_vals],
    z=[0]*len(x_vals),  # You can assign a z value if meaningful
    mode='markers',
    marker=dict(size=4, color='red'),
    name='Data Points'
))


fig_surface.show()


idr_surface = []

# For each i = 1 to n
for i in range(1, n + 1):
    hat_F_i = []
    for z in z_vals:
        min_over_k = float('inf')
        for k in range(1, i + 1):
            max_over_j = 0
            for j in range(k - 1, n):
                subseq = y_vals[k - 1:j + 1]
                if len(subseq) > 0:
                    ecdf = np.mean(np.array(subseq) <= z)
                    max_over_j = max(max_over_j, ecdf)
            min_over_k = min(min_over_k, max_over_j)
        hat_F_i.append(min_over_k)
    idr_surface.append(hat_F_i)

idr_surface = np.array(idr_surface)


# Compute IDR surface
idr_surface = []
for i in range(1, n + 1):
    hat_F_i = []
    for z in z_vals:
        min_over_k = float('inf')
        for k in range(1, i + 1):
            max_over_j = 0
            for j in range(k - 1, n):
                subseq = y_vals[k - 1:j + 1]
                if len(subseq) > 0:
                    ecdf = np.mean(np.array(subseq) <= z)
                    max_over_j = max(max_over_j, ecdf)
            min_over_k = min(min_over_k, max_over_j)
        hat_F_i.append(min_over_k)
    idr_surface.append(hat_F_i)

idr_surface = np.array(idr_surface)

#%%
# Plot 2D heatmap
plt.figure(figsize=(10, 6))
extent = [x_vals[0], x_vals[-1], z_vals[0], z_vals[-1]]

interp_factor = 100  # increase resolution by factor of 10 in each dimension
idr_surface_interp = zoom(idr_surface, (interp_factor, interp_factor), order=1)  # order=1 for linear

plt.imshow(idr_surface_interp.T, aspect='auto', origin='lower',
           extent=extent, cmap='grey', interpolation='none')
cbar = plt.colorbar(label=r'$\hat{F}(z)$')
cbar.set_label(r'$\hat{F}(z)$', fontsize=14)
plt.tick_params(axis='both', which='major', labelsize=14)

plt.xlabel('x', fontsize=14)
plt.ylabel('z | y', fontsize=14)
plt.scatter(x_vals, y_vals, color='red', label='Data Points', s=50)
plt.title('Isotonic Distributional Regression Heatmap', fontsize=16)
plt.tight_layout()
plt.savefig('plots/idr_heatmap.png', bbox_inches='tight', dpi=300)
plt.show()

# %%
