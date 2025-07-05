#%%

import pandas as pd
import numpy as np

import os
import re
os.getcwd()



#%%

distributions = [
    'Normal', 
    'JSU', 
    'Point_IDR',
    'Normal_IDR'
    # 'Logistic',
    # 'Gumbel',
    # 'StudentT',
    # 'SinhArcsinh',
    # 'NormalInverseGaussian',
    # 'TwoPieceNormal',
    
    ]

seeds = [
    # 42,
    # 43,
    10
]

folders = []
for distribution in distributions:
    
    path = "Datasets/forecasts/" + distribution.lower() + "/"
    # Get all subfolders in the distribution path
    
    for root, dirs, files in os.walk(path):
        for dir in dirs:
            subfolder_path = os.path.join(root, dir).replace("\\", "/")
            relative_path = subfolder_path.replace(path + "/", "")
            folders.append(relative_path)

print(f"Found {len(folders)} subfolders")
for folder in folders:
    print(f"  - {folder}")








# %%

wcrps_complete = pd.DataFrame()
for folder in folders:
    # Check if wcrps_values.csv exists in the folder
    csv_path = folder + "/wcrps_values.csv"#path + "/" + 
    if not os.path.isfile(csv_path):
        print(f"Warning: wcrps_values.csv does not exist in {folder}, skipping this folder")
        continue
    wcrps_values = pd.read_csv(csv_path)
    wcrps_values['path'] = folder
    wcrps_complete = pd.concat([wcrps_complete, wcrps_values], ignore_index=True)
    
    # Extract the seed from the path using regex
wcrps_complete['seed'] = wcrps_complete['path'].str.extract(r'seed_(\d+)', expand=False).astype(int)

wcrps_complete['model'] = wcrps_complete['path'].str.extract(r'/(jsu|normal|point_idr|normal_idr)/', expand=False)
wcrps_complete['model'] = wcrps_complete['model'].str.upper()
wcrps_complete['model'] = wcrps_complete['model'].str.replace('_', ' + ')
wcrps_complete.drop(columns=['path', 'seed'], axis = 0, inplace=True)
#%%
# Get the OWCRPS values for jsu_rs_LOG_PROB
original_values = wcrps_complete[wcrps_complete['path'].str.contains('jsu/LOG_PROB')]


# %%
    
crps = wcrps_complete.copy()
crps = crps[crps['percentile'] == 0]
crps = crps[['model', 'twcrps']]
crps = crps.rename(columns={'twcrps': 'CRPS'})
crps.sort_values(by='CRPS', ascending=True, inplace=True)

crps.to_latex("C:/Users/rafaelweinert/OneDrive/Dokumente/Studium/LMU/Master Thesis/Master_Thesis_Notes/other/tables/crps.tex", index=False, float_format="%.2f", escape=False)


# %%

# Copy and filter the original data
twcrps = wcrps_complete.copy()
twcrps = twcrps[(twcrps['percentile'] != 0) & (twcrps['percentile'] != 1)]
twcrps = twcrps.rename(columns={'twcrps': 'twCRPS'})

# Pivot the data into wide format
twcrps_new = pd.DataFrame()
for t, p in zip(twcrps['percentile_value'].unique(), twcrps['percentile'].unique()):
    twcrps_t = twcrps[twcrps['percentile_value'] == t].copy()
    twcrps_t = twcrps_t[['model', 'twCRPS']].rename(columns={'twCRPS': f"{p}"})
    if twcrps_new.empty:
        twcrps_new = twcrps_t
    else:
        twcrps_new = pd.merge(twcrps_new, twcrps_t, on='model')

# Create MultiIndex for column headers
upper_cols = ['0.9', '0.95', '0.975', '0.99']
lower_cols = ['0.1', '0.05', '0.025', '0.01']

# Create column mapping
col_mapping = {}
for col in twcrps_new.columns:
    if col == 'model':
        col_mapping[col] = ('', col)
    elif col in upper_cols:
        col_mapping[col] = ('upper tail', col)
    elif col in lower_cols:
        col_mapping[col] = ('lower tail', col)
    else:
        col_mapping[col] = ('other', col)

# Reorder columns: model, upper, spacer, lower
ordered_cols = ['model'] + upper_cols + ['__spacer__'] + lower_cols
twcrps_new['__spacer__'] = ' '  # create spacer column
twcrps_new = twcrps_new[ordered_cols]

# Apply updated col_mapping including spacer
col_mapping['__spacer__'] = ('', '')  # no label for spacer
twcrps_new.columns = pd.MultiIndex.from_tuples([col_mapping[col] for col in twcrps_new.columns])

# Identify numeric columns for styling
numeric_cols = [col for col in twcrps_new.columns if col[0] in ['upper tail', 'lower tail']]
format_dict = {col: "{:.3f}" for col in numeric_cols}

styled_twcrps = twcrps_new.style \
    .format(format_dict) \
    .background_gradient(cmap='RdYlGn_r', axis=0, subset=pd.IndexSlice[:, numeric_cols]) \
    .hide(axis="index")

styled_twcrps.to_latex(
    "C:/Users/rafaelweinert/OneDrive/Dokumente/Studium/LMU/Master Thesis/Master_Thesis_Notes/other/tables/twcrps.tex",
    hrules=True,
    clines="skip-last;data",
    convert_css=True,
    multicol_align='c'
)

# %%

owcrps = wcrps_complete.copy()
owcrps = owcrps[(owcrps['percentile'] != 0) & (owcrps['percentile'] != 1)]
owcrps = owcrps.rename(columns={'owcrps': 'owCRPS'})

# Pivot the data into wide format
twcrps_new = pd.DataFrame()
for t, p in zip(owcrps['percentile_value'].unique(), owcrps['percentile'].unique()):
    twcrps_t = owcrps[owcrps['percentile_value'] == t].copy()
    twcrps_t = twcrps_t[['model', 'owCRPS']].rename(columns={'owCRPS': f"{p}"})
    if twcrps_new.empty:
        twcrps_new = twcrps_t
    else:
        twcrps_new = pd.merge(twcrps_new, twcrps_t, on='model')

# Create MultiIndex for column headers
upper_cols = ['0.9', '0.95', '0.975', '0.99']
lower_cols = ['0.1', '0.05', '0.025', '0.01']

# Create column mapping
col_mapping = {}
for col in twcrps_new.columns:
    if col == 'model':
        col_mapping[col] = ('', col)
    elif col in upper_cols:
        col_mapping[col] = ('upper tail', col)
    elif col in lower_cols:
        col_mapping[col] = ('lower tail', col)
    else:
        col_mapping[col] = ('other', col)

# Reorder columns: model, upper, spacer, lower
ordered_cols = ['model'] + upper_cols + ['__spacer__'] + lower_cols
twcrps_new['__spacer__'] = ' '  # create spacer column
twcrps_new = twcrps_new[ordered_cols]

# Apply updated col_mapping including spacer
col_mapping['__spacer__'] = ('', '')  # no label for spacer
twcrps_new.columns = pd.MultiIndex.from_tuples([col_mapping[col] for col in twcrps_new.columns])

# Identify numeric columns for styling
numeric_cols = [col for col in twcrps_new.columns if col[0] in ['upper tail', 'lower tail']]
format_dict = {col: "{:.3f}" for col in numeric_cols}

styled_twcrps = twcrps_new.style \
    .format(format_dict) \
    .background_gradient(cmap='RdYlGn_r', axis=0, subset=pd.IndexSlice[:, numeric_cols]) \
    .hide(axis="index")

styled_twcrps.to_latex(
    "C:/Users/rafaelweinert/OneDrive/Dokumente/Studium/LMU/Master Thesis/Master_Thesis_Notes/other/tables/owcrps.tex",
    hrules=True,
    clines="skip-last;data",
    convert_css=True,
    multicol_align='c'
)
# %%
