#%%
import pandas as pd

data = pd.read_csv("scores/crps_total.csv")


data['spacer_1'] = ''
data['spacer_2'] = ''
ordered_columns = ['method', 'CRPS', 'spacer_1', 'twcrps_t = 11', 'twcrps_t = 13', 'twcrps_t = 18', 'spacer_2', 'owcrps_t = 11', 'owcrps_t = 13', 'owcrps_t = 18']

data['method'] = data['method'].str.replace('EMOS_SEA', 'EMOS')
data['method'] = data['method'].str.replace('EMOS_BST', 'EMOS-GB')
data['method'] = data['method'].str.replace('MBM_SEA', 'MBM')
data['method'] = data['method'].str.replace('QRF_LOC', 'QRF')

col_mapping = {
    'method': ('', 'Method'),
    'CRPS': ('', 'CRPS'),
    'spacer_1': ('', ''),
    'twcrps_t = 11': ('twCRPS', 't = 11'),
    'twcrps_t = 13': ('twCRPS', 't = 13'),
    'twcrps_t = 18': ('twCRPS', 't = 18'),
    'spacer_2': (' ', ''),
    'owcrps_t = 11': ('owCRPS', 't = 11'),
    'owcrps_t = 13': ('owCRPS', 't = 13'),
    'owcrps_t = 18': ('owCRPS', 't = 18')
}

data = data[ordered_columns]
data.columns = pd.MultiIndex.from_tuples([col_mapping[col] for col in data.columns])
numeric_cols =  [col for col in data.columns if col[0] in ['twCRPS', 'owCRPS']]
# Save to LaTeX

format_dict = {col: "{:.3f}" for col in numeric_cols}
format_dict[data.columns[1]] = "{:.2f}"

numeric_cols = [col for col in data.columns if col[0] in ['twCRPS', 'owCRPS'] or col[1] == 'CRPS']


styled_data = data.style \
    .format(format_dict) \
    .background_gradient(cmap='RdYlGn_r', axis=0, subset=pd.IndexSlice[:, numeric_cols]) \
    .hide(axis="index")


styled_data.to_latex(
    "C:/Users/rafaelweinert/OneDrive/Dokumente/Studium/LMU/Master Thesis/Master_Thesis_Notes/other/tables/crps_wind.tex",
    hrules=True,
    clines="skip-last;data",
    convert_css=True,
    multicol_align='c'
)

styled_data
