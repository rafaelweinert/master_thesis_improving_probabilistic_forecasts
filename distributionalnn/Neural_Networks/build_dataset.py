#%% 

# not relevant for the thesis, but useful to have


import yfinance as yf
import pandas as pd
import time
import os

os.chdir('C:\\Users\\rafaelweinert\\PycharmProjects\\Master_Thesis_Code\\distributionalnn')

#%%
def fetch_stock_data(ticker, start_date, end_date):
    stock = yf.Ticker(ticker)
    data = stock.history(start=start_date, end=end_date)
    return data


# %% OIL, GAS, COAL

'''

Fetch data for the following commodities:
- Oil (Brent Crude)
- Gas (TTF Natural Gas)
- Coal (API2 Coal)

using the yfinance library.

The data is fetched from 2019-01-01 to 2024-12-31 and saved in the Datasets/raw_data folder.
Missing dates are filled using forward fill.

'''


temp_start_date = "2018-12-28"
start_date = "2019-01-01"
end_date = "2025-01-01"

tickers = {
    'OIL' : 'BZ=F', # in USD
    'GAS' : 'TTF=F',
    'COAL' : 'MTF=F', # in USD
}


for key, value in tickers.items():
    data = fetch_stock_data(value, temp_start_date, end_date)
    data.index = data.index.tz_convert('UTC').tz_localize(None)
    data.index = data.index.floor('D')
    if key in ['OIL', 'COAL']:
        currency = fetch_stock_data('EURUSD=X', temp_start_date, end_date)
        currency.index = currency.index.tz_convert('UTC').tz_localize(None)

        currency.index = currency.index.floor('D')
        data = data.merge(currency['Close'], left_index=True, right_index=True, how='left')
        data['Close'] = data['Close_x'] / data['Close_y']
    
    data = data[['Close']]
    data.rename(columns={'Close': key}, inplace=True)
    data.index = pd.to_datetime(data.index)
    data.index = data.index.tz_localize(None)  # Remove timezone
    data = data.asfreq('D').ffill()  # Forward fill missing dates
    data = data.resample('h').ffill()  # Convert to hourly data with forward fill
    

    data = data[data.index >= pd.to_datetime(start_date)]
    
    data.to_csv(f'Datasets/raw_data/{key}.csv')
    
    
# %% EUA 

eua = pd.read_csv("Datasets/raw_data/icap-graph-price-data-2015-01-01-2025-03-12.csv", header=1, index_col=0)
eua = eua[['Primary Market']]
eua.rename(columns={'Primary Market': 'EUA'}, inplace=True)
eua.index = pd.to_datetime(eua.index)
eua = eua.asfreq('D').ffill()  # Forward fill missing dates
eua = eua.resample('h').ffill()  # Convert to hourly data with forward fill
eua.index = eua.index.tz_localize(None)  # Remove timezone
eua = eua[(eua.index >= pd.to_datetime(start_date)) & (eua.index <= pd.to_datetime(end_date))]
eua.to_csv('Datasets/raw_data/EUA.csv')




# %%

energy = pd.read_csv("Datasets/raw_data/ENERGY.csv", index_col=0)
eua = pd.read_csv("Datasets/raw_data/EUA.csv", index_col=0)
oil = pd.read_csv("Datasets/raw_data/OIL.csv", index_col=0)
gas = pd.read_csv("Datasets/raw_data/GAS.csv", index_col=0)
coal = pd.read_csv("Datasets/raw_data/COAL.csv", index_col=0)

data = energy.merge(eua, left_index=True, right_index=True, how='outer')
data = data.merge(coal, left_index=True, right_index=True, how='outer')
data = data.merge(gas, left_index=True, right_index=True, how='outer')
data = data.merge(oil, left_index=True, right_index=True, how='outer')

data.dropna(inplace=True)

data.to_csv('Datasets/DE_new.csv')

# %%
print(energy.shape)
print(eua.shape)
print(oil.shape)
print(gas.shape)
print(coal.shape)
print(data.shape)
# %%


#%% merge the nn parameters to one dictionary and store to json

params = {}

for distribution in ['Point']:
    for number in range(1,5): 
        
        params_df = pd.read_csv(f"Datasets/trial_params/params_{distribution.lower()}{number}.csv")

        # Update param_value for 'CategoricalDistribution' in distribution_json column -> make 0 to 1 and 1 to 0
        params_df.loc[(params_df['distribution_json'].str.contains('CategoricalDistribution')) & (params_df['distribution_json'].str.contains('true, false')), 'param_value'] = \
            params_df.loc[params_df['distribution_json'].str.contains('CategoricalDistribution')& (params_df['distribution_json'].str.contains('true, false')), 'param_value'].apply(lambda x: 1 if x == 0 else 0)

        params[f'params_{distribution.lower()}{number}'] = params_df.set_index('param_name')['param_value'].to_dict()

#save to json
import json
with open('Datasets/trial_params/params.json', 'w') as f:
    json.dump(params, f, indent=4)

