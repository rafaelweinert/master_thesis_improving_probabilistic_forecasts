# %% 
import os
os.chdir(r"C:\Users\rafaelweinert\PycharmProjects\Master_Thesis_Code\distributionalnn")
print(os.getcwd())
import pandas as pd
import numpy as np
import tensorflow as tf
import tensorflow.keras as keras
import tensorflow_probability as tfp  
from tensorflow_probability import distributions as tfd
from tensorflow_probability import math as tfm

from datetime import datetime

from sklearn.model_selection import train_test_split

import json
import subprocess

from sklearn.model_selection import StratifiedKFold

# Force reloading the module to get any updates
import importlib
from Neural_Networks import utils
importlib.reload(utils)
from Neural_Networks.utils import *
import time
import shutil
import isodisreg
from isodisreg import idr


print("Num GPUs Available:", len(tf.config.list_physical_devices('GPU')))


#%% settings

losses = [
    # 'wCRPS',
    # 'wCRPS_negative_price',
    'LOG_PROB',
    # 'CRPS',
    # # 'twCRPS',
    # 'LOG_PROB_reg_output',
    # # 'LOG_PROB_twCRPS_lower',
    # 'CRPS_reg_output',
    # 'MAE',
    ]
distributions = [
    'Point',
    'Normal', 
    'JSU', 
    # 'Logistic',
    # 'Gumbel',
    # 'StudentT',
    # 'SinhArcsinh',
    # 'NormalInverseGaussian',
    # 'TwoPieceNormal',
    
    ] 
random_splits = [
    True
    ] # take a random split for train and test or take the first years as train and the last year as test
new_datasets = [
    False, 
    # True 
    ] # take the new dataset (2019-2024) or the old dataset (2015-2019)

seeds = [
    # 42, 
    # 43,
    10
    ]

include_weeks = [
    #True, 
    False
    ] # include the week of the year as a feature

#%% train models
check_existing = True
include_difference = False 

transform_y = False
sin_cos_week = False

IDR = True

all_params = json.load(open('Datasets/trial_params/params.json', 'r'))
for seed in seeds:
    for random_split in random_splits:
        for new_dataset in new_datasets:
            for distribution in distributions:
                for loss in losses:
                    for include_week in include_weeks:
                    
                        test_pred = []
                        train_pred = []
                        
                        idr_train_df = pd.DataFrame()
                        splits = 3
                        
                        for number in range(1,5): 
                            
                            params = all_params[f'params_{distribution.lower()}{number}']                
                            
                            if include_difference:
                                params['Difference'] = 1
                            if include_week:
                                params['Week'] = 1
                            if transform_y:
                                params['transform_y'] = 1
                            if sin_cos_week:
                                params['sin_cos_week'] = 1
                            
                            data = pd.read_csv(f'./Datasets/DE{"_new" if new_dataset else ""}.csv', index_col=0)
                            data.index = [datetime.strptime(e, '%Y-%m-%d %H:%M:%S') for e in data.index]
                            data.columns = ['Price', 'Load_Forecast', 'Renewables_Forecast', 'EUA', 'COAL', 'GAS', 'OIL']

                            X, Y = build_data(data, params)
                            
                            test_pred_i = pd.DataFrame()
                            train_pred_i = pd.DataFrame()
                            
                        
                            # Create price-based strata for stratified split
                            # Use the mean price of each day as the stratification feature
                            mean_prices = np.mean(Y, axis=1)
                            
                            q = 10
                            strat_labels = pd.qcut(mean_prices, q=q, labels=False)
                            skf = StratifiedKFold(n_splits=splits, shuffle=True, random_state=seed)
                            fold = 0
                            for temp_indices, test_indices in skf.split(X, strat_labels):
                                train_indices, val_indices = train_test_split(
                                    temp_indices, test_size=1/8, random_state=seed, stratify=strat_labels[temp_indices])

                                X_train, Y_train = X[train_indices], Y[train_indices]
                                X_val, Y_val = X[val_indices], Y[val_indices]
                                X_test, Y_test = X[test_indices], Y[test_indices]
                            
                                try:
                                    print(f'Seed: {seed}, Distribution: {distribution}, Loss: {loss}, Random Split: {random_split}, New Dataset: {new_dataset}, Number: {number}')
                                    
                                    kwargs = {}
                                    if loss == 'twCRPS':
                                        threshold = np.percentile(Y_train, 90)
                                        kwargs['threshold'] = threshold
                                        
                                    if loss == 'LOG_PROB_twCRPS_lower':
                                        kwargs['threshold'] = 0
                                        
                                    if loss == 'LOG_PROB_reg_output' or loss == 'CRPS_reg_output':
                                        kwargs['scale_reg'] = 0.002
                                        kwargs['tailweight_reg'] = 0.002
                                        kwargs['skewness_reg_pos'] = 0.1
                                        kwargs['skewness_reg_neg'] = 0.05 
                                        
                                    if loss == 'wCRPS_negative_price':
                                        kwargs['negative_price_weight'] = 100.0
                                                                        
                                    tf.random.set_seed(seed)
                                    np.random.seed(seed)
                                    
                                    model = build_model(params = params, distribution = distribution, loss = loss, input_shape = (X.shape[1],), y_complete = Y_train, **kwargs)
                                    callbacks = [keras.callbacks.EarlyStopping(patience=50, restore_best_weights=True), BatchProgressLogger(update_freq=100)]
                                    
                                    lr_callback = None
                                    if distribution == 'Gumbel':               
                                        params['learning_rate'] = 0.00001 #FIX
                                        
                                    def step_decay(epoch):
                                        drop_rate = 0.95  # Reduce by 5%
                                        epochs_drop = 5  # Drop LR every 5 epochs
                                        lr = tf.keras.backend.get_value(model.optimizer.lr)  # Get current LR
                                        new_lr = lr * drop_rate if (epoch + 1) % epochs_drop == 0 else lr
                                        print(f"Epoch {epoch+1}: Learning rate = {new_lr:.6f}")
                                        return tf.math.maximum(new_lr, params['learning_rate']) #FIX
                                    lr_callback = tf.keras.callbacks.LearningRateScheduler(step_decay)
                                    
                                    model.fit(X_train, Y_train, validation_data=(X_val, Y_val), epochs=1500, batch_size=32, verbose=False, callbacks=callbacks) #FIX change back to epochs 1500, lr_callback, [callbacks, BatchProgressLogger(update_freq=100)]
                                
                                    if IDR:
                                        train_dist = model(X_train)                                        
                                        test_dist = model(X_test)
                                        
                                        if distribution == 'Point':
                                            train_pred_i = pd.concat([train_pred_i, pd.DataFrame({
                                                'y_true': Y_train.flatten(),
                                                'loc' : train_dist.numpy().flatten(),
                                                'fold' : fold,
                                                'number': number,
                                            })], ignore_index=True)
                                            test_pred_i = pd.concat([test_pred_i, pd.DataFrame({
                                                'y_true': Y_test.flatten(),
                                                'loc' : test_dist.numpy().flatten(),
                                                'fold' : fold,
                                                'number': number,
                                            })], ignore_index=True)
                                            
                                        else:
                                            train_pred_i = pd.concat([train_pred_i, pd.DataFrame({
                                                'y_true': Y_train.flatten(),
                                                'loc': train_dist.loc.numpy().flatten(),
                                                'scale': train_dist.scale.numpy().flatten(),
                                                'fold': fold,
                                                'number': number,

                                            })], ignore_index=True)
                                            
                                            test_pred_i = pd.concat([test_pred_i, pd.DataFrame({
                                                'y_true': Y_test.flatten(),
                                                'loc': test_dist.loc.numpy().flatten(), 
                                                'scale': test_dist.scale.numpy().flatten(), 
                                                'fold': fold,
                                                'number': number,
                                                
                                            })], ignore_index=True)
                                        

                                    else:

                                        dist = model(X_test)
                                        
                                        results_df = pd.DataFrame({
                                            'y_true': Y_test.flatten(),
                                            'loc': dist.loc.numpy().flatten(), 
                                            'scale': dist.scale.numpy().flatten(), 
                                        })
                                        
                                        if distribution == 'StudentT':
                                            results_df['df'] = dist.df.numpy().flatten()    
                                        if distribution in ['GEV']:
                                            results_df['concentration'] = dist.concentration.numpy().flatten()     
                                        if distribution in ['JSU', 'SinhArcsinh', 'NormalInverseGaussian']:
                                            results_df['tailweight'] = dist.tailweight.numpy().flatten()
                                            results_df['skewness'] = dist.skewness.numpy().flatten()                                            
                                        test_pred_i = pd.concat([test_pred_i, results_df], ignore_index=True)
                                                                            
                                        train_dist = model(X_train)
                                        results_train_df = pd.DataFrame({
                                            'y_true': Y_train.flatten(),
                                            'loc': train_dist.loc.numpy().flatten(),
                                            'scale': train_dist.scale.numpy().flatten(),
                                        })
                                        
                                        if distribution == 'StudentT':
                                            results_train_df['df'] = train_dist.df.numpy().flatten()
                                        if distribution in ['GEV']:
                                            results_train_df['concentration'] = train_dist.concentration.numpy().flatten()
                                        if distribution in ['JSU', 'SinhArcsinh', 'NormalInverseGaussian']:
                                            results_train_df['tailweight'] = train_dist.tailweight.numpy().flatten()
                                            results_train_df['skewness'] = train_dist.skewness.numpy().flatten()
                                        
                                        train_pred_i = pd.concat([train_pred_i, results_train_df], ignore_index=True)                                
                                    
                                except Exception as e:
                                    print(e)
                                    print(f'Error in {distribution} {number}')
                                
                                fold += 1
                                
                            test_pred.append(test_pred_i)
                            train_pred.append(train_pred_i)   
                        
                        try:
                            if IDR:
                                print('Performing IDR')
                                train_pred_df = pd.concat(train_pred, ignore_index=False)
                                test_pred_df = pd.concat(test_pred, ignore_index=False)
                                combined_mean = pd.DataFrame()
                                
                                for i in range(splits):
                                    
                                    def format_df_helper(df, i):
                                        df_i = df[df['fold'] == i]
                                        #df_i = df_i.reset_index(drop=True)
                                        if 'scale' in df_i.columns:
                                            df_i.drop(columns=['scale'], inplace=True)
                                        df_i = df_i.pivot(columns='number', values=['y_true', 'loc'])
                                        df_i.columns = [f'{col[0]}_{col[1]}' for col in df_i.columns]
                                        df_i.rename(columns={'y_true_1': 'y_true'}, inplace=True)
                                        varNames = ['loc_1', 'loc_2', 'loc_3', 'loc_4']
                                        loc_means = df_i[varNames].mean(axis=1)
                                        df_i['loc'] = loc_means
                                        return df_i[['y_true', 'loc']]
                                    
                                    train_pred_df_i = format_df_helper(train_pred_df, i)
                                    test_pred_df_i = format_df_helper(test_pred_df, i)
                                                                                                        
                                    groups = {'loc': 1}
                                    orders = {'1': 'icx'}
                                    fit = idr(y = train_pred_df_i['y_true'], X = train_pred_df_i[['loc']], groups = groups, orders = orders)
                                    predictions = fit.predict(test_pred_df_i[['loc']])
                                    
                                    points = pd.DataFrame([predictions.predictions[j].points for j in range(len(predictions.predictions))])
                                    ecdf = pd.DataFrame([predictions.predictions[j].ecdf for j in range(len(predictions.predictions))])
                                    
                                    points['type'] = 'points'
                                    points['y_true'] = test_pred_df_i['y_true'].values
                                    ecdf['type'] = 'ecdf'
                                    ecdf['y_true'] = test_pred_df_i['y_true'].values
                                    
                                    points.reset_index(inplace=True)
                                    ecdf.reset_index(inplace=True)
                                    
                                    merged = pd.concat([points, ecdf], axis=0)
                                    merged['fold'] = i
                                    
                                    combined_mean = pd.concat([combined_mean, merged], axis=0)
                                                
                            else:
                                # First take the mean of all parameters as before
                                combined_mean = pd.concat(test_pred).groupby(level=0).mean()

                                # Now add the original parameters with numerical suffixes
                                for i, df in enumerate(test_pred, 1):
                                    # Exclude y_true when creating renamed columns
                                    columns_to_rename = [col for col in df.columns if col != 'y_true']
                                    renamed_columns = {col: f"{col}_{i}" for col in columns_to_rename}
                                    
                                    # Create a new dataframe with renamed columns (excluding y_true)
                                    df_renamed = df[columns_to_rename].rename(columns=renamed_columns)
                                    
                                    # Add the renamed columns to the combined dataframe
                                    combined_mean = pd.concat([combined_mean, df_renamed], axis=1)
                                    
                            combined_mean = combined_mean.reset_index(drop=True)
                                                                                    
                            timestamp = time.strftime("%m%d_%H%M")
                            path = f'Datasets/forecasts/{distribution.lower()}{("_idr" if IDR else "")}/{loss}/seed_{seed}/paper/'
                                                                        
                            os.makedirs(path, exist_ok=True)
                            
                            combined_mean.to_csv(path + 'forecast_test.csv', index=False)
                            
                            metadata = {
                                'distribution': distribution,
                                'loss_function': loss,
                                'random_split': random_split,
                                'new_dataset': new_dataset,
                                'include_week': include_week,
                                'include_difference': include_difference,
                                'seed': seed,
                                'sin_cos_week': sin_cos_week,
                                'kwargs': ''.join([f"{key}-{value}--" for key, value in kwargs.items()]),
                                'params': ''.join([f"{key}-{value}--" for key, value in params.items()]),
                                'IDR': IDR,
                            }
                            
                            with open(path + 'metadata.json', 'w') as f:
                                json.dump(metadata, f)
                            
                            if not IDR:
                                combined_mean_train = pd.concat(train_pred).groupby(level=0).mean()
                                for i, df in enumerate(train_pred, 1):
                                    # Exclude y_true when creating renamed columns
                                    columns_to_rename = [col for col in df.columns if col != 'y_true']
                                    renamed_columns = {col: f"{col}_{i}" for col in columns_to_rename}
                                    
                                    # Create a new dataframe with renamed columns (excluding y_true)
                                    df_renamed = df[columns_to_rename].rename(columns=renamed_columns)
                                    
                                    # Add the renamed columns to the combined dataframe
                                    combined_mean_train = pd.concat([combined_mean_train, df_renamed], axis=1)
                                    
                                combined_mean_train = combined_mean_train.reset_index(drop=True)
                                combined_mean_train.to_csv(path + 'forecast_train.csv', index=False)



                        except:
                            print(f'Error in {distribution} {loss} {random_split} {new_dataset}')               
                                    
                        
                    
            
            


# %%
