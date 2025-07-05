import numpy as np
import pandas as pd
import tensorflow as tf
import tensorflow.keras as keras
import tensorflow_probability as tfp  
from tensorflow_probability import distributions as tfd
# from tensorflow_probability import bijectors as tfb
from tensorflow_probability import math as tfm
from tensorflow.keras.callbacks import Callback


class BatchProgressLogger(Callback):
    def __init__(self, update_freq=100):
        super().__init__()
        self.update_freq = update_freq

    def on_epoch_end(self, epoch, logs=None):
        if epoch % self.update_freq == 0:
            print(f"Epoch {epoch}: loss = {logs['loss']:.4f}")


def safe_log_prob(y, rv_y, epsilon=1e-8):
    prob = rv_y.prob(y)
    prob = tf.maximum(prob, epsilon)  # Avoid zero probabilities
    return rv_y.log_prob(y)  # Apply the log_prob with safe probabilities

def twcrps_loss(y, rv_y, threshold, num_points=1000):
    """Computes the threshold-weighted CRPS loss separately for each of the 24 distributions."""

    # Define integration range per hour
    x_min = tf.reduce_min(y, axis=0, keepdims=True) - 3 * tf.math.reduce_std(y, axis=0, keepdims=True)
    x_max = tf.reduce_max(y, axis=0, keepdims=True) + 3 * tf.math.reduce_std(y, axis=0, keepdims=True)

    # Generate points for numerical integration
    x_vals = tf.linspace(x_min, x_max, num_points)  # Shape: (num_points, 24)
    
    # Compute CDF for each hour
    #F_x = rv_y.cdf(tf.cast(x_vals, tf.float32))  # Shape: (num_points, batch_size, 24)
    F_x = rv_y.cdf(x_vals)  # Shape: (num_points, batch_size, 24)
    # Compute indicator function 1(x ≥ y)
    indicator = tf.cast(x_vals >= y[tf.newaxis, ...], dtype=tf.float32)      
    # Compute squared difference (F(x) - 1(x ≥ y))²
    integrand = tf.square(F_x - indicator) * tf.cast(x_vals <= threshold, dtype=tf.float32)
        
    # Perform numerical integration using trapezoidal rule per hour
    crps_per_hour = tfm.trapz(integrand, x=x_vals, axis=0)  # Shape: (batch_size, 24)
    
    # Aggregate loss: Sum or mean over hours
    return tf.reduce_mean(crps_per_hour)  # Use tf.reduce_sum if you prefer sum over mean

def crps_loss(y, rv_y, y_complete=None, num_points=500, weights=False):
    batch_size = tf.shape(y)[0]
    num_hours = tf.shape(y)[1]

    y_complete = tf.cast(y_complete, tf.float32)
    y_min = tf.reduce_min(y_complete, axis=0, keepdims=True) - 3 * tf.math.reduce_std(y_complete, axis=0, keepdims=True)
    y_max = tf.reduce_max(y_complete, axis=0, keepdims=True) + 3 * tf.math.reduce_std(y_complete, axis=0, keepdims=True)

    x_vals = tf.linspace(y_min, y_max, num_points)
    x_vals = tf.broadcast_to(x_vals, [num_points, batch_size, num_hours])

    F_x = rv_y.cdf(x_vals)
    indicator = tf.cast(x_vals >= y[tf.newaxis, ...], dtype=tf.float32)

    if not weights:
        integrand = tf.square(F_x - indicator)
    else:
        lower_bound = tfp.stats.percentile(y_complete, 10.0, axis=0, interpolation='linear', keepdims=True)
        upper_bound = tfp.stats.percentile(y_complete, 90.0, axis=0, interpolation='linear', keepdims=True)
        weights = tf.where((x_vals < lower_bound) | (x_vals > upper_bound), 1.0, 0.5)
        integrand = tf.square(F_x - indicator) * weights

    crps_per_hour = tfm.trapz(integrand, x=x_vals, axis=0)
    return tf.reduce_mean(crps_per_hour)


def wcrps_negative_price_loss(y, rv_y, num_points=500, y_complete=None, negative_price_weight=100):
    batch_size = tf.shape(y)[0]
    num_hours = tf.shape(y)[1]

    y_complete = tf.cast(y_complete, tf.float32)
    y_min = tf.reduce_min(y_complete, axis=0, keepdims=True) - 3 * tf.math.reduce_std(y_complete, axis=0, keepdims=True)
    y_max = tf.reduce_max(y_complete, axis=0, keepdims=True) + 3 * tf.math.reduce_std(y_complete, axis=0, keepdims=True)

    x_vals = tf.linspace(y_min, y_max, num_points)
    x_vals = tf.broadcast_to(x_vals, [num_points, batch_size, num_hours])

    F_x = rv_y.cdf(x_vals)
    indicator = tf.cast(x_vals >= y[tf.newaxis, ...], dtype=tf.float32)
    
    weights = tf.where(x_vals < 5, negative_price_weight, 1) # IDEA: x=1, y=0.3 changes the tail behaviour a little bit -> most predictions will not lie in the tails though
    # Compute squared difference (F(x) - 1(x ≥ y))²
       
    integrand = tf.square(F_x - indicator) * weights
    
    # Perform numerical integration using trapezoidal rule per hour
    crps_per_hour = tfm.trapz(integrand, x=x_vals, axis=0)  # Shape: (batch_size, 24)
    
    # Aggregate loss: Sum or mean over hours
    return tf.reduce_mean(crps_per_hour)  # Use tf.reduce_sum if you prefer sum over mean
    
def regularize_output(rv_y, **kwargs):
    regularization = 0.0
    if hasattr(rv_y, 'scale'):
        regularization += tf.reduce_mean(tf.square(rv_y.scale)) * kwargs.get('scale_reg', 0)
    if hasattr(rv_y, 'tailweight'):
       regularization -= tf.reduce_mean(tf.square(rv_y.tailweight)) * kwargs.get('tailweight_reg', 0) # higher tailweight is more precise --> better
    if hasattr(rv_y, 'skewness'):
        # Penalize only positive skewness values
        positive_skewness = tf.maximum(rv_y.skewness, 0)
        regularization += tf.reduce_mean(tf.square(positive_skewness)) * kwargs.get('skewness_reg_pos', 0) # higher skewness is more precise --> better
        negative_skewness = tf.minimum(rv_y.skewness, 0)
        regularization += tf.reduce_mean(tf.square(negative_skewness)) * kwargs.get('skewness_reg_neg', 0) # higher skewness is more precise --> better

    return regularization

    
def build_data(data, params):
    
    if params.get('transform_y', 0) == 1:
        data['Price'] = np.arcsinh((data['Price']-np.median(data['Price']))/20)
    
    
    data['day'] = data.index.date
    data['hour'] = data.index.hour
    data['Price_D-1'] = data['Price'].shift(24)
    data['Price_D-2'] = data['Price'].shift(48)
    data['Price_D-3'] = data['Price'].shift(72)
    data['Price_D-7'] = data['Price'].shift(168)

    data['Load_Forecast_D-1'] = data['Load_Forecast'].shift(24)
    data['Load_Forecast_D-7'] = data['Load_Forecast'].shift(168)

    data['Renewables_Forecast_D-1'] = data['Renewables_Forecast'].shift(24)

    data['EUA_D-2'] = data['EUA'].shift(48)
    data['COAL_D-2'] = data['COAL'].shift(48)
    data['GAS_D-2'] = data['GAS'].shift(48)
    data['OIL_D-2'] = data['OIL'].shift(48)
    
    #FIX
    data['Difference'] = data['Load_Forecast'] - data['Renewables_Forecast']

    data_wide = data.pivot_table(index='day', columns='hour')
    data_wide.columns = [f"{col}_{hour}" for col, hour in data_wide.columns]
    data_wide.index = pd.to_datetime(data_wide.index)
    data_wide['Weekday'] = data_wide.index.weekday
    data_wide['Week'] = data_wide.index.isocalendar().week.astype('int32')
    data_wide['sin_week'] = np.sin(2 * np.pi * data_wide['Week'] / 52.0)
    data_wide['cos_week'] = np.cos(2 * np.pi * data_wide['Week'] / 52.0)

    data_wide.dropna(inplace=True)    
            
    Y = data_wide[[f'Price_{hour}' for hour in range(24)]].values
    X_cols = []

    if params['price_D-1']:
        X_cols += [f'Price_D-1_{hour}' for hour in range(24)]
    if params['price_D-2']:
        X_cols += [f'Price_D-2_{hour}' for hour in range(24)]
    if params['price_D-3']:
        X_cols += [f'Price_D-3_{hour}' for hour in range(24)]
    if params['price_D-7']:
        X_cols += [f'Price_D-7_{hour}' for hour in range(24)]
    if params['load_D']:
        X_cols += [f'Load_Forecast_{hour}' for hour in range(24)]
    if params['load_D-1']:
        X_cols += [f'Load_Forecast_D-1_{hour}' for hour in range(24)]
    if params['load_D-7']:
        X_cols += [f'Load_Forecast_D-7_{hour}' for hour in range(24)]
    if params['RES_D']:
        X_cols += [f'Renewables_Forecast_{hour}' for hour in range(24)]
    if params['RES_D-1']:
        X_cols += [f'Renewables_Forecast_D-1_{hour}' for hour in range(24)]
    if params['EUA']:
        X_cols += [f'EUA_D-2_{hour}' for hour in range(24)]
    if params['Coal']:
        X_cols += [f'COAL_D-2_{hour}' for hour in range(24)]
    if params['Gas']:
        X_cols += [f'GAS_D-2_{hour}' for hour in range(24)]
    if params['Oil']:
        X_cols += [f'OIL_D-2_{hour}' for hour in range(24)]
    if params['Dummy']:
        X_cols += ['Weekday']
    if params.get('Week', 0) == 1:
        if not params['Dummy']:
            X_cols += ['Weekday']
        X_cols += ['Week']
    if params.get('sin_cos_week', 0) == 1:
        if not params['Dummy']:
            if 'Weekday' not in X_cols:
                X_cols += ['Weekday']
        X_cols += ['sin_week', 'cos_week']
        # Remove 'Week' from X_cols if it exists
        if 'Week' in X_cols:
            X_cols.remove('Week')
        
        
    #FIX
    if params.get('Difference', 0) == 1:
        X_cols += [f'Difference_{hour}' for hour in range(24)]

        
    X = data_wide[X_cols].values
    
    return X, Y

def build_model(params, distribution, loss, input_shape, y_complete = None, **kwargs):
    
    activations = ['sigmoid', 'relu', 'elu', 'tanh', 'softplus', 'softmax']
    paramcount = {'Normal': 2,
              'StudentT': 3,
              'JSU': 4,
              'SinhArcsinh': 4,
              'NormalInverseGaussian': 4,
              'GEV': 3,
              'Gumbel': 2,
              'Gamma': 2,
              'Logistic': 2,   
              'TwoPieceNormal': 3,          
              'Point': None         
              }
    # Build the best model with the best parameters
    inputs = keras.Input(shape=input_shape)
    last_layer = keras.layers.BatchNormalization()(inputs)

    if params['dropout']:
        drop = keras.layers.Dropout(params['dropout_rate'])(last_layer)
        last_layer = drop

    h1_activation_rate = params['h1_activation_rate_l1'] if params['regularize_h1_activation'] else 0.0
    h1_kernel_rate = params['h1_kernel_rate_l1'] if params['regularize_h1_kernel'] else 0.0
    hidden = keras.layers.Dense(params['neurons_1'],
                                activation=activations[int(params['activation_1'])],
                                kernel_regularizer=keras.regularizers.L1(h1_kernel_rate),
                                activity_regularizer=keras.regularizers.L1(h1_activation_rate))(last_layer)
    tf.debugging.check_numerics(hidden, "hidden layer 1 contains NaNs!")

    h2_activation_rate = params['h2_activation_rate_l1'] if params['regularize_h2_activation'] else 0.0
    h2_kernel_rate = params['h2_kernel_rate_l1'] if params['regularize_h2_kernel'] else 0.0
    hidden = keras.layers.Dense(params['neurons_2'],
                                activation=activations[int(params['activation_2'])],
                                kernel_regularizer=keras.regularizers.L1(h2_kernel_rate),
                                activity_regularizer=keras.regularizers.L1(h2_activation_rate))(hidden)
    tf.debugging.check_numerics(hidden, "hidden layer 2 contains NaNs!")


    param_layers = []
    param_names = ["loc", "scale", "tailweight", "skewness"]
    
    if distribution == 'Point':
        outputs = keras.layers.Dense(24, activation='linear')(hidden)
    else:
        for p in range(paramcount[distribution]):
            param_kernel_rate = 0
            param_layers.append(keras.layers.Dense(
                24, activation='linear',
                kernel_regularizer=keras.regularizers.L1(param_kernel_rate))(hidden))
        linear = keras.layers.concatenate(param_layers)
        tf.debugging.check_numerics(linear, "linear layer output contains NaNs!")
        
        if distribution == 'Normal':
            outputs = tfp.layers.DistributionLambda(
                lambda t: tfd.Normal(
                    loc=t[..., :24],
                    scale=1e-3 + 3 * tf.nn.softplus(t[..., 24:])))(linear)
        elif distribution == 'StudentT':
            outputs = tfp.layers.DistributionLambda(
                lambda t: tfd.StudentT(
                    loc=t[..., :24],
                    scale=1e-3 + 3 * tf.nn.softplus(t[..., 24:48]),
                    df=1 + 3 * tf.nn.softplus(t[..., 48:])))(linear)
        elif distribution == 'JSU':
            outputs = tfp.layers.DistributionLambda(
                lambda t: tfd.JohnsonSU(
                    loc=t[..., :24],
                    scale=1e-3 + 3 * tf.nn.softplus(t[..., 24:48]),
                    tailweight= 1 +  3 * tf.nn.softplus(t[..., 48:72]),
                    skewness=t[..., 72:]))(linear)
        elif distribution == 'SinhArcsinh':
            outputs = tfp.layers.DistributionLambda(
                lambda t: tfd.SinhArcsinh(
                    loc=t[..., :24],
                    scale=1e-3 + 3 * tf.nn.softplus(t[..., 24:48]),
                    tailweight=1e-3 + 3 * tf.nn.softplus(t[..., 48:72]),
                    skewness=t[..., 72:]))(linear)
        elif distribution == 'NormalInverseGaussian':
            outputs = tfp.layers.DistributionLambda(
                lambda t: tfd.NormalInverseGaussian(
                    loc=t[..., :24],
                    scale=1e-3 + 3 * tf.nn.softplus(t[..., 24:48]),
                    tailweight=1e-3 + 3 * tf.nn.softplus(t[..., 48:72]),
                    skewness=(1e-3 + tf.nn.softplus(t[..., 72:]))*tf.tanh(t[..., 72:])))(linear)          
        elif distribution == 'Gumbel':
            outputs = tfp.layers.DistributionLambda(
                lambda t: tfd.Gumbel(
                    loc=tf.clip_by_value(t[..., :24], -30, 500.0),
                    scale = 2 + 2*tf.nn.softplus(t[..., 24:48])))(linear) 
        elif distribution == 'Logistic':
            outputs = tfp.layers.DistributionLambda(
                lambda t: tfd.Logistic(
                    loc=t[..., :24],
                    scale=1e-3 + 3 * tf.nn.softplus(t[..., 24:48])))(linear)
        elif distribution == 'TwoPieceNormal':
            outputs = tfp.layers.DistributionLambda(
                lambda t: tfd.TwoPieceNormal(
                    loc=t[..., :24],
                    scale=1e-3 + 3 * tf.nn.softplus(t[..., 24:48]),
                    skewness = 1e-3 + 3 * tf.nn.softplus(t[..., 48:72])))(linear)

        else:
            raise ValueError(f'Incorrect distribution {distribution}')
    
    model = keras.Model(inputs=inputs, outputs=outputs)
    
    if loss == 'MAE': loss_function = 'mae'
    if loss == 'MSE': loss_function = 'mse'
    if loss == 'LOG_PROB_reg_output': loss_function = lambda y, rv_y: -rv_y.log_prob(y) + regularize_output(rv_y)
    if loss == 'LOG_PROB': loss_function = lambda y, rv_y: -rv_y.log_prob(y)
    if loss == 'LOG_PROB_twCRPS_lower': loss_function = lambda y, rv_y: -rv_y.log_prob(y) + twcrps_loss(y, rv_y, kwargs['threshold'])
    if loss == 'CRPS_reg_output': loss_function = lambda y, rv_y: crps_loss(y, rv_y, y_complete = y_complete) + regularize_output(rv_y)
    if loss == 'CRPS': loss_function = lambda y, rv_y: crps_loss(y, rv_y, y_complete = y_complete)
    if loss == 'twCRPS': loss_function = lambda y, rv_y: twcrps_loss(y, rv_y, kwargs['threshold'])
    if loss == 'wCRPS': loss_function = lambda y, rv_y: crps_loss(y, rv_y, y_complete=y_complete, weights = True)
    if loss == 'wCRPS_negative_price': loss_function = lambda y, rv_y: wcrps_negative_price_loss(y, rv_y, y_complete=y_complete, negative_price_weight=kwargs['negative_price_weight'])
    model.compile(optimizer=keras.optimizers.Adam(0.001), loss=loss_function)
                    
    return model

