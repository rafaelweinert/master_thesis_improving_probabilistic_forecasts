# master_thesis_improving_probabilistic_forecasts

This file briefly explains the code for the Master Thesis "Improving probabilistic forecasts of extreme day-ahead electricity prices and wind gusts" by Rafael Weinert.

1. Folder: mt_theory

This folder contains the scripts that were used to create the plots of the theoretical introduction in the thesis. To reproduce, adjust the working directory accordingly in each file.

2. Folder: master_thesis_tail_wind

This folder contains the code for evaluating the wind gust forecasts. 
The main script is tailcalibration_plots.R. It evaluates both the calibration, creates the respective plots for each postprocessing method, and calculates the CRPS values. In the function, both tasks can be done separately, or jointly (Calculating the CRPS of the HEN method takes a few days).
The file crps_plot.R creates plots of the obtained CRPS values for the thesis.
The file crps_total.R and crps_total.py are used to create tables with the CRPS values for the paper (run in this order).
The file visual_inspection.R is only for having a first look into the data provided. It is not relevant for the thesis.
Again, to reproduce the code, adjust the working directories accordingly in each file.

3. Folder: distributionallnn

This folder contains the code for evaluating the DDNN models in the second case studies.
The original code is stored in the folder "Python". It is not used, but left here for reference.
The code used to train and test the models is in the folder "Neural_Networks". Several files are contained in this folder:
- build_dataset.py: Not relevant for the paper
- nn_best_params.py: In this file, the various DDNN models are trained, and the forecasts of the test data are stored. Make sure that the file utils.py is imported properly. IMPORTANT: before running the script, make sure to correctly set the loss functions, output distributions and IDR flag!
- nn_tail_calibration.R: In this file, the predictions made in the previous file are evaluated (both, calibration and CRPS, but only the original model architecture). Make sure to properly import utils.R
- ird.R: This file computes the calibration and CRPS for the IDR postprocessed forecasts. Note: due to their large file size, the forecast files are not included already, but have to be reproduced first.


General notes:
The requirements.txt file contains the packages that were used for the python files. Note, that the isodisreg package must be downloaded at https://github.com/evwalz/isodisreg and installed manually first.

Moreover, the python version used is 3.9.21 (higher versions of python are not compatible with the version of tensorflow, and higher versions of tensorflow are not compatible with the code, so make sure to have the correct versions installed!)