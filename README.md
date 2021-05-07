# UAS Soil Moisture Sensing

This repository contains *R* codes and data to accompany Araya et al. 2021. HESS.

## Brief descrions of the file strucutre and important files. 

- **Data_processed/**: has all tabular data including the "master" tables that are used for training and testing
  - `uav_data_v3.rds`: the training data
- `*.html` files are reports of corresponding R scripts with the same file name.
- `uav_Preporcess.R`  compiles and prepared the master table (i.e. `uav_data_v3.rds`) and the training and testing splits.
- **uavtune/**: This contains the main tunning codes that were run on `merced` cluster.
  - **uavout/**: has the built machine learning models.
  - `universal_tune_doParallel.R` is the main file that runs the training. It is called  by individual training scripts. For example: to tune a BRT62 model, run the `BRT62.R` file on cluster with all it's dependencies (the other R files, the parameter .csv file, etc.).
- **model_analysis/**: This folder has files that produce reports for each model. Uses `drake` R package called drake”.
- **Reports/**: where produced reports are saved in `*.html` file
