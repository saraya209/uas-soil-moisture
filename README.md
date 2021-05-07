# UAS Soil Moisture Sensing

This repository contains *R* codes and data to accompany Araya et al. 2021. HESS.

## Brief descrions of the file strucutre and important files. 
The `*.html` and `*.md` files are reports of corresponding `*.R` scripts with the same file name.

- `uav_data_preprocess.R`:  compiles and prepared the master table (i.e. `uav_data_v4.rds`)
- `uav_data_split.R`:  takes `uav_data_v4.rds` and creates testing and training data splits.
- **Data_Processed/**: has the tabular data including used for training and testing of models
  - `ML_Training_Metada.xlsx`: Description of column headers in dataset. 
  - `VWC_Met_GIS_table4.rds`: Merged data table of soil moisture, hydrologic variables, and terrain variables 
  - `uav_data4.rds`: Cleaned data for machine learning training. Produced from `uav_data_preprocess.R`script.
- **uavtune/**: This contains the main tunning codes that were run on `merced` cluster.
  - **uavout/**: has the built machine learning models. Can be downloded from [here](https://tinyurl.com/h4jbmjnp).
  - `universal_tune_doParallel.R` is the main file that runs the training. It is called  by individual training scripts. For example: to tune a BRT62 model, run the `BRT62.R` file on cluster with all it's dependencies (the other R files, the parameter .csv file, etc.).
- **model_analysis/**: This folder has files that produce reports for each model. Uses `drake` R package.
- **Reports/**: where produced reports are saved in `*.html` file
