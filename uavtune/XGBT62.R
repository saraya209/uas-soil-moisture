#' # Machine Learning Tunning In Parallel 
#' (Samuel N Araya)

# *********************************************
#' Model type and version selector:
model.number = 62
model.string = 'xgbTree'
# Hyperparameter file
para.fname = "tune6_parameters.csv"
# Subset to split training data
keep.idx = NULL
# Run model tuning script
source("universal_tune_doParallel.R")