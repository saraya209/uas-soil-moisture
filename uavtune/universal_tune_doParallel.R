### Machine Learning Model Tuning In Parallel 
# (Samuel N Araya)
## Accept `model.number`, `model.string`, and `para.fname`

#' # (1) Setup Environment
#' ## (1.1) Load libraries 
library(doParallel) # Parallel computation package (also loads: foreach, iterators and parallel)
library(dplyr)
library(plyr) #required for xgboost and gbm
library(caret) # platform for training.

#
#' ## (1.2) Set up file directory
wDir <- getwd()
outDir <- file.path(wDir, "uavout")
print(wDir)
print(outDir)
#
#' ## (1.3) Import data
# Data set
data.fname = 'std_uav_data_v3.rds'
# Training and testing splitting list.
subset.lst.fname = 'train_split_list.rds'
# Cross-validation training indices
cv.lst.fname = 'cv_index_list.rds'
#
dt = readRDS(file = file.path(wDir,data.fname))
subset.lst = readRDS(file = file.path(wDir,subset.lst.fname))
# subset split list if requred
if (!is.null(keep.idx)){
  subset.lst = subset.lst[keep.idx]
}
#
cv.lst = readRDS(file = file.path(wDir,cv.lst.fname))
#' ## (1.4) Import hyperparameter space
para_file <- file.path(wDir, para.fname)
# #
hypara.dt <- read.csv(file = para_file, colClasses = "character")
# # flter tuning iteration 
hypara.dt <- subset(hypara.dt, model_number == model.number & 
                      model_string == model.string)
# Document hyperparameters:
print("Tuning Hyperparameter Space:")
print(hypara.dt)
#
#' # (2) Tune Model
#' Import tunning related functions
source(file = file.path(wDir,"tune6_functions.R"))
#
#' # (2.1) Setup Parallel Proccessing
numcores = parallel::detectCores()
cl <- makeCluster(numcores)
doParallel::registerDoParallel(cl)
# Document parallel processing variables
print(paste("detectCores() = ", numcores)) # number of cores available and requested
print(paste("getDoParWorkers() = ", getDoParWorkers()))
print(paste("getDoParName() = ", getDoParName()))

#' ## (2.2) Prepare training sets
#' Generate list of training data frames
train.dt.lst <- lapply(subset.lst, resampleListFunction, mydata = dt)
#
#' ## (2.3) Generate list of models
#model.fit.lst <- lapply(train.dt.lst, tuneListFunction, myhypara.dt = hypara.dt)
#' ### (2.3.1) Create nested list of train data and cv list
nest_lst <- function(train_tbl, cv_vec){
  train_cv_lst = list(train_tbl, cv_vec)
  return(train_cv_lst)
}

train_cv_lst <- mapply(nest_lst, 
                       train.dt.lst, 
                       cv.lst)

model.fit.lst <- lapply(train_cv_lst, 
                        tuneListFunction2, 
                        hypara.dt)

# model.fit.lst <- mapply(tuneListFunction, 
#                                     train.dt.lst, 
#                                     cv.lst, 
#                                     MoreArgs = list(myhypara.dt = hypara.dt),
#                                     SIMPLIFY = FALSE)
#
#' # (3) Save  Model
model.fname <- paste0(model.string, "_", model.number, "_list.rds")
saveRDS(model.fit.lst, file = file.path(outDir, model.fname))
#
#' # (4) End Run
#' ## (4.1) Stop the cluster
parallel::stopCluster(cl)
registerDoSEQ() # avoid some error
q()


