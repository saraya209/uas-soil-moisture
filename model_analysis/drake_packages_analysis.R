# Analyze models and create reports and summary tables
# Libraries ------------
library(kernlab)
library(xgboost)
library(gbm)
library(randomForest)
library(nnet)
library(caret)
library(tidyverse)
library(readxl)
library(DT)
library(drake)
library(DALEX)
library("ingredients")
library(viridis)
library(pdp)
library(ALEPlot)
#Install dependecies for DALEX
#install_dependencies(packages = c("pdp", "ALEPlot", "breakDown","ggpubr", "factorMerger"))
