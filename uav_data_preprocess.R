#' ---
#' title: "Prepare UAS data for machine learning tuning"
#' author: "Samuel Araya"
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' output:
#'    html_document:
#'      toc: true
#'      keep_md: true
#' ---
# /*
## RENDER CODE:
rm(list=ls())
library(knitr)
opts_chunk$set(tidy=TRUE, warning=FALSE, message=FALSE)
library(kableExtra)
rmarkdown::render(input = "uav_data_preprocess.R")
#
# */

#+ setup, warning=FALSE, message=FALSE
# Libraries
library(tidyverse)
library(magrittr)
library(data.table)
library(caret) 
library(mlr)
library(lubridate)
library(kableExtra)
library(doParallel)
library(gbm)
library(e1071)
library(Hmisc)
#library(parallel)

#+ DataDir
# File Directories
wDir <- getwd()
procDataDir <- file.path(wDir,"Data_Processed")
#
#' Import data
#+ Data, message=F
uav.dt = readRDS(file.path(procDataDir, "VWC_Met_GIS_table4.rds"))
dim(uav.dt)

#' ## Summary
#' This script imports the `VWC_MET_GIS_table4.rds` data table cleans it 
#' for the machine learning procedure and saves it as `uav_data4.rds`. 
#' This script is followed by `uav_data_split.R`.
#' 
#' ## Remove predictors
#'  1. Resolutions < 60 cm: because soil measurement points are not to accurate to <60 cm.
#'  2. ndvi: bacause it is redundant with ttvi
#'  3. Fdsm: because it is redundant with dsm
#'  4. TWI_DI_dsm: because it is redundant with NTWI_DI_dsm
#'  5. TWI_MF_dsm: because it is redundant with NTWI_MF_dsm
#'  6. NTWI_DI_dsm: because it has NA values
#'  7. NTWI_MF_dsm resolutions >100: because it contains NA values at 300 and 500 cm resolutions
#'  8. cumET and cumPrecip: longer than 30 days worth is not really influencing moisture.
#'  9. Asp: redundant with N and E. Circular
#'  
rm_pattern <- c("_15", "_30$","dsm_6$", "ndvi_","Fdsm_", "TWI_DI_dsm", "^TWI_MF_dsm",
                "NTWI_DI_dsm", "NTWI_MF_dsm_300" , "NTWI_MF_dsm_500", "Asp_dsm_",
                ## Non predictor variables
                #"Distance", 
                "Date", "wy$", "Basin", "MUSYM", "MUKEY",
                ## Remove CIMIS precipitation data and UCM ET data
                "cum"
                )

pred_remove = unique(grep(paste(rm_pattern, collapse = "|"),
                          names(uav.dt), value = T) )
pred_remove

uav.dt <- uav.dt%>%
  dplyr::select(-pred_remove)%>%
  dplyr::mutate(Transect = as.factor(Transect))

dim(uav.dt)

uav.dt <- uav.dt%>%
  dplyr::mutate(A_MF_dsm_60  = log(A_MF_dsm_60),
                A_MF_dsm_100 = log(A_MF_dsm_100),
                A_MF_dsm_300 = ifelse(A_MF_dsm_300 > 0.01,
                                         log(A_MF_dsm_300), -4.7),
                A_MF_dsm_500 = ifelse(A_MF_dsm_500 > 0.01,
                                         log(A_MF_dsm_500), -4.7) 
                )

#' there are no missing (NA) values
colSums(is.na(uav.dt))


#' ## Check for  zero or near-zero variance predictors
nzv.dt<- nearZeroVar(uav.dt,saveMetrics = T)
nzv.dt
# All pass.

#' ## Check for correlated predictors
dt.num = dplyr::select_if(uav.dt, is.numeric)

descrCor = rcorr(as.matrix(dt.num))

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}

flat_corr = flattenCorrMatrix((descrCor$r^2), descrCor$P)
high_corr = dplyr::filter(flat_corr, cor > 0.8)
write.csv(high_corr, file.path(procDataDir,"High_corr_predictors.csv"))

#' Removed the following preditctors due to  high linear correlation:
#' 
#'  1. roll2.et: with roll1.et
#'  2. roll3.et: with roll1.et
#'  3. roll7.et: with roll1.et and roll15.et
#'  4. roll30.et: with roll15.et
#'  5. roll7.Precip: with roll1.Precip
#'  6. roll3.Precip: with roll2.Precip
#'  7. A_DI_dsm_: with A_MF_dsm_
#'  8. C_dsm_: because of high correlation with C_Pla and C_Pro.
#'  9. dsm: High correlation between scales also strong indicator between transects.
#'  10. N_dsm_100: High correlation with N_dsm_60 and N_dsm_300
#'  11. E_dsm_100: High correlation with E_dsm_60
#'  12. N_S_dsm_100: High correlation with N_S_dsm_60
#'  13. TPI_1_5_: high correlation with TPI_1_3_ and TPI_1_7_
#'  14. TPI_1_7_: high correlation with TPI_3_5_ and TPI_3_7
#'  15. TPI_3_5_: high correlation with TPI_3_7_
#'  
#'  
rm_pattern2 <- c("roll2.et", "roll3.et", "roll7.et", "roll30.et",
                 "roll3.Precip", "roll7.Precip",
                 "A_DI", "C_dsm","^dsm", "N_dsm_100", "E_dsm_100", "N_S_dsm_100",
                 "TPI_1_5_","TPI_1_7_", "TPI_3_5_")

pred_remove2 = unique(grep(paste(rm_pattern2, collapse = "|"),
                           names(uav.dt), value = T) )
pred_remove2

uav.dt <- uav.dt%>%
  dplyr::select(-pred_remove2)
dim(uav.dt)
# Second linear correlation
dt.num2 = dplyr::select_if(uav.dt, is.numeric)
descrCor2 = rcorr(as.matrix(dt.num2))
flat_corr2 = flattenCorrMatrix((descrCor2$r^2), descrCor2$P)
high_corr2 = dplyr::filter(flat_corr2, cor > 0.7)
write.csv(high_corr2, file.path(procDataDir,"High_corr_predictors2.csv"))

#' ## Check linear dependencies
dt.num2$wyd = NULL
comboInfo <- findLinearCombos(dt.num2)
comboInfo
#No linear dependency

#' Recommended columns to remove due to linear combinations
names(dt.num2)[comboInfo$remove]


#' there are no missing (NA) values
colSums(is.na(uav.dt))





## Following RFE Drop the following variables...
# rm_pattern3 <- c()
# 
# pred_remove3 = unique(grep(paste(rm_pattern3, collapse = "|"),
#                            names(uav.dt), value = T) )
# pred_remove3
# 
# uav.dt <- uav.dt%>%
#   dplyr::select(-pred_remove3)
# dim(uav.dt)

#' ## Save Cleaned Table
saveRDS(uav.dt, file.path(procDataDir, "uav_data4.rds"))