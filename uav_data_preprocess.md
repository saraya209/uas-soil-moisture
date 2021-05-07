---
title: "Prepare UAS data for machine learning tuning"
author: "Samuel Araya"
date: 'May 07, 2021'
output:
   html_document:
     toc: true
     keep_md: true
---


```r
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
```

```r
# File Directories
wDir <- getwd()
procDataDir <- file.path(wDir,"Data_Processed")
#
```

Import data


```r
uav.dt = readRDS(file.path(procDataDir, "VWC_Met_GIS_table4.rds"))
dim(uav.dt)
```

```
## [1] 406 168
```

## Summary
This script imports the `VWC_MET_GIS_table4.rds` data table cleans it 
for the machine learning procedure and saves it as `uav_data4.rds`. 
This script is followed by `uav_data_split.R`.

## Remove predictors
 1. Resolutions < 60 cm: because soil measurement points are not to accurate to <60 cm.
 2. ndvi: bacause it is redundant with ttvi
 3. Fdsm: because it is redundant with dsm
 4. TWI_DI_dsm: because it is redundant with NTWI_DI_dsm
 5. TWI_MF_dsm: because it is redundant with NTWI_MF_dsm
 6. NTWI_DI_dsm: because it has NA values
 7. NTWI_MF_dsm resolutions >100: because it contains NA values at 300 and 500 cm resolutions
 8. cumET and cumPrecip: longer than 30 days worth is not really influencing moisture.
 9. Asp: redundant with N and E. Circular
 


```r
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
```

```
##  [1] "Date"            "wy"              "cum.Precip"      "cum.ET"          "ccum.Precip"     "ccum.ET"         "MUSYM"          
##  [8] "MUKEY"           "A_DI_dsm_15"     "A_DI_dsm_30"     "A_MF_dsm_15"     "A_MF_dsm_30"     "Asp_dsm_100"     "Asp_dsm_15"     
## [15] "Asp_dsm_30"      "Asp_dsm_300"     "Asp_dsm_500"     "Asp_dsm_60"      "C_dsm_15"        "C_dsm_30"        "C_Pla_dsm_15"   
## [22] "C_Pla_dsm_30"    "DINF_dsm_15"     "DINF_dsm_30"     "dsm_15"          "dsm_30"          "dsm_6"           "Fdsm_100"       
## [29] "Fdsm_15"         "Fdsm_30"         "Fdsm_300"        "Fdsm_500"        "Fdsm_60"         "NTWI_DI_dsm_100" "NTWI_DI_dsm_15" 
## [36] "NTWI_DI_dsm_30"  "NTWI_DI_dsm_300" "NTWI_DI_dsm_500" "NTWI_DI_dsm_60"  "NTWI_MF_dsm_15"  "NTWI_MF_dsm_30"  "NTWI_MF_dsm_300"
## [43] "NTWI_MF_dsm_500" "S_dsm_15"        "S_dsm_30"        "TWI_DI_dsm_100"  "TWI_DI_dsm_15"   "TWI_DI_dsm_30"   "TWI_DI_dsm_300" 
## [50] "TWI_DI_dsm_500"  "TWI_DI_dsm_60"   "TWI_MF_dsm_100"  "TWI_MF_dsm_15"   "TWI_MF_dsm_30"   "TWI_MF_dsm_300"  "TWI_MF_dsm_500" 
## [57] "TWI_MF_dsm_60"   "ed_15"           "ed_30"           "g_15"            "g_30"            "ndvi_100"        "ndvi_15"        
## [64] "ndvi_30"         "ndvi_300"        "ndvi_60"         "nir_15"          "nir_30"          "r_15"            "r_30"           
## [71] "ttvi_15"         "ttvi_30"         "Basin"
```

```r
uav.dt <- uav.dt%>%
  dplyr::select(-pred_remove)%>%
  dplyr::mutate(Transect = as.factor(Transect))

dim(uav.dt)
```

```
## [1] 406  95
```

```r
uav.dt <- uav.dt%>%
  dplyr::mutate(A_MF_dsm_60  = log(A_MF_dsm_60),
                A_MF_dsm_100 = log(A_MF_dsm_100),
                A_MF_dsm_300 = ifelse(A_MF_dsm_300 > 0.01,
                                         log(A_MF_dsm_300), -4.7),
                A_MF_dsm_500 = ifelse(A_MF_dsm_500 > 0.01,
                                         log(A_MF_dsm_500), -4.7) 
                )
```

there are no missing (NA) values


```r
colSums(is.na(uav.dt))
```

```
##        Transect        Distance             VWC             wyd    roll1.Precip        roll1.et    roll2.Precip        roll2.et 
##               0               0               0               0               0               0               0               0 
##    roll3.Precip        roll3.et    roll7.Precip        roll7.et   roll15.Precip       roll15.et   roll30.Precip       roll30.et 
##               0               0               0               0               0               0               0               0 
##    A_DI_dsm_100    A_DI_dsm_300    A_DI_dsm_500     A_DI_dsm_60    A_MF_dsm_100    A_MF_dsm_300    A_MF_dsm_500     A_MF_dsm_60 
##               0               0               0               0               0               0               0               0 
##       C_dsm_100       C_dsm_300       C_dsm_500      C_dsm_5000        C_dsm_60   C_Pla_dsm_100   C_Pla_dsm_300   C_Pla_dsm_500 
##               0               0               0               0               0               0               0               0 
##  C_Pla_dsm_5000    C_Pla_dsm_60   C_Pro_dsm_100   C_Pro_dsm_300   C_Pro_dsm_500  C_Pro_dsm_5000    DINF_dsm_100    DINF_dsm_300 
##               0               0               0               0               0               0               0               0 
##    DINF_dsm_500     DINF_dsm_60         dsm_100         dsm_300         dsm_500          dsm_60       E_dsm_100       E_dsm_300 
##               0               0               0               0               0               0               0               0 
##       E_dsm_500        E_dsm_60       N_dsm_100       N_dsm_300       N_dsm_500        N_dsm_60     N_S_dsm_100     N_S_dsm_300 
##               0               0               0               0               0               0               0               0 
##     N_S_dsm_500      N_S_dsm_60 NTWI_MF_dsm_100  NTWI_MF_dsm_60       S_dsm_100       S_dsm_300       S_dsm_500        S_dsm_60 
##               0               0               0               0               0               0               0               0 
##     TPI_1_3_100     TPI_1_3_300     TPI_1_3_500     TPI_1_5_100     TPI_1_5_300     TPI_1_5_500     TPI_1_7_100     TPI_1_7_300 
##               0               0               0               0               0               0               0               0 
##     TPI_1_7_500    TPI_3_20_500     TPI_3_5_100     TPI_3_5_300     TPI_3_5_500     TPI_3_7_100     TPI_3_7_300     TPI_3_7_500 
##               0               0               0               0               0               0               0               0 
##          ed_100          ed_300           ed_60           g_100           g_300            g_60         nir_100         nir_300 
##               0               0               0               0               0               0               0               0 
##          nir_60           r_100           r_300            r_60        ttvi_100        ttvi_300         ttvi_60 
##               0               0               0               0               0               0               0
```

## Check for  zero or near-zero variance predictors


```r
nzv.dt<- nearZeroVar(uav.dt,saveMetrics = T)
nzv.dt
```

```
##                 freqRatio percentUnique zeroVar   nzv
## Transect         1.056338     1.4778325   FALSE FALSE
## Distance         1.027027     4.6798030   FALSE FALSE
## VWC              1.818182    20.1970443   FALSE FALSE
## wyd              1.243243     1.4778325   FALSE FALSE
## roll1.Precip     5.343750     0.4926108   FALSE FALSE
## roll1.et         1.243243     1.4778325   FALSE FALSE
## roll2.Precip     1.942029     0.4926108   FALSE FALSE
## roll2.et         1.243243     1.4778325   FALSE FALSE
## roll3.Precip     1.942029     0.4926108   FALSE FALSE
## roll3.et         1.243243     1.4778325   FALSE FALSE
## roll7.Precip     3.621622     0.7389163   FALSE FALSE
## roll7.et         1.243243     1.4778325   FALSE FALSE
## roll15.Precip    1.243243     1.4778325   FALSE FALSE
## roll15.et        1.243243     1.4778325   FALSE FALSE
## roll30.Precip    1.243243     1.4778325   FALSE FALSE
## roll30.et        1.243243     1.4778325   FALSE FALSE
## A_DI_dsm_100     4.700000    20.4433498   FALSE FALSE
## A_DI_dsm_300    13.375000    17.9802956   FALSE FALSE
## A_DI_dsm_500    15.111111    15.2709360   FALSE FALSE
## A_DI_dsm_60      2.625000    22.4137931   FALSE FALSE
## A_MF_dsm_100     1.000000    23.8916256   FALSE FALSE
## A_MF_dsm_300     1.500000    23.6453202   FALSE FALSE
## A_MF_dsm_500     2.000000    22.4137931   FALSE FALSE
## A_MF_dsm_60      1.000000    23.8916256   FALSE FALSE
## C_dsm_100        1.000000    23.8916256   FALSE FALSE
## C_dsm_300        1.000000    23.8916256   FALSE FALSE
## C_dsm_500        1.000000    22.9064039   FALSE FALSE
## C_dsm_5000       1.216216     4.6798030   FALSE FALSE
## C_dsm_60         1.000000    23.8916256   FALSE FALSE
## C_Pla_dsm_100    1.000000    23.8916256   FALSE FALSE
## C_Pla_dsm_300    1.000000    23.8916256   FALSE FALSE
## C_Pla_dsm_500    1.000000    22.9064039   FALSE FALSE
## C_Pla_dsm_5000   1.216216     4.6798030   FALSE FALSE
## C_Pla_dsm_60     1.000000    23.8916256   FALSE FALSE
## C_Pro_dsm_100    1.000000    23.8916256   FALSE FALSE
## C_Pro_dsm_300    1.000000    23.8916256   FALSE FALSE
## C_Pro_dsm_500    1.000000    22.9064039   FALSE FALSE
## C_Pro_dsm_5000   1.216216     4.6798030   FALSE FALSE
## DINF_dsm_100     1.125000    13.0541872   FALSE FALSE
## DINF_dsm_300     1.468085    13.3004926   FALSE FALSE
## DINF_dsm_500     1.450000    10.0985222   FALSE FALSE
## DINF_dsm_60      1.117647    10.5911330   FALSE FALSE
## dsm_100          1.000000    23.8916256   FALSE FALSE
## dsm_300          1.000000    23.6453202   FALSE FALSE
## dsm_500          1.000000    22.9064039   FALSE FALSE
## dsm_60           1.000000    23.8916256   FALSE FALSE
## E_dsm_100        1.000000    23.8916256   FALSE FALSE
## E_dsm_300        1.000000    23.8916256   FALSE FALSE
## E_dsm_500        1.000000    22.9064039   FALSE FALSE
## E_dsm_60         1.000000    23.8916256   FALSE FALSE
## N_dsm_100        1.000000    23.8916256   FALSE FALSE
## N_dsm_300        1.000000    23.8916256   FALSE FALSE
## N_dsm_500        1.000000    22.9064039   FALSE FALSE
## N_dsm_60         1.000000    23.8916256   FALSE FALSE
## N_S_dsm_100      1.000000    23.8916256   FALSE FALSE
## N_S_dsm_300      1.000000    23.8916256   FALSE FALSE
## N_S_dsm_500      1.000000    22.9064039   FALSE FALSE
## N_S_dsm_60       1.000000    23.8916256   FALSE FALSE
## NTWI_MF_dsm_100  1.000000    23.8916256   FALSE FALSE
## NTWI_MF_dsm_60   1.000000    23.8916256   FALSE FALSE
## S_dsm_100        1.000000    23.8916256   FALSE FALSE
## S_dsm_300        1.000000    23.8916256   FALSE FALSE
## S_dsm_500        1.000000    22.9064039   FALSE FALSE
## S_dsm_60         1.000000    23.8916256   FALSE FALSE
## TPI_1_3_100      1.000000    23.8916256   FALSE FALSE
## TPI_1_3_300      1.000000    23.8916256   FALSE FALSE
## TPI_1_3_500      1.000000    22.9064039   FALSE FALSE
## TPI_1_5_100      1.000000    23.8916256   FALSE FALSE
## TPI_1_5_300      1.000000    23.8916256   FALSE FALSE
## TPI_1_5_500      1.000000    22.9064039   FALSE FALSE
## TPI_1_7_100      1.000000    23.8916256   FALSE FALSE
## TPI_1_7_300      1.000000    23.8916256   FALSE FALSE
## TPI_1_7_500      1.000000    22.9064039   FALSE FALSE
## TPI_3_20_500     1.000000    22.9064039   FALSE FALSE
## TPI_3_5_100      1.000000    23.8916256   FALSE FALSE
## TPI_3_5_300      1.000000    23.8916256   FALSE FALSE
## TPI_3_5_500      1.000000    22.9064039   FALSE FALSE
## TPI_3_7_100      1.000000    23.8916256   FALSE FALSE
## TPI_3_7_300      1.000000    23.8916256   FALSE FALSE
## TPI_3_7_500      1.000000    22.9064039   FALSE FALSE
## ed_100           1.000000    97.7832512   FALSE FALSE
## ed_300           1.000000    97.7832512   FALSE FALSE
## ed_60            1.000000    97.7832512   FALSE FALSE
## g_100            1.000000    97.7832512   FALSE FALSE
## g_300            1.000000    97.7832512   FALSE FALSE
## g_60             1.000000    97.7832512   FALSE FALSE
## nir_100          1.000000    97.7832512   FALSE FALSE
## nir_300          1.000000    97.7832512   FALSE FALSE
## nir_60           1.000000    97.7832512   FALSE FALSE
## r_100            1.000000    97.7832512   FALSE FALSE
## r_300            1.000000    97.7832512   FALSE FALSE
## r_60             1.000000    97.7832512   FALSE FALSE
## ttvi_100         1.000000    97.7832512   FALSE FALSE
## ttvi_300         1.000000    97.7832512   FALSE FALSE
## ttvi_60          1.000000    97.7832512   FALSE FALSE
```

```r
# All pass.
```

## Check for correlated predictors


```r
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
```

Removed the following preditctors due to  high linear correlation:

 1. roll2.et: with roll1.et
 2. roll3.et: with roll1.et
 3. roll7.et: with roll1.et and roll15.et
 4. roll30.et: with roll15.et
 5. roll7.Precip: with roll1.Precip
 6. roll3.Precip: with roll2.Precip
 7. A_DI_dsm_: with A_MF_dsm_
 8. C_dsm_: because of high correlation with C_Pla and C_Pro.
 9. dsm: High correlation between scales also strong indicator between transects.
 10. N_dsm_100: High correlation with N_dsm_60 and N_dsm_300
 11. E_dsm_100: High correlation with E_dsm_60
 12. N_S_dsm_100: High correlation with N_S_dsm_60
 13. TPI_1_5_: high correlation with TPI_1_3_ and TPI_1_7_
 14. TPI_1_7_: high correlation with TPI_3_5_ and TPI_3_7
 15. TPI_3_5_: high correlation with TPI_3_7_
 
 


```r
rm_pattern2 <- c("roll2.et", "roll3.et", "roll7.et", "roll30.et",
                 "roll3.Precip", "roll7.Precip",
                 "A_DI", "C_dsm","^dsm", "N_dsm_100", "E_dsm_100", "N_S_dsm_100",
                 "TPI_1_5_","TPI_1_7_", "TPI_3_5_")

pred_remove2 = unique(grep(paste(rm_pattern2, collapse = "|"),
                           names(uav.dt), value = T) )
pred_remove2
```

```
##  [1] "roll2.et"     "roll3.Precip" "roll3.et"     "roll7.Precip" "roll7.et"     "roll30.et"    "A_DI_dsm_100" "A_DI_dsm_300" "A_DI_dsm_500"
## [10] "A_DI_dsm_60"  "C_dsm_100"    "C_dsm_300"    "C_dsm_500"    "C_dsm_5000"   "C_dsm_60"     "dsm_100"      "dsm_300"      "dsm_500"     
## [19] "dsm_60"       "E_dsm_100"    "N_dsm_100"    "N_S_dsm_100"  "TPI_1_5_100"  "TPI_1_5_300"  "TPI_1_5_500"  "TPI_1_7_100"  "TPI_1_7_300" 
## [28] "TPI_1_7_500"  "TPI_3_5_100"  "TPI_3_5_300"  "TPI_3_5_500"
```

```r
uav.dt <- uav.dt%>%
  dplyr::select(-pred_remove2)
dim(uav.dt)
```

```
## [1] 406  64
```

```r
# Second linear correlation
dt.num2 = dplyr::select_if(uav.dt, is.numeric)
descrCor2 = rcorr(as.matrix(dt.num2))
flat_corr2 = flattenCorrMatrix((descrCor2$r^2), descrCor2$P)
high_corr2 = dplyr::filter(flat_corr2, cor > 0.7)
write.csv(high_corr2, file.path(procDataDir,"High_corr_predictors2.csv"))
```

## Check linear dependencies


```r
dt.num2$wyd = NULL
comboInfo <- findLinearCombos(dt.num2)
comboInfo
```

```
## $linearCombos
## list()
## 
## $remove
## NULL
```

```r
#No linear dependency
```

Recommended columns to remove due to linear combinations


```r
names(dt.num2)[comboInfo$remove]
```

```
## character(0)
```

there are no missing (NA) values


```r
colSums(is.na(uav.dt))
```

```
##        Transect        Distance             VWC             wyd    roll1.Precip        roll1.et    roll2.Precip   roll15.Precip 
##               0               0               0               0               0               0               0               0 
##       roll15.et   roll30.Precip    A_MF_dsm_100    A_MF_dsm_300    A_MF_dsm_500     A_MF_dsm_60   C_Pla_dsm_100   C_Pla_dsm_300 
##               0               0               0               0               0               0               0               0 
##   C_Pla_dsm_500  C_Pla_dsm_5000    C_Pla_dsm_60   C_Pro_dsm_100   C_Pro_dsm_300   C_Pro_dsm_500  C_Pro_dsm_5000    DINF_dsm_100 
##               0               0               0               0               0               0               0               0 
##    DINF_dsm_300    DINF_dsm_500     DINF_dsm_60       E_dsm_300       E_dsm_500        E_dsm_60       N_dsm_300       N_dsm_500 
##               0               0               0               0               0               0               0               0 
##        N_dsm_60     N_S_dsm_300     N_S_dsm_500      N_S_dsm_60 NTWI_MF_dsm_100  NTWI_MF_dsm_60       S_dsm_100       S_dsm_300 
##               0               0               0               0               0               0               0               0 
##       S_dsm_500        S_dsm_60     TPI_1_3_100     TPI_1_3_300     TPI_1_3_500    TPI_3_20_500     TPI_3_7_100     TPI_3_7_300 
##               0               0               0               0               0               0               0               0 
##     TPI_3_7_500          ed_100          ed_300           ed_60           g_100           g_300            g_60         nir_100 
##               0               0               0               0               0               0               0               0 
##         nir_300          nir_60           r_100           r_300            r_60        ttvi_100        ttvi_300         ttvi_60 
##               0               0               0               0               0               0               0               0
```

```r
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
```

## Save Cleaned Table


```r
saveRDS(uav.dt, file.path(procDataDir, "uav_data4.rds"))
```

