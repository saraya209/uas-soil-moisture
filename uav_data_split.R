#' ---
#' title: "Multiple Splits Into Training and Testing Sets"
#' author: "Samuel Araya"
#' date: "`r format(Sys.time(), '%d %B, %Y')`"
#' output:
#'    html_document:
#'      toc: true
#'      keep_md: true
#' ---
#'
# /*
## RENDER CODE:
rm(list=ls())
library(knitr)
opts_chunk$set(tidy=TRUE, warning=FALSE, message=FALSE)
library(kableExtra)
rmarkdown::render(input = "uav_data_split.R")
#
# */ 
#+ Libraries, include=FALSE
library(tidyverse)
library(magrittr)
library(data.table)
library(caret) 
library(mlr)
library(lubridate)
library(kableExtra)
library(viridis)
#library(parallel)

#+ DataDir, include=FALSE, message=FALSE
# File Directories
wDir <- getwd()
procDataDir <- file.path(wDir,"Data_Processed")
#

#+ Data, message=F
# Import data
uav.dt <- read_rds(file.path(procDataDir, "uav_data4.rds"))

#' ## Standardize variables
#' **The standardization (centering and scaling) is done based on mean and variance of entire dataset. 
#' This should not meaningfully affect the independence of the hold-out test sets.**
pred.names <- names(uav.dt)[-(1:4)]
pred.names
pp <- preProcess(uav.dt, method = list(center = pred.names , scale = pred.names))
#Preprocess tables
std_uav.dt <- stats::predict(pp, uav.dt)

saveRDS(std_uav.dt, file = file.path(procDataDir, "std_uav_data_v3.rds"), version = 2)

#' ## Training and testing set split
#' For the testing set, we will select two transects at random from 
#' 4 of the 6 sampling days, and select one transect from each of the
#' remaining 2 sampling dates. This will ensure that:
#' 
#' 1. all six days are represented in the training set.
#' 2. training set is between 70 and 80 percent (i.e. 284 and 325 records).
#' 
#' Number of records within date-transect group:
kable(table(uav.dt$wyd, uav.dt$Transect)) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

#' Each combination of date:transect on average has
{{round(mean(table(uav.dt$wyd, uav.dt$Transect)), digits = 2)}}
#' records. For the training set:
#' within four randomely selected dates select 4 random transects and
#' 5 random transects within the remaining two sampling dates.

make_trainset <- function(x, uav.dt){
  all_transects <- as.character(unique(uav.dt$Transect))
  all_wyds <- unique(uav.dt$wyd)
  # subset with 4 transects from 4 days
  wyd4 <- sample(all_wyds, 4, replace = F)
  train_set4 <- tibble()
  for (wyd in wyd4){
    Transect <- sample(all_transects, 4, replace = F)
    train_set.i <- tidyr::crossing(wyd, Transect)
    train_set4 <- bind_rows(train_set4, train_set.i)
  }
  
  # subset with 5 Transect from the REMAINING two days
  wyd2 <- all_wyds[!all_wyds %in% wyd4]
  train_set5 <- tibble()
  for (wyd in wyd2){
    Transect <- sample(all_transects, 5)
    train_set.i2 <- tidyr::crossing(wyd, Transect)
    train_set5 <- bind_rows(train_set5, train_set.i2)
  }
  # complate training set
  train_set <- bind_rows(train_set4, train_set5)
  train_set <- train_set %>% 
    arrange(wyd, Transect)
  return(train_set)
}
#' Produce 15 random split indices
# Create 100 indices and remove duplicates...
train.lst <- lapply(1:60, make_trainset, uav.dt)
# Remove duplicates
train.lst <- train.lst[!duplicated(train.lst)]
# Check duplicates!
for (i in 1:length(train.lst)){
  for (j in 1:length(train.lst)){
    if(i != j){
      dup = identical(train.lst[[i]], train.lst[[j]])
      if(dup){
      print(paste("Duplicates", i, "and", j))
        }
    }
    
  }
  
}
#' Keep only 30 splits for training set.
#' 

length(train.lst)
train.lst <- train.lst[1:30]

#' Get index of sets to be used

# Save the training random split references  
#Version 2 is compatible with R 3.5 current version in MERCED cluster.
saveRDS(train.lst, file = file.path(procDataDir, "train_split_list.rds"), version = 2)

#' ## Summarize Training and Testing Sets
#' Create a list of the
{{length(train.lst)}}
#'  training and testing set tables.
#+ summarize, message=FALSE, warning=FALSE
resampleListFunction <- function(trainset.lst, mydata, train = TRUE, rem.cols = TRUE){
  if (train){
    sub.dt <- left_join(trainset.lst, mydata, 
                        by = c( "wyd","Transect"))
  }else {
    sub.dt <- anti_join(mydata, trainset.lst, 
                        by = c( "wyd", "Transect"))
  }
  if (rem.cols){
    # Remove non predictor columns:
    sub.dt <- subset(sub.dt, select= -c(Transect, Distance, wyd))
  }
  return(sub.dt)
}
# Generate list of train and test data frames
# Import from the saved version
#train.lst <- readRDS(file.path(procDataDir, "train_split_list.rds"))
train.dt.lst = lapply(train.lst, 
                     resampleListFunction, 
                     mydata = uav.dt, 
                     train = TRUE,
                     rem.cols = FALSE)

test.dt.lst = lapply(train.lst, 
                     resampleListFunction, 
                     mydata = uav.dt, 
                     train = FALSE,
                     rem.cols = FALSE)

# Merge lists into a table
rbind_list <- function(table_list){
  c.dt <- data.frame()
  for (i in 1:length(table_list)){
    i.dt = table_list[[i]]
    i.dt$split_id = i 
    c.dt = bind_rows(c.dt, i.dt) 
  }
  
  return(c.dt)
}

train.dt <- rbind_list(train.dt.lst)
test.dt <- rbind_list(test.dt.lst)

#' ### Summary of water year days and transects in each set
summ_sets <- function(split_dt, keep.tran = FALSE){
  
  if(keep.tran){
    split_dt <- split_dt %>% 
      dplyr::group_by(split_id, wyd, Transect) %>% 
      dplyr::summarise(count_sample = n())
  }else{
    split_dt <- split_dt %>% 
      dplyr::group_by(split_id) %>% 
      dplyr::summarise(count_wyd = length(unique(wyd)),
                       count_tran = length(unique(Transect)),
                       tran = paste(unique(Transect), collapse = ", "),
                       count_sample = n())
  }
  return(split_dt)
}

summ.train <- summ_sets(train.dt)
summ.test <- summ_sets(test.dt)

#' Training set
kable(mlr::summarizeColumns(summ.train) ) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)
#' Testing set
kable(mlr::summarizeColumns(summ.test)) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

#' ### Distribution of samples across transect and sampling dates

plot_summ <- function(split_dt){
  summ_split_dt <- summ_sets(split_dt, keep.tran = T)
  
  bp <- ggplot(data = summ_split_dt, aes(y = count_sample, x = factor(wyd)))+
    geom_bar(aes(fill = Transect), stat="identity")+
    labs(x = "Days Into Water Year",
         y = "Number of Samples")+
    theme_bw()+
    theme(legend.position="top",
          strip.background = element_blank(),
          panel.border = element_rect(colour = "black"))+
    scale_fill_viridis_d()+
    facet_wrap(~split_id, ncol = 6)
    
  return(bp)
}

p.train <- plot_summ(train.dt)
p.test <- plot_summ(test.dt)

#' Training sets
#+ train.plot, fig.width=9, fig.height=7
p.train

#' Testing sets
#+ test.plot, fig.width=9, fig.height=7
p.test

#' ### Detailed summary of sampling dates and transects in each set
#' Training set
kable(summ.train) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)
#' Testing set
kable(summ.test) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)
