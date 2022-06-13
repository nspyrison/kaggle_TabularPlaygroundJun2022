# tabular playground, Jun 2022 -----
######|
## https://www.kaggle.com/competitions/tabular-playground-series-jun-2022/data
## See the results of 1eda.r, 
## then continuing on here trying to apply the mice package.
######|
## Ok, so with the 3mice.r, mice package I was about to get to about q=.5, 
## I want to be able to see if I can improve this with an LGBM model before 
## I would turn to Python for parallelization and more through tuning.
######|

## NB: I think the LGBM link I was following is not going to work for this case,
## have be 


attempt <- "LGBM"
message(paste0("Started run: ", attempt, " at ", Sys.time()))

## Read and split ----
library(data.table)
library(Matrix)
library(dplyr)
library(MLmetrics)
library(lightgbm)
library(tictoc)
library(beepr)

tictoc::tic("4lgbm.r full run")
dat <- fread("./data/data.csv") %>% as.data.frame()
idx_train <- rep(TRUE, nrow(dat))
idx_train[sample(1:nrow(dat), size = .2 * nrow(dat))] <- FALSE


## Pre Processing -----


tictoc::tic("median impute")
train = col_median_impute(train)
test  = col_median_impute(test)
tictoc::toc()
