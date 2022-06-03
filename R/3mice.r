# tabular playground, Jun 2022 -----
######|
## https://www.kaggle.com/competitions/tabular-playground-series-jun-2022/data
## See the results of 1eda.r, 
## then continuing on here trying to apply the mice package.
######|

## Read ----
library(readr)
library(mice)
library(tictoc)
library(beepr)

tictoc::tic("3mice.r full run")
dat <- readr::read_csv("./data/data.csv")
dat <- dat[, -1] ## Remove "row_id"
# dim(dat) ## 1M x 80
# table(sapply(dat, class)) ## all numeric columns

## DEV size reduction ----
#dat <- dat[1:1E4, ]


## mice -----

## too many rows to view miss with md.pattern()

## browse method speed
methods <- "norm.predict" # Predictive mean matching
#c("pmm", "norm.predict", "norm", "mean")
mouse <- NA
## Do the imputation:
for(m in methods){
  tictoc::tic(paste0("mice: method=", m, ", 1-1 iter, ", nrow(dat), " rows."))
  mouse <- mice(dat,
                m = 1,
                maxit = 1,
                method = "rf", #c("pmm", "norm.predict", "norm", "mean")
                seed = 2022,
                printFlag = FALSE)
  tictoc::toc()
}
beepr::beep(4)

comp_dat <- complete(mouse)
sum(is.na(comp_dat)) == 0 ## nice
remove(mouse)

## mice: method=pmm, 1-1 iter, 1000000 rows: 1459.55 sec elapsed
#####---
##  10k, 80, 1-1 iter:   11 sec (1/6 min)
## 100k, 80, 1-1 iter:  535 sec (6   min)
##   1M, 80, 1-1 iter: 1443 sec (25  min)
#####---
## pmm; mice on 10k with pmm, 1-1 iter: 14.44 sec elapsed
## norm.predict; mice on 10k with pmm, 1-1 iter: 11.02 sec elapsed
## norm; mice on 10k with pmm, 1-1 iter: 10.84 sec elapsed
## mean; mice on 10k with pmm, 1-1 iter: 6.7 sec elapsed
#####---
##pmm; mice on 10k with pmm, 1-1 iter: 12.15 sec elapsed
##norm.predict; mice on 10k with pmm, 1-1 iter: 11.49 sec elapsed
##norm; mice on 10k with pmm, 1-1 iter: 11.23 sec elapsed
##mean; mice on 10k with pmm, 1-1 iter: 7.49 sec elapsed


## submission format -----
## The values of the missing
na_mat  <- is.na(dat)
value   <- comp_dat[na_mat]
resp_df <- data.frame(
  `row-col` = rep(NA, length(value)),
  value     = value ## I vetted matrix indexing works, NS
) 
## Solve the location of the missings
loc_mat <- matrix(character(80E6), nrow = 1E6, 80)
cn <- names(dat)
for(i in 1:80){
  loc_mat[, i] <- paste0(0:(1E6-1), "-", cn[i])
}
resp_df$`row-col` <- loc_mat[na_mat]


## Write file -----
readr::write_csv(resp_df, "last_submission.csv.gz")
tictoc::toc()
beepr::beep(4)

str(resp_df)
head(resp_df, 3)
message(paste0("Completed at ", Sys.time()))

## Cleanup -----
remove(dat)
remove(comp_dat)
remove(na_mat)
remove(loc_mat)
gc()

if(F){
  remove(resp_df)
  #rm(list = ls())
  gc() 
}

