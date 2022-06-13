# tabular playground, Jun 2022 -----
######|
## https://www.kaggle.com/competitions/tabular-playground-series-jun-2022/data
## See the results of 1eda.r, 
## then continuing on here trying to apply the mice package.
######|
## It was a good first run I am on the Kaggle score board ~ 50q.
## Data doesn't seem realistic, so methods like RF didn't do near 
## as well as it would would expect say for related company situation.
## 
## Last/best attempt: mice package "norm.predict" methof with m=2 iterations: 
## 1.08555, q~=.5; not worth the squeeze with this methof; 
## Judging by leading notebooks: next improvement would be LGBM &| LAMA
## I do mention LGBM in my answers to such questions.
######|



methods <- "norm.predict" 
M <- 2
MAXIT <- 1
#c("pmm", "norm.predict", "norm", "mean")
message(paste0("Started run: ", methods, " at ", Sys.time()))

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


## mice -----
## too many rows to view miss with md.pattern()
mouse <- NA
## Do the imputation:
for(m in methods){
  tictoc::tic(paste0(
    "mice: method=", m, ", ", M,"-",MAXIT," iter, ", nrow(dat), " rows."))
  mouse <- mice::mice(dat,
                      m = M,
                      maxit = MAXIT,
                      method = methods, #c("pmm", "norm.predict", "norm", "mean")
                      seed = 2022,
                      printFlag = FALSE)
  tictoc::toc()
}
beepr::beep(1)

if(exists("mouse") == FALSE){
  message("!!! mouse does not exist !!!")
} else{
  #message("mouse did exist ^^")
  comp_dat <- mice::complete(mouse)
  sum(is.na(comp_dat)) == 0 ## nice
  remove(mouse)

  
  
  ## submission format -----
  ## The values of the missing
  na_mat  <- is.na(dat)
  value   <- comp_dat[na_mat]
  
  ## See if we can reuse last data
  last_sub <- readr::read_csv("last_submission.csv.gz")
  if(all(dim(last_sub) == c(1E6, 2))){
    last_sub$value <- value
    resp_df <- last_sub
  } else {
    ## Else create from scrath
    ### create `row-col` from scratch -----
    resp_df <- data.frame(
      `row-col`   = rep(NA, length(value)),
      value       = value, ## I vetted matrix indexing works, NS
      check.names = FALSE
    )
    ## Solve the location of the missingness
    loc_mat <- matrix(character(80E6), nrow = 1E6, 80)
    cn <- names(dat)
    for(i in 1:80){
      loc_mat[, i] <- paste0(0:(1E6-1), "-", cn[i])
    }
    resp_df$`row-col` <- loc_mat[na_mat]
    remove(loc_mat)
  }
  
  ## Write file -----
  str(resp_df)
  readr::write_csv(resp_df, "last_submission.csv.gz")
  
  ## Cleanup -----
  remove(dat)
  remove(comp_dat)
  remove(na_mat)
  gc()
  
  message(paste0("Completed run: ", methods, " at ", Sys.time()))
  tictoc::toc()
  beepr::beep(4)
}

# Kaggle scores ----

# kaggle score:
# pmm: 1.47398
# rf: 1.61458
# mean: 1.41613
# norm:  1.52337
# norm.predict: 1.08933
# norm.predict m=2: 1.08555; not worth the squeeze; next improvement would be LGBM &| LAMA
#####---
## mice: method=mean, 1-1 iter, 1000000 rows.: 772.57 sec elapsed
## mice: method=rf, 1-1 iter, 1000000 rows.: 25845.59 sec elapsed (7.17 hrs)
## mice: method=pmm, 1-1 iter, 1000000 rows: 1459.55 sec elapsed
## mice: method=norm, 1-1 iter, 1000000 rows.: 1644.06 sec elapsed
## mice: method=norm.predict, 1-1 iter, 1000000 rows.: 1688.79 sec elapsed
## mice: method=norm.predict, 2-1 iter, 1000000 rows.: 2776.64 sec elapsed


# Benchmarking mice ----

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
## pmm; mice on 10k with pmm, 1-1 iter: 12.15 sec elapsed
## norm.predict; mice on 10k with pmm, 1-1 iter: 11.49 sec elapsed
## norm; mice on 10k with pmm, 1-1 iter: 11.23 sec elapsed
## mean; mice on 10k with pmm, 1-1 iter: 7.49 sec elapsed