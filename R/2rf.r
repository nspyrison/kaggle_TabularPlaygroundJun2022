# tabular playground, Jun 2022 -----
######|
## https://www.kaggle.com/competitions/tabular-playground-series-jun-2022/data
## Ok, see the results of 1eda.r, 
## then continuing on here to get baseline RF models 
######|

## Read ----
library(ranger) ## iirc ranger fit faster than randomForest in cheem testing
dat <- readr::read_csv("./data/data.csv")
dat <- dat[, -1] ## Remove "row_id"
# dim(dat) ## 1M x 80
# table(sapply(dat, class)) ## all numeric columns


## Index -----
na_mat <- is.na(dat)
idx_col_to_pred <- apply(na_mat, 2, sum) ## some have 0 missing, most have about 18k missing
idx_col_to_pred <- which(idx_col_to_pred > 0)
cn_to_pred <- names(idx_col_to_pred)
length(idx_col_to_pred)
message("Keep in mind we aren't fitting just 1 variable, but 55 of them.")

## Fit ----
#?ranger::ranger()
hp_ntree    = 10 #log(nrow(dat))^2 ## 190.9  #cheem was 125,
hp_mtry     = log2(nrow(dat))  ## 19.93 #cheem was ifelse(is_discrete(y), sqrt(ncol(x)), ncol(x)/3),
hp_nodesize = 5 ## ranger: "5 for regression" #cheem was max(ifelse(is_discrete(y), 1, 5), nrow(x)/500)
model_ls <- vector(mode = "list", length = sum(idx_col_to_pred))
resp_df  <- data.frame(`row-col` = rep(NA, 1E6), 
                       value = rep(NA, 1E6))
oos <- NULL
for(i in seq_along(idx_col_to_pred)){
  col_num <- idx_col_to_pred[i]
  idx_rows_na <- na_mat[, col_num]
  
  ### Model ----
  model_ls[i] <- ranger::ranger(formula       = paste0(cn_to_pred[i], " ~ ."),
                                data          = dat,
                                num.trees     = hp_ntree,
                                mtry          = hp_mtry,
                                min.node.size = hp_nodesize)
  
  ### Predict ----
  pred <- predict(model_ls[i], dat[idx_rows_na, col_num])
  dat[idx_rows_na, col_num] <- pred ## Backfill NA with predictions
}


library(ranger)
rf <- ranger(mpg ~ ., mtcars[1:26, ])
pred <- predict(rf, mtcars[27:32, ])
pred$predictions
