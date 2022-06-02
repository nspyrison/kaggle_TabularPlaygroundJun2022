# tabular playground, Jun 2022 -----
######|
## https://www.kaggle.com/competitions/tabular-playground-series-jun-2022/data
## From data of 1M x 81 our goal is to predict the missing (NA) quantitative
## values given the rest of the data. Description says categorical, but all NUM.
######|

## read ----
library(tidyverse)
dat <- read_csv("./data/data.csv")
if(interactive()){
  dim(dat) ## 1M x 81
  table(sapply(dat, class)) ## all numeric columns
  names(dat)
}
dat <- dat[,-1] ## Remove "row_id"

## View missingness ----
if(interactive()){
  idxmat_datNA <- is.na(dat) %>% as.matrix()
  sum(idxmat_datNA) ## 1M missing total
  count_NA_bycol <- apply(idxmat_datNA, 2, sum) ## some have 0 missing, most have about 18k missing
  hist(count_NA_bycol)
  hist(count_NA_bycol[count_NA_bycol != 0]) ## ~ N(18k, 130)
}

## Correlations ----
if(interactive()){
  # install.packages("WGCNA")
  # BiocManager::install(version = '3.14') ## wants to update 240 packages...
  # BiocManager::install("GO.db")
  # BiocManager::install("impute")
  tictoc::tic("WGCNA::corAndPvalue(dat, method='pearson')")
  cormat <- WGCNA::corAndPvalue(dat, method='pearson')
  tictoc::toc() ## ~ 28 seconds
  corrplot::corrplot(cormat$cor, type = "upper", order = "hclust",
                     tl.col = "black", tl.srt = 45)
}
message("Wow not a lot there, as a linear realationship atleast.")
message("Not sure how good models are going to be able predict values with this,")
message("but let's try some trivial rf anyway.")