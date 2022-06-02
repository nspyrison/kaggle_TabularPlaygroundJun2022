if(F) ## Following:
  browseURL("https://datascienceplus.com/imputing-missing-data-with-r-mice-package/")

#install.packages("mice")


## Data ----
# Add some missingness for more realistic situation
data <- airquality
data[4:10,3] <- rep(NA,7)
data[1:5,4] <- NA
# Remove a couple columns and summarize
data <- data[-c(5,6)]
summary(data)


## Look at missingness by col/row
pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(data, 2, pMiss) ## Pct missing by Col
apply(data, 1, pMiss) ## Pct missing by Row


## mice -----
library(mice)
## Sorted count table, of model matrix (unique missingness types)
md.pattern(data, rotate.names = TRUE)


## Another two visual takes at the same idea
library(VIM)
aggr_plot <- aggr(
  data, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE,
  labels=names(data), cex.axis=.7, gap=3, 
  ylab=c("Histogram of missing data","Pattern"))
marginplot(data[c(1,2)])


## Doing the imputation by predictive mean matching (pmm)
tempData <- mice(data,m=5,maxit=50,meth='pmm',seed=2022.)
summary(tempData)
str(tempData)

methods(mice) ## possible methods we could use


## getting back to complete data:
completedData <- complete(tempData)
summary(completedData)
