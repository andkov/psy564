# remove all elements for a clean start
rm(list=ls(all=TRUE))

## @knitr LoadPackages
require(sas7bdat)

## @knitr LoadData
pathDir  <- getwd()
pathCh2  <- file.path(pathDir,"Chapters/02/SAS_Chapter2/SAS_Chapter2.sas7bdat")
dsL2   <- read.sas7bdat(pathCh2, debug=TRUE) 
ds <- dsL2

names(ds)
str(ds)
table(ds$sexMW, ds$demgroup)


library(nlme)
m0 <- gls(cognition ~ 1,data=ds,method="ML")
ds$m0 <- predict(m0)
summary(m0)
