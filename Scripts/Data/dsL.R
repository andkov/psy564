# remove all elements for a clean start
rm(list=ls(all=TRUE))

## @knitr LoadPackages
require(sas7bdat)
require(Hmisc)

## @knitr LoadData
pathDir <- getwd()
pathFileSAS <- file.path(pathDir,"Chapters/02/SAS_Chapter2/SAS_Chapter2.sas7bdat")
pathFileSPSS <- file.path(pathDir,"Chapters/02/SPSS_Chapter2/SPSS_Chapter2.sav")

path_ds0 <- file.path(pathDir, "Data/Derived/02/ds0.Rds")
path_dsL <- file.path(pathDir, "Data/Derived/02/dsL.Rds")


ds0SAS <- read.sas7bdat(pathFileSAS, debug=TRUE) # Use this when running for the first time
ds0SPSS <- Hmisc::spss.get(pathFileSPSS, use.value.labels = TRUE)

saveRDS(object=ds0, file=path_ds0, compress="xz")   # Use this when running for the first time
### Either use ds0 definition above or below.
ds0<-readRDS(path_ds0) # This saves time              # Use for subsequent run
