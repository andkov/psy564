# remove all elements for a clean start
rm(list=ls(all=TRUE))

## @knitr LoadPackages
require(sas7bdat)

## @knitr LoadData
pathDir  <- getwd()
pathCh2  <- file.path(pathDir,"Chapters/02/SAS_Chapter2/SAS_Chapter2.sas7bdat")
pathCh3a <- file.path(pathDir,"Chapters/03/SAS_Chapter3a/SAS_Chapter3a.sas7bdat")
pathCh3b <- file.path(pathDir,"Chapters/03/SAS_Chapter3b/SAS_Chapter3b.sas7bdat")
pathCh4  <- file.path(pathDir,"Chapters/04/SAS_Chapter4/SAS_Chapter4.sas7bdat")
pathCh5  <- file.path(pathDir,"Chapters/05/SAS_Chapter4/SAS_Chapter5.sas7bdat")
pathCh6  <- file.path(pathDir,"Chapters/06/SAS_Chapter4/SAS_Chapter6.sas7bdat")
pathCh7a <- file.path(pathDir,"Chapters/07/SAS_Chapter7a/SAS_Chapter7a.sas7bdat")
pathCh7b <- file.path(pathDir,"Chapters/07/SAS_Chapter7b/SAS_Chapter7b.sas7bdat")
pathCh8  <- file.path(pathDir,"Chapters/08/SAS_Chapter8/SAS_Chapter8.sas7bdat")
pathCh9  <- file.path(pathDir,"Chapters/09/SAS_Chapter9/SAS_Chapter9.sas7bdat")
pathCh10a <- file.path(pathDir,"Chapters/10/SAS_Chapter10a/SAS_Chapter10a.sas7bdat")
pathCh10b <- file.path(pathDir,"Chapters/10/SAS_Chapter10b/SAS_Chapter10b.sas7bdat")
pathCh11a <- file.path(pathDir,"Chapters/11/SAS_Chapter11a/SAS_Chapter11a.sas7bdat")
pathCh11b <- file.path(pathDir,"Chapters/11/SAS_Chapter11b/SAS_Chapter11b.sas7bdat")
pathCh12  <- file.path(pathDir,"Chapters/12/SAS_Chapter12/SAS_Chapter12.sas7bdat")



dsL2   <- read.sas7bdat(pathCh2, debug=TRUE) 
dsL3a  <- read.sas7bdat(pathCh3a, debug=TRUE)
dsL3b  <- read.sas7bdat(pathCh3b, debug=TRUE)
dsL4   <- read.sas7bdat(pathCh4, debug=TRUE) 
dsL5   <- read.sas7bdat(pathCh5, debug=TRUE) 
dsL6   <- read.sas7bdat(pathCh6, debug=TRUE) 
dsL7a  <- read.sas7bdat(pathCh7a, debug=TRUE)
dsL7b  <- read.sas7bdat(pathCh7b, debug=TRUE)
dsL8   <- read.sas7bdat(pathCh8, debug=TRUE) 
dsL9   <- read.sas7bdat(pathCh9, debug=TRUE) 
dsL10a <- read.sas7bdat(pathCh10a, debug=TRUE)
dsL10b <- read.sas7bdat(pathCh10b, debug=TRUE)
dsL11a <- read.sas7bdat(pathCh11a, debug=TRUE)
dsL11b <- read.sas7bdat(pathCh11b, debug=TRUE)
dsL12  <- read.sas7bdat(pathCh12, debug=TRUE) 

# # path_dsL2 <- file.path(pathDir, "Data/Derived/02/dsL.Rds")
# saveRDS(object=ds0, file=path_ds0, compress="xz")   # Use this when running for the first time
# ### Either use ds0 definition above or below.
# ds0<-readRDS(path_ds0) # This saves time              # Use for subsequent run
