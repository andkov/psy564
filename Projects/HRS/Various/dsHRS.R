# remove all elements for a clean start
rm(list=ls(all=TRUE))
cat("\014")


## @knitr InstallPackage
# source("./Scripts/Utility/InstallPackages.R")



## @knitr LoadPackages
require(sas7bdat) # for inputting data 
library(dplyr) # for general data manipulation
# library(reshape2) # for data : wide <-> long
# library(psych) # data summary + etc
library(ggplot2) # graphing
# library(nlme) # estimate fixed models | esp. gls()
# library(lme4) # estimate mixed models | esp. lmer()
# library(arm)  # process model objects


## @knitr LoadData
ds0 <- read.csv("~/GitHub/psy564/Data/Raw/HRS/HRS_WIDE_n2000.dat", header=F)
dim(ds0)

# namesare <- read.table("./Projects/HRS/HRS/namesare.txt", sep=" ")
namesarePath <- "./Projects/HRS/Various/namesare.txt"
namesare <- scan (namesarePath, what="character", sep=" ")
names(ds0) <- namesare

selectVars <- c()