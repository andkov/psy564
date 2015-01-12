# remove all elements for a clean start
rm(list=ls(all=TRUE))
# Supplementary Material for Longitudinal Analysis: Modeling Within-Person Fluctuation and Change 
# Chapter 2: MPLUS Syntax and Output by Model

## @knitr LoadPackages
require(sas7bdat)
library(nlme)


## @knitr LoadData
pathDir  <- getwd()
pathCh2  <- file.path(pathDir,"Chapters/02/SAS_Chapter2/SAS_Chapter2.sas7bdat")
dsL2   <- read.sas7bdat(pathCh2, debug=TRUE) 
ds <- dsL2
names(ds)
str(ds)
table(ds$sexMW, ds$demgroup)


## @knitr EmptyMeansModel
m0 <- nlme::gls(cognition ~ 1, data=ds, method="ML")
summary(m0)
ds$m0 <- predict(m0)


## @knitr AddingAge(0=85)
ds$age85 <- ds$age - 85
# m1 <- nlme::gls(cognition ~ 1 + age85, data=ds, method="ML")
m1 <- stats::glm(cognition ~ 1 + age85, data=ds)
summary(m1)
ds$m1 <- predict(m1)


model <- m1
logLik<- summary(model)$logLik
deviance<- -2*logLik
AIC<- AIC(model)
BIC<- BIC(model)
df.resid<- NA
N<- summary(model)$dims$N
p<- summary(model)$dims$p
ids<- length(unique(ds$PersonID))
df.resid<- N-p
mInfo<- data.frame("logLik" = logLik, 
                   "deviance"= deviance, 
                   "AIC" = AIC, "BIC" = BIC,
                   "df.resid" = df.resid, "N" = N, 
                   "p" = p, "ids" = ids)
t<- t(mInfo)
rownames(t)<-colnames(mInfo)
dsmInfo<- data.frame(new=t)
colnames(dsmInfo) <- c("modelA")
# dsmInfo$Coefficient <- rownames(dsmInfo)
mA <- dsmInfo
print(mA)
