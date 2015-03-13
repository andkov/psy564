# remove all elements for a clean start
rm(list=ls(all=TRUE))
cat("\014")

## @knitr LoadPackages
require(sas7bdat) # for inputting data 
library(dplyr) # for general data manipulation
library(reshape2) # for data : wide <-> long
library(psych) # data summary + etc
library(ggplot2) # graphing
library(nlme) # estimate fixed models | esp. gls()
library(lme4) # estimate mixed models | esp. lmer()
library(arm)  # process model objects

# source("./Scripts/Mplus/mplus.R")
source("http://www.statmodel.com/mplus-R/mplus.R")
ls() # list the availible function in mplus.R

m0 <- "./Projects/ELSA/CogDecline/irecall/irecall_r(i)_r(s)_nocovar.gh5"
m1 <- "./Projects/ELSA/CogDecline/irecall/irecall_r(i)_r(s)_(is)age80.gh5"
m2 <- "./Projects/ELSA/CogDecline/irecall/irecall_r(i)_r(s)_(is)age80_(is)edu11.gh5"
m3 <- "./Projects/ELSA/CogDecline/irecall/irecall_r(i)_r(s)_(is)age80_(is)edu11_(is)smoked.gh5"


mplus.view.plots(m3)
model <- m3

mplus.list.variables(m3)

mplus.plot.histogram(model, "OUTCOME")
mplus.plot.histogram(model, "TIMEL")
mplus.plot.histogram(model, "EVRSMK1A")
mplus.plot.histogram(model, "EDU11")
mplus.plot.histogram(model, "AGE80")
mplus.plot.histogram(model, "LINEAR")
mplus.plot.histogram(model, "B_OUTCOME")

# extract raw data
a <- mplus.get.data(m2,"OUTCOME")
b <- mplus.get.data(m2,"TIMEL")
c <- mplus.get.data(m2,"EDU11")
d <- mplus.get.data(m2,"AGE80")
e <- mplus.get.data(m2,"LINEAR")
f <- mplus.get.data(m2,"B_OUTCOME")
g <- mplus.get.data(m2,"ID")

ds <- data.frame(cbind(a, b, c, d, e, f, g))
names(ds) <-  c("OUTCOME", "TIMEL","EDU11","AGE80","LINEAR","B_OUTCOME","ID")

head(ds)



######   ########
mplus.load(m1)
mplus.plot.loop(m1, ypred)



