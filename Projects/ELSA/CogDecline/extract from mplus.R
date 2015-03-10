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

source("./Scripts/Mplus/mplus.R")
ls()

m0 <- "./Projects/ELSA/CogDecline/irecall/irecall_0r_1r_NoCovar.gh5"

mplus.view.plots(m0)

a <- mplus.list.variables('./Projects/ELSA/CogDecline/irecall/irecall_0r_1r_NoCovar.gh5')
b <- mplus.get.data('./Projects/ELSA/CogDecline/irecall/irecall_0r_1r_NoCovar.gh5',variable)

mplus.plot.histogram(m0, "OUTCOME")
mplus.plot.histogram(m0, "TIMEL")
mplus.plot.histogram(m0, "LINEAR")
mplus.plot.histogram(m0, "B_OUTCOME")


b <- mplus.get.data('./Projects/ELSA/CogDecline/irecall/irecall_0r_1r_NoCovar.gh5',  "TIMEL" )


m1 <- "./Projects/ELSA/CogDecline/irecall/irecall_0r_1r_01age80_01edu11.gh5"

mplus.list.variables(m1)


a <- mplus.get.data(m1,"OUTCOME")
b <- mplus.get.data(m1,"TIMEL")
c <- mplus.get.data(m1,"EDU11")
d <- mplus.get.data(m1,"AGE80")
e <- mplus.get.data(m1,"LINEAR")
f <- mplus.get.data(m1,"B_OUTCOME")
g <- mplus.get.data(m1,"ID")

ds <- data.frame(cbind(a, b, c, d, e, f, g))
names(ds) <-  c("OUTCOME", "TIMEL","EDU11","AGE80","LINEAR","B_OUTCOME","ID")

head(ds)





