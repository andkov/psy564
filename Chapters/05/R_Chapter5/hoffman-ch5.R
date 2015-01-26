# remove all elements for a clean start
rm(list=ls(all=TRUE))
# Supplementary Material for Longitudinal Analysis: Modeling Within-Person Fluctuation and Change 
# Chapter 2: MPLUS Syntax and Output by Model

## @knitr InstallPackage
# source("./Scripts/Utility/InstallPackages.R")

## @knitr LoadPackages
require(sas7bdat) # for inputting data 
library(dplyr) # for general data manipulation
library(reshape2) # for data : wide <-> long
library(psych) # data summary + etc
library(ggplot2) # graphing
library(nlme) # estimate mixed models | esp. gls()
library(lme4) # estimate mixed models | esp. lmer()
library(arm)  # process model objects

## @knitr LoadData
pathDir  <- getwd() # get working directory, e.i. residence of .Rproj file
pathFile  <- file.path(pathDir,"Chapters/05/SAS_Chapter5/SAS_Chapter5.sas7bdat") # location of the file
ds0   <- read.sas7bdat(pathFile, debug=TRUE) # import file 
ds0 <- data.frame(ds0) # save as a data frame 


## @knitr BasicDescriptive
class(ds0) # what class?
dim(ds0)  # what dimensions?
names(ds0) # what are column names?
str(ds0) # what its structure?
head(ds0) # what do first few lines look like?
base::summary(ds0) # basic summary
psych::describe(ds0) # summary by psych package
table(ds0$wave)# one-way table


## @knitr StackData
dsL <- ds0

## @knitr TweakLong
dsM <- dsL

#### Basic Graphs  ####

## @knitr GraphingData0
p <- ggplot2::ggplot(dsM,aes(x=wave,y=outcome)) # map data dimension
p <- p + geom_line(aes(group=PersonID)) # draw lines and map unit of measurement
p

## @knitr LoadGraphThemes
baseSize <- 12 # set as the point of further reference
theme1 <- ggplot2::theme_bw(base_size=baseSize) +
  ggplot2::theme(title=ggplot2::element_text(colour="gray20",size = baseSize+1)) +
  ggplot2::theme(axis.text=ggplot2::element_text(colour="gray40", size=baseSize-2)) +
  ggplot2::theme(axis.title=ggplot2::element_text(colour="gray40")) +
  ggplot2::theme(panel.border = ggplot2::element_rect(colour="gray80")) +
  ggplot2::theme(axis.ticks.length = grid::unit(0, "cm")) +
  ggplot2::theme(text = element_text(size =baseSize+7)) 

## @knitr GraphingData1
p <- ggplot2::ggplot(dsM,aes(x=wave,y=outcome))
p <- p + geom_line(aes(group=PersonID)) # draw lines
p <- p + theme1 
p <- p + scale_x_continuous(limits=c(1,4), breaks=c(1:4)) # X axis
p <- p + scale_y_continuous(limits=c(5,25), 
                            breaks=seq(5,25, by=5)) # Y axis
p <- p + labs(list(
  title="Does outcome change over time?", # main title
  x="Wave of measurement", y="Performance on the outcome")) # axes titles
p


## @knitr GraphingData2
p <- ggplot2::ggplot(dsM,aes(x=wave,y=outcome))
p <- p + geom_line(aes(group=PersonID)) 
p <- p + geom_point(size=6, shape=21, fill="purple", color="black", alpha=.5)
p <- p + theme1 
p <- p + scale_x_continuous(limits=c(1,4), breaks=c(1:4)) # X axis
p <- p + scale_y_continuous(limits=c(5,25), 
                            breaks=seq(5,25, by=5)) # Y axis
p <- p + labs(list(
  title="Does outcome change over time?", # main title
  x="Wave of measurement", y="Performance on the outcome")) # axes titles
p

## @knitr dummyChunk
####  MODEL 3.1  ####

## @knitr RunM3_1
m3.1 <- nlme::gls(outcome ~ 1, data=dsM, method="ML") # create model object
dsM$m3.1 <- predict(m3.1) # stores values predicted by the model
summary(m3.1) # print a bit more info
# str(summary(m3.1)) # to inspect object directly



## @knitr InspectM3_1
model  <- m3.1 # rename object for generic use
logLik <- summary(model)$logLik # extract log likelihood
deviance <- -2*logLik # extract deviance
AIC <- AIC(model) # extract Akaike information criterion
BIC <- BIC(model) # extract Bayesian information criterion
df.resid <- NA # empty slot for later use
N <- summary(model)$dims$N  # Number of distinct data points
p <- summary(model)$dims$p  # Number of estimated parameters
ids <- length(unique(dsM$PersonID)) # Number of unique units
df.resid <- N-p # residual degrees of freedom
mInfo <- data.frame("logLik" = logLik,   # collect model information indo a dataframe
                    "deviance"= deviance, 
                    "AIC" = AIC, "BIC" = BIC,
                    "df.resid" = df.resid, "N" = N, 
                    "p" = p, "ids" = ids)
t<- t(mInfo) # transpose
rownames(t)<-colnames(mInfo) # rename rows
mInfo<- data.frame(new=t) # turn into a dataframe
colnames(mInfo) <- c("m3.1") # rename columns
mi3.1 <- mInfo # save (m)odel (o)utput of model (3.1)
mi3.1$Coefficient <- rownames(mi3.1) # create a column with the name of each index
m3.1 #  model results
mi3.1 #  model information
head(dsM) # visual check


## @knitr GraphM3_1
p <- ggplot2::ggplot(dsM,aes(x=wave, y=outcome))
p <- p + geom_line(aes(group=PersonID), color="firebrick", alpha=.5)  # individual trajectories
p <- p + geom_point(size=3, shape=21, fill=NA, color="black", alpha=.4) # cross-section data points
p <- p + geom_line(aes(y=m3.1, group=PersonID), color="royalblue3", size=3, alpha=.05) # modelled data
p <- p + theme1 
p <- p + scale_x_continuous(limits=c(1,4), breaks=c(1:4)) # X axis
p <- p + scale_y_continuous(limits=c(5,25), 
                            breaks=seq(5,25, by=1)) # Y axis
p <- p + labs(list(
  title="Does outcome change over time?", # main title
  x="Wave of measurement", y="Performance on the outcome")) # axes titles
p

## @knitr dummyChunk
####  MODEL 5.1  ####

## @knitr RunM5_1
m5.1 <- lme4::lmer(outcome ~ 1 + (1 | PersonID), data=dsM, REML=FALSE)# create model object
dsM$m5.1 <- predict(m5.1) # stores values predicted by the model
display(m5.1) # tidy results

## @knitr InspectM5_1
model <- m5.1
mInfo<-summary(model)$AICtab
mInfo["N"]<- model@devcomp$dims["N"] # number of datapoints, verify
mInfo["p"]<- model@devcomp$dims["p"] # number of estimated parameters, verify
mInfo["ids"]<- (summary(model))$ngrps # number of units on level-2, here: individuals
# mInfo<- c(mInfo, "modelName"=modelName)
mInfo<-data.frame(mInfo) # turn into a dataframe
mInfo<- plyr::rename(mInfo,replace= c("mInfo"="m5.1")) # rename variables
mInfo$Coefficient <- rownames(mInfo) # save index names as a column
mi5.1 <- mInfo # create model information  object
display(m5.1)
mi5.1 #  model information
head(dsM) # visual check

## @knitr GraphM5_1
p <- ggplot2::ggplot(dsM,aes(x=wave, y=outcome))
p <- p + geom_line(aes(group=PersonID), color="firebrick", alpha=.2)  # individual trajectories
p <- p + geom_point(size=3, shape=21, fill=NA, color="black", alpha=.6) # cross-section data points
p <- p + geom_line(aes(y=m5.1, group=PersonID), color="royalblue3", alpha=.7) # modelled data
p <- p + theme1 
p <- p + scale_x_continuous(limits=c(1,4), breaks=c(1:4)) # X axis
p <- p + scale_y_continuous(limits=c(5,25), 
                            breaks=seq(5,25, by=1)) # Y axis
p <- p + labs(list(
  title="Does outcome change over time?", # main title
  x="Wave of measurement", y="Performance on the outcome")) # axes titles
p

## @knitr dummyChunk
####  MODEL 5.3  ####

## @knitr RunM5_3
m5.3 <- lme4::lmer(outcome ~ 1 + wave + (1 | PersonID), data=dsM, REML=FALSE)# create model object
dsM$m5.3 <- predict(m5.3) # stores values predicted by the model
display(m5.3) # tidy results

## @knitr InspectM5_3
model <- m5.3
mInfo<-summary(model)$AICtab
mInfo["N"]<- model@devcomp$dims["N"] # number of datapoints, verify
mInfo["p"]<- model@devcomp$dims["p"] # number of estimated parameters, verify
mInfo["ids"]<- (summary(model))$ngrps # number of units on level-2, here: individuals
# mInfo<- c(mInfo, "modelName"=modelName)
mInfo<-data.frame(mInfo) # turn into a dataframe
mInfo<- plyr::rename(mInfo,replace= c("mInfo"="m5.3")) # rename variables
mInfo$Coefficient <- rownames(mInfo) # save index names as a column
mi5.3 <- mInfo # create model information  object
display(m5.3)
mi5.3 #  model information
head(dsM) # visual check

## @knitr GraphM5_3
p <- ggplot2::ggplot(dsM,aes(x=wave, y=outcome))
p <- p + geom_line(aes(group=PersonID), color="firebrick", alpha=.2)  # individual trajectories
p <- p + geom_point(size=3, shape=21, fill=NA, color="black", alpha=.6) # cross-section data points
p <- p + geom_line(aes(y=m5.3, group=PersonID), color="royalblue3", alpha=.7) # modelled data
p <- p + theme1 
p <- p + scale_x_continuous(limits=c(1,4), breaks=c(1:4)) # X axis
p <- p + scale_y_continuous(limits=c(5,25), 
                            breaks=seq(5,25, by=1)) # Y axis
p <- p + labs(list(
  title="Does outcome change over time?", # main title
  x="Wave of measurement", y="Performance on the outcome")) # axes titles
p

## @knitr dummyChunk
####  MODEL 5.5  ####

## @knitr RunM5_5
m5.5 <- lme4::lmer(outcome ~ 1 + wave + (1 + wave | PersonID), data=dsM, REML=FALSE)# create model object
dsM$m5.5 <- predict(m5.5) # stores values predicted by the model
display(m5.5) # tidy results

## @knitr InspectM5_5
model <- m5.5
mInfo<-summary(model)$AICtab
mInfo["N"]<- model@devcomp$dims["N"] # number of datapoints, verify
mInfo["p"]<- model@devcomp$dims["p"] # number of estimated parameters, verify
mInfo["ids"]<- (summary(model))$ngrps # number of units on level-2, here: individuals
# mInfo<- c(mInfo, "modelName"=modelName)
mInfo<-data.frame(mInfo) # turn into a dataframe
mInfo<- plyr::rename(mInfo,replace= c("mInfo"="m5.5")) # rename variables
mInfo$Coefficient <- rownames(mInfo) # save index names as a column
mi5.5 <- mInfo # create model information  object
display(m5.5)
mi5.5 #  model information
head(dsM) # visual check

## @knitr GraphM5_5
p <- ggplot2::ggplot(dsM,aes(x=wave, y=outcome))
p <- p + geom_line(aes(group=PersonID), color="firebrick", alpha=.2)  # individual trajectories
p <- p + geom_point(size=3, shape=21, fill=NA, color="black", alpha=.6) # cross-section data points
p <- p + geom_line(aes(y=m5.5, group=PersonID), color="royalblue3", alpha=.7) # modelled data
p <- p + theme1 
p <- p + scale_x_continuous(limits=c(1,4), breaks=c(1:4)) # X axis
p <- p + scale_y_continuous(limits=c(5,25), 
                            breaks=seq(5,25, by=1)) # Y axis
p <- p + labs(list(
  title="Does outcome change over time?", # main title
  x="Wave of measurement", y="Performance on the outcome")) # axes titles
p