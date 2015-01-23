# remove all elements for a clean start
rm(list=ls(all=TRUE))
# Supplementary Material for Longitudinal Analysis: Modeling Within-Person Fluctuation and Change 
# Chapter 2: MPLUS Syntax and Output by Model

## @knitr LoadPackages
require(sas7bdat)
library(nlme)
library(dplyr)
library(reshape2)
library(ggplot2)
library(psych)

## @knitr LoadData
pathDir  <- getwd() # get working directory, e.i. residence of .Rproj file
pathFile  <- file.path(pathDir,"Chapters/03/SAS_Chapter3a/SAS_Chapter3a.sas7bdat") # location of the file
ch3a   <- read.sas7bdat(pathFile, debug=TRUE) # import file 
ch3a <- data.frame(ch3a) # save as a data frame 





## @knitr BasicDescriptive
class(ch3a) # what class?
dim(ch3a)  # what dimensions?
names(ch3a) # what are column names?
str(ch3a) # what its structure?
head(ch3a) # what do first few lines look like?
summary(ch3a) # basic summary
psych::describe(ch3a) # summary by psych package
table(ch3a$group)# one-way table


## @knitr StackData

# stack data into a new dataset
dsLong <- reshape2::melt(ch3a,id.vars=c("PersonID","group")) # id.vars are those NOT STACKED
head(dsLong) # inspect
dsLong <- dsLong[order(dsLong$PersonID, dsLong$variable),] # sort for visual inspection
head(dsLong) # inspect



## @knitr TweakLong1
# substitute the repeating string in column "variable" by nothing
dsLong$variable <- gsub(pattern="outcome", replacement='', x=dsLong$variable) 
head(dsLong)
dsLong <- plyr::rename(dsLong, c("variable"="time", "value"="outcome")) # rename with plyr package
# Alternatively: rename and order variables with dplyr package
# dsLong <- dplyr::select(dsLong, id=PersonID, group,time=variable, outcome=value)
head(dsLong)
ds <- dsLong # save with a more convenient name

## @knitr TweakLong2
ds$group <- ordered(ds$group, levels = c(1,2),
                              labels = c("Control","Treatment"))

ds$time <- ordered(ds$time, levels = c(1,2),
                            labels = c("Pre-Test","Post-Test"))
str(ds)

## @knitr GraphingData0
p <- ggplot2::ggplot(ds,aes(x=time,y=outcome)) # map data dimension
p <- p + geom_line(aes(group=PersonID)) # draw lines and map unit of measurement
p

## @knitr GraphingData1
p <- ggplot2::ggplot(ds,aes(x=time,y=outcome))
p <- p + geom_line(aes(group=PersonID, color=group)) # map color
p <- p + scale_x_discrete(labels=c("Pre-Test","Post-Test")) # X axis
p <- p + scale_y_continuous(limits=c(30,70), 
                            breaks=seq(30,70, by=5)) # Y axis
p <- p + labs(list(
  title="Does treatment affect test performance?", # main title
  x="Times of observation", y="Test score")) # axes titles
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


## @knitr GraphingData2
p <- ggplot2::ggplot(ds,aes(x=time,y=outcome))
p <- p + geom_line(aes(group=PersonID, color=group))
p <- p + theme1 # add a graph theme
p <- p + scale_x_discrete(labels=c("Pre-Test","Post-Test"))
p <- p + scale_y_continuous(limits=c(30,70), 
                            breaks=seq(30,70, by=5))
p <- p + labs(list(
  title="Does treatment affect test performance?",
  x="Times of observation", y="Test score"))
p

## @knitr GraphingData3
p <- ggplot2::ggplot(ds,aes(x=time,y=outcome))
p <- p + geom_line(aes(group=PersonID, color=group))
p <- p + theme1
p <- p + scale_x_discrete(labels=c("Pre-Test","Post-Test"))
p <- p + scale_y_continuous(limits=c(30,70), 
                            breaks=seq(30,70, by=5))
p <- p + facet_grid(.~group) # place groups on separate graphs
p <- p + labs(list(
  title="Does treatment affect test performance?",
  x="Times of observation", y="Test score"))
p



## @knitr EmptyBP1
m3.1 <- nlme::gls(outcome ~ 1, data=ds, method="ML") # create model object
m3.1 # print basic info
summary(m3.1) # print a bit more info
# str(summary(m3.1)) # will get you all the elements of this model object, so you can extract what you need
ds$m3.1 <- predict(m3.1) # stores values predicted by the model
var(ds$outcome - ds$m3.1) # variance of the residual computed directly
sd(ds$outcome - ds$m3.1) # standard deviation of the residual computed directly


## @knitr EmptyBP2
model  <- m3.1 # rename object for generic use
logLik <- summary(model)$logLik # extract log likelihood
deviance <- -2*logLik # extract deviance
AIC <- AIC(model) # extract Akaike information criterion
BIC <- BIC(model) # extract Bayesian information criterion
df.resid <- NA # empty slot for later use
N <- summary(model)$dims$N  # Number of distinct data points
p <- summary(model)$dims$p  # Number of estimated parameters
ids <- length(unique(ds$PersonID)) # Number of unique units
df.resid <- N-p # residual degrees of freedom
mInfo <- data.frame("logLik" = logLik,   # collect model information indo a dataframe
                   "deviance"= deviance, 
                   "AIC" = AIC, "BIC" = BIC,
                   "df.resid" = df.resid, "N" = N, 
                   "p" = p, "ids" = ids)
t<- t(mInfo) # transpose
rownames(t)<-colnames(mInfo) # rename rows
dsmInfo<- data.frame(new=t) # save as dataframe
colnames(dsmInfo) <- c("m3.1") # rename columns
mo3.1 <- dsmInfo # save (m)odel (o)utput of model (3.1)
mo3.1$Coefficient <- rownames(mo3.1) # create a column with the name of each index
m3.1 # model results
mo3.1 # model performance


## @knitr EmptyBP3
head(ds)

## @knitr EmptyBP4
p <- ggplot2::ggplot(ds,aes(x=time,y=m3.1)) # replace the outcome with model prediction
p <- p + geom_line(aes(group=PersonID))
p <- p + theme1
p <- p + scale_x_discrete(labels=c("Pre-Test","Post-Test"))
p <- p + scale_y_continuous(limits=c(30,70), 
                            breaks=seq(30,70, by=5))
p <- p + facet_grid(.~group) 
p <- p + labs(list(
  title="Does treatment affect test performance?",
  x="Times of observation", y="Test score"))
p

## @knitr EmptyBP5
p <- ggplot2::ggplot(ds,aes(x=time,y=outcome, group=PersonID))  # mapping for entire plot
p <- p + geom_line(aes(color=group)) # geom specific mapping
p <- p + geom_line(aes(y=m3.1))      # geom specific mapping
p <- p + theme1
p <- p + scale_x_discrete(labels=c("Pre-Test","Post-Test"))
p <- p + scale_y_continuous(limits=c(30,70), 
                            breaks=seq(30,70, by=5))
p <- p + facet_grid(.~group) 
p <- p + labs(list(
  title="Does treatment affect test performance?",
  x="Times of observation", y="Test score"))
p




## @knitr EmptyWP1
m3.2 <- lme4::lmer(outcome ~ 1 + (1 | PersonID), data=ds)# create model object
m3.2 # print basic info
summary(m3.2) # print a bit more info
ds$m3.2 <- predict(m3.2) # stores values predicted by the model
var(ds$outcome - ds$m3.2) # variance of the residual computed directly
sd(ds$outcome - ds$m3.2) # standard deviation of the residual computed directly

ds$dm1 <- ds$outcome - ds$m3.1
ds$dm2 <- ds$outcome - ds$m3.2
head(ds)
sum(ds$dm1)

## @knitr EmptyWP2
model <- m3.2
mInfo<-summary(model)$AICtab
mInfo["N"]<- model@devcomp$dims["N"] # number of datapoints, verify
mInfo["p"]<- model@devcomp$dims["p"] # number of estimated parameters, verify
mInfo["ids"]<- (summary(model))$ngrps # number of units on level-2, here: individuals
# mInfo<- c(mInfo, "modelName"=modelName)
dsmInfo<-data.frame(mInfo)
dsmInfo<- plyr::rename(dsmInfo,replace= c("mInfo"="m3.2"))
dsmInfo$Coefficient <- rownames(dsmInfo)
mo3.2 <- dsmInfo # model information 
 m3.2# model results
mo3.2 # model performance

## @knitr EmptyWP3
head(ds)

## @knitr EmptyWP4
p <- ggplot2::ggplot(ds,aes(x=time,y=m3.2)) # replace the outcome with model prediction
p <- p + geom_line(aes(group=PersonID))
p <- p + theme1
p <- p + scale_x_discrete(labels=c("Pre-Test","Post-Test"))
p <- p + scale_y_continuous(limits=c(30,70), 
                            breaks=seq(30,70, by=5))
p <- p + facet_grid(.~group) 
p <- p + labs(list(
  title="Does treatment affect test performance?",
  x="Times of observation", y="Test score"))
p

## @knitr EmptyWP5
p <- ggplot2::ggplot(ds,aes(x=time,y=outcome, group=PersonID))  # mapping for entire plot
p <- p + geom_line(aes(color=group)) # geom specific mapping
p <- p + geom_line(aes(y=m3.2))      # geom specific mapping
p <- p + theme1
p <- p + scale_x_discrete(labels=c("Pre-Test","Post-Test"))
p <- p + scale_y_continuous(limits=c(30,70), 
                            breaks=seq(30,70, by=5))
p <- p + facet_grid(.~group) 
p <- p + labs(list(
  title="Does treatment affect test performance?",
  x="Times of observation", y="Test score"))
p













