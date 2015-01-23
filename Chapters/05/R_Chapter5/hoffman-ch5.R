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
pathDir  <- getwd()
pathChapter  <- file.path(pathDir,"Chapters/05/SAS_Chapter5/SAS_Chapter5.sas7bdat")
dsL5   <- read.sas7bdat(pathChapter, debug=TRUE) 
ds <- data.frame(dsL5)
names(ds)
str(data.frame(ds))
# basic tables
head(ds)

summary(ds)
psych:summary(ds)

table(ds$group)

# stack data 
dsLong <- reshape2::melt(ds,id.vars=c("PersonID","group"))
# sort for visual inspection
dsLong <- dsLong[order(dsLong$PersonID, dsLong$PersonID),]
head(dsLong,12)
# tweak the datatset
dsLong$variable <- gsub(pattern="outcome", replacement='', x=dsLong$variable)
head(dsLong,12)
dsLong <- dplyr::select(dsLong, id=PersonID, group,time=variable, outcome=value)
ds <- dsLong


ds$group <- ordered(ds$group,
                               levels = c(1,2),
                               labels = c("Control","Treatment"))
ds$time <- ordered(ds$time,
                    levels = c(1,2),
                    labels = c("Pre-Test","Post-Test"))

str(ds)



head(dsLong)
ds <- dsLong
head(ds)
# Load graph themes
source("Scripts/Graphs/graphThemes.R")
# graph
p <- ggplot2::ggplot(ds,aes(x=time,y=outcome))
p <- p + geom_line(aes(group=id, color=factor(group)), alpha=1,
                   position=position_jitter(w=0.0, h=0.0))
p <- p + theme1
p <- p + scale_x_discrete(limits=c(1,2), labels=c("Pre-Test","Post-Test"))
p <- p + scale_y_continuous(limits=c(30,70), 
                            breaks=seq(30,70, by=5))
# p <- p + facet_grid(.~group)
# p <- p + labs(list(
#   title="How often did you attend worship last year?",
#   x="Year of observation", y="Church attendance"))
p





## @knitr EmptyMeansModel
m3.1 <- nlme::gls(outcome ~ 1, data=ds, method="ML", correlation=corSymm(form = ~ 1|id))
summary(m3.1)
str(summary(m3.1))
ds$m3.1 <- predict(m3.1)

var(ds$outcome - ds$m3.1)


model <- m3.1
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
colnames(dsmInfo) <- c("m3.1")
# dsmInfo$Coefficient <- rownames(dsmInfo)
mo3.1 <- dsmInfo
mo3.1$Coefficient <- rownames(mo3.1)
summary(m3.1)
print(mo3.1)




## @knitr EmptyMeansModel
m3.2 <- lme4::lmer(outcome ~ 1 + (1|id), data=ds, REML=FALSE)
summary(m3.2)
ds$m3.2 <- predict(m3.2)
var(ds$outcome - ds$m3.2)

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

print(mo3.2)
print(mo3.1) 















