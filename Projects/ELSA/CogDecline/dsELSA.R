# remove all elements for a clean start
rm(list=ls(all=TRUE))
cat("\014")


## @knitr InstallPackage
# source("./Scripts/Utility/InstallPackages.R")



## @knitr LoadPackages
require(sas7bdat) # for inputting data 
library(dplyr) # for general data manipulation
library(reshape2) # for data : wide <-> long
library(psych) # data summary + etc
library(ggplot2) # graphing
library(nlme) # estimate fixed models | esp. gls()
library(lme4) # estimate mixed models | esp. lmer()
library(arm)  # process model objects


## @knitr LoadData
ds0 <- readRDS("~/GitHub/psy564/Data/Raw/ELSA/ds0_ELSA.rds")
dim(ds0)
length(unique(ds0$PID))
head(ds0)

dsL <- ds0
 
## @knitr DataFilter

table(dsL$irecall,dsL$wave, useNA="ifany") # frequency table
# There are no valid respondes to the focal outcome, so we remove wave 5
# Wave 0 doesn't not contain observations of interest, so remove wave 0
dsL <- dplyr::filter(dsL, wave %in% c(1:4))  
table(dsL$irecall,dsL$wave, useNA="ifany")
# The missing values will complecate some of the modeling so remove for now


## @knitr DataFilter2
# Selecting individuals into the working dataset
length(unique(dsL$id))
table(dsL$wave)
dsL <- dplyr::filter(dsL, (ave(!is.na(irecall), id, FUN = all))) # only complete trajectoies
dsL <- dplyr::filter(dsL, (ave(!is.na(drecall), id, FUN = all))) # only complete trajectoies
dsL <- dplyr::filter(dsL, (ave(!is.na(animal), id, FUN = all))) # only complete trajectoies
dsL <- dplyr::filter(dsL, (ave(!is.na(prospect), id, FUN = all))) # only complete trajectoies
length(unique(dsL$id))
table(dsL$wave)
dsL <- dsL[dsL$id %in% unique((dsL[dsL$wave==4,c("id","wave")])$id ),] # only who reached 4th wave
length(unique(dsL$id))
table(dsL$wave)


## @knitr DataExport

# save data for use in R
saveRDS(object=dsL, file="~/GitHub/psy564/Data/Derived/ELSA/dsL_ELSA.rds", compress="xz")  


#### save LONG data for MPlus  ####
write.table(dsL,file="~/GitHub/psy564/Projects/ELSA/CogDecline/dsL_ELSA.csv",na="-9999",row.names=FALSE, col.names=FALSE, sep=",")
# save the header
varNamesL <- colnames(dsL) # get variable names
cat(varNamesL) # print, copy, paste into Mplus
write.csv(t(varNamesL), file="~/GitHub/psy564/Projects/ELSA/CogDecline/dsL_ELSA_varNames.csv") # export as a csv


#### save WIDE data for MPlus ####

## Transform 
head(dsL)
ds <- dplyr::select(dsL, id, dob, sex, age, edu, ht, db, htdb, nocase, 
                    angina=ang1A, stroke=stk1A, smoked=evrsmk1A,
                    time, wave, year, ageCur=Age,
                    irecall, drecall, animal, prospect)
# head(ds)
timeInvariant <- c( "id", "dob", "sex", "age", "edu", "ht", "db", "htdb", "nocase", 
                    "angina", "stroke", "smoked") 
timeVariant <- c("time", "wave", "year", "ageCur", "irecall", "drecall", "animal","prospect")
dsLong <- reshape2::melt(ds,id.vars = c(timeInvariant, "wave"), measure.vars = timeVariant )
dsLong <- dsLong[order(dsLong$id, dsLong$variable), ]
# head(dsLong, 20)
dsW <- reshape2::dcast(dsLong, id + dob + sex + age + edu + ht + db + htdb + nocase + angina + stroke + smoked  ~ variable + wave, value.var="value"  )
head(dsW)
# Export the data
write.table(dsW,file="~/GitHub/psy564/Projects/ELSA/CogDecline/dsW_ELSA.dat",col.names = F)
write.csv(colnames(dsW), file="~/GitHub/psy564/Projects/ELSA/CogDecline/dsW_ELSA_varNames.csv")
# Export the header
varNamesW <- colnames(dsW) # get variable names
cat(varNamesW) # print, copy, paste into Mplus
write.csv(t(varNamesW), file="~/GitHub/psy564/Projects/ELSA/CogDecline/dsW_ELSA_varNames.csv") # export as a csv


#### Graphing ####

## @knitr LoadGraphThemes
paletteColor4conditions <- c("Hypertension"="red",
                             "Diabetes"="blue",
                             "Both"="purple",
                             "None"="grey10")


baseSize <- 12 # set as the point of further reference
theme1 <- ggplot2::theme_bw(base_size=baseSize) +
  ggplot2::theme(title=ggplot2::element_text(colour="gray20",size = baseSize+1)) +
  ggplot2::theme(axis.text=ggplot2::element_text(colour="gray40", size=baseSize-2)) +
  ggplot2::theme(axis.title=ggplot2::element_text(colour="gray40")) +
  ggplot2::theme(panel.border = ggplot2::element_rect(colour="gray80")) +
  ggplot2::theme(axis.ticks.length = grid::unit(0, "cm")) +
  ggplot2::theme(text = element_text(size =baseSize+7)) 


## @knitr CrossSectionDist
# Cross-sectional view: irecall
p <- ggplot2::ggplot(dsL, aes(y=irecall,x=factor(year), fill=condition))
p <- p + geom_violin( adjust=1, alpha=.4 )
p <- p + scale_y_continuous(limits=c(0,10), breaks=seq(0,10,by=1))
p <- p + scale_fill_manual(values=paletteColor4conditions)
p <- p + geom_boxplot(width=1, fill=NA, color="black", alpha=.5, outlier.colour="red") 
p <- p + stat_summary(fun.y=mean, geom="point", fill="white", shape=21, size=4)
p <- p + facet_grid(dbts ~ hptn)
p <- p + theme1
p

## @knitr CrossSectionCount
# Cross-sectional view: frequence of irecall
p <- ggplot2::ggplot(dsL, aes(x=factor(year)))
p <- p + scale_y_continuous(breaks=seq(0,6000,by=1000))
p <- p + geom_bar(aes(fill=condition), alpha=.4)
p <- p + facet_grid(dbts ~ hptn)
p <- p + scale_fill_manual(values=paletteColor4conditions)
p <- p + guides(fill = guide_legend(reverse=F, title="Diagnosis at A")) 
p <- p + theme1
p


## @knitr EasyData
ds <- dplyr::filter(dsL, id %in% sample(unique(id),100)) %>% # select only N ids
  dplyr::select(id, dob, female, hptn, dbts, condition, year, time, Age, irecall,animal,prospect, drecall)
head(ds)




