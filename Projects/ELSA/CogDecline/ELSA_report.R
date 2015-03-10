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
dsL <- readRDS("~/GitHub/psy564/Data/Derived/ELSA/dsL_ELSA.rds")


## @knitr EasyData
ds <- dplyr::filter(dsL, id %in% sample(unique(id),100)) %>% # select only N ids
  dplyr::select(id, dob, female, age80, edu11, hptn, dbts, condition, wave, year, time, Age, irecall,animal,prospect, drecall)
head(ds)


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


head(ds)
## @knitr irecall_0R_1F_2x_x
ds$int <- paste(
ds$linear <- .3
ds$quad <- .1

p <- ggplot2::ggplot(ds,aes(x=wave, y=irecall))
p <- p + geom_line(aes(group=id), color="firebrick", alpha=.5)  # individual trajectories
p <- p + geom_point(size=3, shape=21, fill=NA, color="black", alpha=.4) # cross-section data points
p <- p + geom_abline(, color="royalblue3", size=3, alpha=.05) # modelled data
p <- p + theme1 
p <- p + scale_x_continuous(limits=c(1,4), breaks=c(1:4)) # X axis
p <- p + scale_y_continuous(limits=c(0,10), 
                            breaks=seq(0,10, by=1)) # Y axis
p <- p + labs(list(
  title="Does outcome change over time?", # main title
  x="Wave of measurement", y="Performance on the outcome")) # axes titles
p
