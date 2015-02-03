# remove all elements for a clean start
rm(list=ls(all=TRUE))
cat("\014")

library(dplyr)
library(lme4)
library(ggplot2)

#### Data entry ####
y <- c(2,3,2,1,2,1,5,5)
id <- c(1,1,1,1,2,2,2,2)
ds <- 


head(ds)

#### Model m00  ####



#### Model m01  ####



#### Model m0*  ####



#### Model m1*  ####



#### Model m0  ####



#### Model m0a  ####




#### Model m1  ####



 

#### Graphing ####


baseSize <- 12 # set as the point of further reference
theme1 <- ggplot2::theme_bw(base_size=baseSize) +
  ggplot2::theme(title=ggplot2::element_text(colour="gray20",size = baseSize+1)) +
  ggplot2::theme(axis.text=ggplot2::element_text(colour="gray40", size=baseSize-2)) +
  ggplot2::theme(axis.title=ggplot2::element_text(colour="gray40")) +
  ggplot2::theme(panel.border = ggplot2::element_rect(colour="gray80")) +
  ggplot2::theme(axis.ticks.length = grid::unit(0, "cm")) +
  ggplot2::theme(text = element_text(size =baseSize+7)) 


ds
g <- ggplot(ds, aes(x=wave,y=y, group=id))#, color=factor(id)))
g <- g + geom_line()
g <- g + geom_point(shape=21, size=4, fill="purple", alpha=.5)
# g <- g + geom_line(aes(y=m1_), color="red", linetype="dashed", size=2)
g <- g + geom_line(aes(y=m00, color=factor(id)), linetype="dashed", size=2)
g <- g + scale_y_continuous(limits=c(0,6))
g <- g + theme1
g

