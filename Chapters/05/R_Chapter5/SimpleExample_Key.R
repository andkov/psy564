# remove all elements for a clean start
rm(list=ls(all=TRUE))
cat("\014")

library(dplyr)
library(lme4)
library(ggplot2)

#### Data entry ####
y <- c(2,3,2,1,2,1,5,5)
id <- c(1,1,1,1,2,2,2,2)
ds <- data.frame(cbind(id,y))
ds
ds$wave <- with(ds, ave( id, id, FUN = seq_along)) # sequential numbers
ds 

ds1 <- ds[ds$id==1,]
ds2 <- ds[ds$id==2,]

lm(y ~ 1, ds1)
lm(y ~ 1, ds2)



# dplyr::group_by(ds,id) %>% dplyr::summarize(means = mean(y)) 

mean(ds[ds$id==1,"y"])
mean(ds[ds$id==2,"y"])
  

head(ds)

#### Model m00  ####
m00 <- lm(y ~ 1 ,ds)
ds$m00 <- predict(m00)
ds
#### Model m01  ####
m01 <- lm(y ~ 1 + wave, ds)
ds$m01 <- predict(m01)
ds
#### Model m0*  ####
m0_1 <- lm(y ~ 1, ds1)
m0_2 <- lm(y ~ 1, ds2)
# # Automating grouping with dplyr
# results<-ds %.% 
#   group_by(id) %.% 
#   do(model =lm(y ~ 1 , data = .))
# coef(results$model[[1]])
# coef(results$model[[2]])
ds1$m0_ <- predict(m0_1)
ds2$m0_ <- predict(m0_2)
dsa <- merge(ds, rbind(ds1,ds2))
ds


#### Model m1*  ####
m1_1 <- lm(y ~ 1 + wave, ds1)
m1_2 <- lm(y ~ 1 + wave, ds2)
# # Automating grouping with dplyr
# results<-ds %.% 
#   group_by(id) %.% 
#   do(model =lm(y ~ 1 + wave , data = .))
# coef(results$model[[1]])
# round(coef(results$model[[2]]),2)
ds1$m1_ <- predict(m1_1)
ds2$m1_ <- predict(m1_2)
ds <- merge(ds, rbind(ds1,ds2))
ds

#### Model m0  ####
m0 <- lme4::lmer(y ~ 1 + (1|id) , data = ds)
ranef(m0)
fixef(m0)
ds$m0 <- predict(m0)
ds

#### Model m0a  ####
m0a <- lme4::lmer(y ~ 1 + wave + (1|id) , data = ds)
ranef(m0a)
fixef(m0a)
ds$m0a <- predict(m0a)
ds

#### Model m1  ####
m1 <- lme4::lmer(y ~ 1 + wave + (1 + wave|id) , data = ds)
ranef(m1)
fixef(m1)
ds$m1 <- predict(m1)
ds 
 

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

