y <- c(2,3,2,1,2,1,5,5)
id <- c(1,1,1,1,2,2,2,2)
ds <- data.frame(cbind(id,y))
ds
ds$wave <- with(ds, ave( id, id, FUN = seq_along)) # sequential numbers
ds 

ds$ym00 <- mean(ds$y)
ds



library(dplyr)

dplyr::group_by(ds,id) %>% dplyr::summarize(means = mean(y)) 

mean(ds[ds$id==1,"y"])
mean(ds[ds$id==2,"y"])

lm(y ~ 1, data=ds[ds$id==1,"y"])

# 
# ds <- dplyr::select(ds, id, y wave) %>%
#   dplyr::group
ids <- unique(ds$id)
ds
for (i in ids ){
    ds[ds$is==i,"m0"] <- mean(ds[ds$id ==i,"y"])
}
ds


library(lme4)
library (ggplot2)
lm(y ~ 1 + wave, ds)
model <- lmer(y ~ 1 + wave +(1 + wave|id), ds)
coef(model)

ds$m00 <- mean(ds$y)
g <- ggplot(ds, aes(x=wave,y=y, group=id))
g <- g + geom_line()
g <- g + geom_point()
g <- g + geom_line(aes(y=m00), color="red")
g

ds$m0 <- predict(lmer(y ~ 1 + (1|id),ds, REML=T))
ds

(coefs <- fixef(model)) #Extract the coefficients for the fixed effects.
#create conditional prediction lines for each of the birth year cohorts.
# This is where the bottom part goes from "the list of models.R"
dsp$YPar<-(
  (coefs["(Intercept)"])         +(coefs["agec"]*dsp$agec)
  +(coefs["timec"]*dsp$timec)    +(coefs["timec:agec"]*dsp$agec*dsp$timec)
  +(coefs["timec2"]*dsp$timec2)  +(coefs["timec2:agec"]*dsp$agec*dsp$timec2)
  +(coefs["timec3"]*dsp$timec3)  
)