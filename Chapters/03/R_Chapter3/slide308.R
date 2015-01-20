rm(list=ls(all=TRUE)) # clears environment
cat("\014") # clears console

library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)




# create a vector of data called Y containing numbers 4, 6, 5, 7, 8
Y <- c(4, 6, 5, 7, 8) 

# at this moment Y is of class...
class(Y)

# we can make it a matrix using matric() function
Y <- matrix(Y,5,1,byrow=TRUE) # matrix (data,#rows,#columns,...)


# create six data vectors of length 5 , one for each person (i)
i1 <- c(1, 11,  11.5, 11.8, 10.3, 9.5) 
i2 <- c(2, 9,    9.8,  9.7,  9.6, 9.3) 
i3 <- c(3, 9,    7.8,  8.1,  9.1, 7.8) 
i4 <- c(4, 6.8,  7.9,  7,    7.8, 6.8) 
i5 <- c(5, 6.4,  6.2,  6.8,  7.1, 7.2) 
i6 <- c(6, 4,    5.6,  4.5,  5.2, 6.8) 

# combine
data <- cbind(i1, i2, i3, i4, i5, i6)
data

class(data)
X <- matrix(data,6,6, byrow=TRUE)
X


i <- paste0("i",1:6)
vars <- c("id",paste0("t",1:5))
X
rownames(X) <- i
X
colnames(X) <- vars
X

ds <- data.frame(X)
ds

p <- ggplot(ds, aes(x=1:6))
p <- geom_line(aes(y=t1))




dsLong <- reshape2::melt(ds, id.vars="id")
dsLong <- dsLong[order(dsLong$id, dsLong$variable),]
head(dsLong,12)
dsLong$variable <- gsub(pattern="t", replacement='', x=dsLong$variable)
head(dsLong,12)
str(dsLong)
dsLong$variable <- as.integer(dsLong$variable)
str(dsLong)
dsLong <- plyr::rename(dsLong,c("variable"="time", "value"="outcome"))
ds <- dsLong
head(ds)

pathDir <- getwd()
source(file.path(pathDir,"Scripts/Graphs/graphThemes.R"))
p <- ggplot2::ggplot(ds,aes(x=time,y=outcome))
p <- p + geom_line(aes(group=id, color=factor(id)))
p <- p + geom_point(aes(group=id, color=factor(id)))
p <- p + theme1
p <- p + scale_x_continuous(limits=c(1,5), breaks=seq(0,6))
p <- p + scale_y_continuous(limits=c(0,12), 
                            breaks=seq(0,13, by=2))
p <- p + labs(list(
  title="Hypothetical trajectories from slide 308",
  x="Occasion", y="Outcome"))
p
