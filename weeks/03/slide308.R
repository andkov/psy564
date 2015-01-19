rm(list=ls(all=TRUE)) # clears environment
cat("\014") # clears console


# create a vector of data called Y containing numbers 4, 6, 5, 7, 8
Y <- c(4, 6, 5, 7, 8) 

# at this moment Y is of class...
class(Y)

# we can make it a matrix using matric() function
Y <- matrix(Y,5,1,byrow=TRUE) # matrix (data,#rows,#columns,...)


# create six data vectors of length 5 , one for each person (i)
i1 <- c(11,  11.5, 11.8, 10.3, 9.5) 
i2 <- c(9,    9.8,  9.7,  9.6, 9.3) 
i3 <- c(9,    7.8,  8.1,  9.1, 7.8) 
i4 <- c(6.8,  7.9,  7,    7.8, 6.8) 
i5 <- c(6.4,  6.2,  6.8,  7.1, 7.2) 
i6 <- c(4,    5.6,  4.5,  5.2, 6.8) 

# combine
data <- cbind(i1, i2, i3, i4, i5, i6)

class(data)
X <- matrix(data,6,5, byrow=TRUE)
X


i <- paste0("i",1:6)
t <- paste0("t",1:5)
X
rownames(X) <- i
X
colnames(X) <- t
X

ds <- data.frame(X)
ds
