---
title: "Reporting Statistical Models (demo)"
author: "Andrey Koval"
date: "Friday, January 30, 2015"
output:
  html_document:
    css: ~/GitHub/psy564/libs/css/sidebar.css
    highlight: haddock
    theme: cerulean
    toc: yes
---

<!--  Set the working directory to the repository's base directory; this assumes the report is nested inside of only one directory.-->


<!-- Set the report-wide options, and point to the external script file. -->



## Resources

#### General R


- [A GOOD PLACE TO START LEARNING R](http://www.rstudio.com/resources/training/online-learning/) - The RStudio team collects the best online resources.  

- [Swirl](http://swirlstats.com/students.html) and [DataCamp](https://www.datacamp.com/courses/introduction-to-r)  offer interactive courses.  

- for brief reviews of key books and resources see Will Beasley's [Resources Opinions](https://github.com/OuhscBbmc/RedcapExamplesAndPatterns/blob/master/DocumentationGlobal/ResourcesOpinions.md)  

 - another presentation by Will provides an excellent overview of [Statistical Collaboration with GitHub](http://htmlpreview.github.io/?https://raw.githubusercontent.com/OuhscBbmc/StatisticalComputing/master/2014_Presentations/05_May/BeasleyScugGitHub2014-05.html#/)
 - Winston Chan's [R Cookbook](http://shop.oreilly.com/product/9780596809164.do) is a perfect book to get you started with producing graphs with RStudio  
 - [Quick-R](http://www.statmethods.net/) Quick and convenient resource for R reference  
 - [60+ resources for learning R](http://www.computerworld.com/article/2497464/business-intelligence-60-r-resources-to-improve-your-data-skills.html) Will give you more than you can handle. Browse and see which resource fits your learning style and organizes information in the manner that makes sense to you.

#### Modeling

  - [stats::lm()](http://www.rdocumentation.org/packages/stats/functions/lm) function, see [Gelman & Hill](http://www.stat.columbia.edu/~gelman/arm/), pages 38-39       
  - [stats::glm()](http://www.rdocumentation.org/packages/stats/functions/glm) function     
  - [nlme::gls()](http://www.rdocumentation.org/packages/nlme/functions/gls) function    
  - see a basic example of model result processing in the slides on [Statistical Modeling](http://ialsa.github.io/COAG-colloquium-2014F/2014-11-18-Statistical-Modeling.html#35) of the COAG [Colloquium series](http://ialsa.github.io/COAG-colloquium-2014F) on reproducible research.  
  - [lme4::lmer](http://www.rdocumentation.org/packages/nlme/functions/gls) function,  see [Gelman & Hill](http://www.stat.columbia.edu/~gelman/arm/), pages 259-262 and Appendix C. 
  - [lmer guide](http://htmlpreview.github.io/?https://github.com/andkov/Longitudinal_Models_of_Religiosity_NLSY97/blob/master/Vignettes/lmer/for%20Appendix/lmerGuide.html#adding-model-output). Shows direct interaction with the model object, not through functions like Gelman & Hill show.


## Data input

Before asking R to perform operations (functions), we load the packages which contain the definitions of these functions. 

```r
require(sas7bdat) # for inputting data 
library(dplyr) # for general data manipulation
library(reshape2) # for data : wide <-> long
library(psych) # data summary + etc
library(ggplot2) # graphing
library(nlme) # estimate mixed models | esp. gls()
library(lme4) # estimate mixed models | esp. lmer()
library(arm)  # process model objects
```

We would like to import data the data file

```
SAS_Chapter5.sas7bdat
```

located at 

```
./Chapters/05/SAS_Chapter5/
```

of your repository. 


```r
pathDir  <- getwd() # get working directory, e.i. residence of .Rproj file
pathFile  <- file.path(pathDir,"Chapters/05/SAS_Chapter5/SAS_Chapter5.sas7bdat") # location of the file
ds0   <- read.sas7bdat(pathFile, debug=TRUE) # import file 
ds0 <- data.frame(ds0) # save as a data frame 
```


## Data inspection

We'd like to inspect the data object we've just imported. Doulbe-click the icon of the dataset in the Environment tab of RStudio or use 
```
View(ds)
```
A few basic function calls give us a quick look ath the object *ds0* - our "ground zero" in the chain of data transformations

```r
class(ds0) # what class?
```

```
[1] "data.frame"
```

```r
dim(ds0)  # what dimensions?
```

```
[1] 100   3
```

```r
names(ds0) # what are column names?
```

```
[1] "PersonID" "wave"     "outcome" 
```

```r
str(ds0) # what its structure?
```

```
'data.frame':	100 obs. of  3 variables:
 $ PersonID: num  1 1 1 1 2 2 2 2 3 3 ...
 $ wave    : num  1 2 3 4 1 2 3 4 1 2 ...
 $ outcome : num  10.26 13.69 16.55 22.4 9.57 ...
```

```r
head(ds0) # what do first few lines look like?
```

```
  PersonID wave outcome
1        1    1   10.26
2        1    2   13.69
3        1    3   16.55
4        1    4   22.40
5        2    1    9.57
6        2    2    9.95
```

```r
base::summary(ds0) # basic summary
```

```
    PersonID       wave         outcome     
 Min.   : 1   Min.   :1.00   Min.   : 6.96  
 1st Qu.: 7   1st Qu.:1.75   1st Qu.:10.52  
 Median :13   Median :2.50   Median :12.35  
 Mean   :13   Mean   :2.50   Mean   :12.85  
 3rd Qu.:19   3rd Qu.:3.25   3rd Qu.:14.78  
 Max.   :25   Max.   :4.00   Max.   :22.41  
```

```r
psych::describe(ds0) # summary by psych package
```

```
         vars   n  mean   sd median trimmed  mad  min   max range skew kurtosis   se
PersonID    1 100 13.00 7.25  13.00   13.00 8.90 1.00 25.00 24.00 0.00    -1.24 0.72
wave        2 100  2.50 1.12   2.50    2.50 1.48 1.00  4.00  3.00 0.00    -1.39 0.11
outcome     3 100 12.85 3.14  12.34   12.65 3.22 6.96 22.41 15.45 0.63     0.20 0.31
```

```r
table(ds0$wave)# one-way table
```

```

 1  2  3  4 
25 25 25 25 
```


## Wide to Long

Our data were in a long format, with each level of the variable *wave* recorded in a single column 

```r
head(ds0)
```

```
  PersonID wave outcome
1        1    1   10.26
2        1    2   13.69
3        1    3   16.55
4        1    4   22.40
5        2    1    9.57
6        2    2    9.95
```

No other transformation is necessary. 

```r
dsL <- ds0
```
Suffix "L" in "dsL" signifies that the data is in the **L**ong format.

## Data tweaking

Data didn't require any tweaking.

```r
dsM <- dsL

#### Basic Graphs  ####
```
Suffix "M" in "dsM" signifies that the data is ready for  **M**odeling

## Graphing

To better understand the patterns we visualize the data with a simple line plot

```r
p <- ggplot2::ggplot(dsM,aes(x=wave,y=outcome)) # map data dimension
p <- p + geom_line(aes(group=PersonID)) # draw lines and map unit of measurement
p
```

<img src="figure_rmd/GraphingData0.png" title="plot of chunk GraphingData0" alt="plot of chunk GraphingData0" width="490" />

To make the graph more readable and informative we customize it with a set of presets collected in a ggplot theme

```r
baseSize <- 12 # set as the point of further reference
theme1 <- ggplot2::theme_bw(base_size=baseSize) +
  ggplot2::theme(title=ggplot2::element_text(colour="gray20",size = baseSize+1)) +
  ggplot2::theme(axis.text=ggplot2::element_text(colour="gray40", size=baseSize-2)) +
  ggplot2::theme(axis.title=ggplot2::element_text(colour="gray40")) +
  ggplot2::theme(panel.border = ggplot2::element_rect(colour="gray80")) +
  ggplot2::theme(axis.ticks.length = grid::unit(0, "cm")) +
  ggplot2::theme(text = element_text(size =baseSize+7)) 
```

and add customizations relevant to current scenario


```r
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
```

<img src="figure_rmd/GraphingData1.png" title="plot of chunk GraphingData1" alt="plot of chunk GraphingData1" width="490" />

black lines give us a longitudinal perspective on the data, depicting the observed trajectories of individuals. To juxtapose is with cross-sectional perspective, we can add a different geom, styled to contrast the difference 


```r
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
```

<img src="figure_rmd/GraphingData2.png" title="plot of chunk GraphingData2" alt="plot of chunk GraphingData2" width="490" />

```r
####  MODEL 3.1  ####
```



## Modeling 

The baseline for the models to follow is the empty between-person model with no predictors

### Run m3.1

```r
m3.1 <- nlme::gls(outcome ~ 1, data=dsM, method="ML") # create model object
dsM$m3.1 <- predict(m3.1) # stores values predicted by the model
summary(m3.1) # print a bit more info
```

```
Generalized least squares fit by maximum likelihood
  Model: outcome ~ 1 
  Data: dsM 
    AIC   BIC logLik
  515.5 520.7 -255.8

Coefficients:
            Value Std.Error t-value p-value
(Intercept) 12.85    0.3139   40.94       0

Standardized residuals:
    Min      Q1     Med      Q3     Max 
-1.8860 -0.7476 -0.1616  0.6190  3.0615 

Residual standard error: 3.123 
Degrees of freedom: 100 total; 99 residual
```

```r
# str(summary(m3.1)) # to inspect object directly
```

### Inspect m3.1

```r
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
```

```
Generalized least squares fit by maximum likelihood
  Model: outcome ~ 1 
  Data: dsM 
  Log-likelihood: -255.8

Coefficients:
(Intercept) 
      12.85 

Degrees of freedom: 100 total; 99 residual
Residual standard error: 3.123 
```

```r
mi3.1 #  model information
```

```
           m3.1 Coefficient
logLik   -255.8      logLik
deviance  511.5    deviance
AIC       515.5         AIC
BIC       520.7         BIC
df.resid   99.0    df.resid
N         100.0           N
p           1.0           p
ids        25.0         ids
```

```r
head(dsM) # visual check
```

```
  PersonID wave outcome  m3.1
1        1    1   10.26 12.85
2        1    2   13.69 12.85
3        1    3   16.55 12.85
4        1    4   22.40 12.85
5        2    1    9.57 12.85
6        2    2    9.95 12.85
```

### Graph m3.1

```r
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
```

<img src="figure_rmd/GraphM3_1.png" title="plot of chunk GraphM3_1" alt="plot of chunk GraphM3_1" width="490" />

```r
####  MODEL 5.1  ####
```


Now we'd like to see how this basic model prediction will change when we compute a **U**nique intercept for each person

### Run m5.1

```r
m5.1 <- lme4::lmer(outcome ~ 1 + (1 | PersonID), data=dsM, REML=FALSE)# create model object
dsM$m5.1 <- predict(m5.1) # stores values predicted by the model
display(m5.1) # tidy results
```

```
lme4::lmer(formula = outcome ~ 1 + (1 | PersonID), data = dsM, 
    REML = FALSE)
coef.est  coef.se 
   12.85     0.42 

Error terms:
 Groups   Name        Std.Dev.
 PersonID (Intercept) 1.64    
 Residual             2.66    
---
number of obs: 100, groups: PersonID, 25
AIC = 508.4, DIC = 502.4
deviance = 502.4 
```

### Inspect m5.1
















