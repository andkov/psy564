---
title: "Beginnings of modeling"
author: "Andrey Koval"
date: "Monday, January 12, 2015"
output:
  html_document:
    css: ~/GitHub/psy564/libs/css/sidebar.css
    highlight: zenburn
    theme: cerulean
    toc: yes
---

<!--  Set the working directory to the repository's base directory; this assumes the report is nested inside of only one directory.-->


<!-- Set the report-wide options, and point to the external script file. -->


====

## Resources 

There are several packages in R developed to fit linear and non-linear models. Some of the most popular are <code>stats</code>, <code>nlme</code>, and <code>lme4</code>. They have distinctions that we will grow to appreciate and take advantage of.
 
 
#### <code>stats</code>  package  
  - [Official Documentation](http://www.rdocumentation.org/packages/stats)  
  - [lm()](http://www.rdocumentation.org/packages/stats/functions/lm) function    
  - [glm()](http://www.rdocumentation.org/packages/stats/functions/glm) function     
  
  
#### <code>nlme</code>  package  
  - [Official Documentation](http://www.rdocumentation.org/packages/nlme)  
  - [gls()](http://www.rdocumentation.org/packages/nlme/functions/gls) function    
  - see a basic example of model result processing in the slides on [Statistical Modeling](http://ialsa.github.io/COAG-colloquium-2014F/2014-11-18-Statistical-Modeling.html#35) of the COAG [Colloquium series](http://ialsa.github.io/COAG-colloquium-2014F) on reproducible research.  
  

#### <code>lme4</code>  package  
  - [Official Documentation](http://www.rdocumentation.org/packages/lme4)  
  - [lmer](http://www.rdocumentation.org/packages/nlme/functions/gls) function 
  - [lmer guide](http://htmlpreview.github.io/?https://github.com/andkov/Longitudinal_Models_of_Religiosity_NLSY97/blob/master/Vignettes/lmer/for%20Appendix/lmerGuide.html#adding-model-output)


There are many more resources on the web, but these will get you started. 

## Data input

Before we can do anything with R in RStudio, we need to load the packages we'll require for this project. Frequently we don't know which one we'll eventually end up using, so expect to edit this section as you develop the script. 

```r
require(sas7bdat)
library(nlme)
library(dplyr)
library(reshape2)
library(ggplot2)
library(psych)
```

We would like to import data the data file

```
SAS_Chapter3a.sas7bdat
```

located at 

```
./Chapters/03/SAS_Chapter3a/
```

of your repository. 


```r
pathDir  <- getwd() # get working directory, e.i. residence of .Rproj file
pathFile  <- file.path(pathDir,"Chapters/03/SAS_Chapter3a/SAS_Chapter3a.sas7bdat") # location of the file
ch3a   <- read.sas7bdat(pathFile, debug=TRUE) # import file 
ch3a <- data.frame(ch3a) # save as a data frame 
```



## Data inspection

We'd like to inspect the data object we've just imported. You can do it by clicking on the icon of the object in the Environment tab of RStudio. The command
```
View(ds)
```
would accomplish the same. Most of our interactions with data, however, would be with the help f question/commands we'll issue to the computer. For example, to get better idea about the object *ds*  I might ask serveral questions: what is its class? what are its dimensions? do columns have names? what do the first few rows look like? and other. This section demonstrates some of useful functions to inspect you data with. 

```r
class(ch3a) # what class?
```

```
[1] "data.frame"
```

```r
dim(ch3a)  # what dimensions?
```

```
[1] 50  4
```

```r
names(ch3a) # what are column names?
```

```
[1] "PersonID" "group"    "outcome1" "outcome2"
```

```r
str(ch3a) # what its structure?
```

```
'data.frame':	50 obs. of  4 variables:
 $ PersonID: num  1 2 3 4 5 6 7 8 9 10 ...
 $ group   : num  1 1 1 1 1 1 1 1 1 1 ...
 $ outcome1: num  56 53.8 53.3 44.8 47.2 ...
 $ outcome2: num  57.8 58.9 59.1 44.6 50.1 ...
```

```r
head(ch3a) # what do first few lines look like?
```

```
  PersonID group outcome1 outcome2
1        1     1    55.96    57.81
2        2     1    53.75    58.94
3        3     1    53.26    59.15
4        4     1    44.80    44.56
5        5     1    47.16    50.09
6        6     1    45.03    46.90
```

```r
summary(ch3a) # basic summary
```

```
    PersonID        group        outcome1       outcome2   
 Min.   : 1.0   Min.   :1.0   Min.   :37.5   Min.   :44.6  
 1st Qu.:13.2   1st Qu.:1.0   1st Qu.:46.2   1st Qu.:53.5  
 Median :25.5   Median :1.5   Median :49.5   Median :56.8  
 Mean   :25.5   Mean   :1.5   Mean   :49.9   Mean   :56.8  
 3rd Qu.:37.8   3rd Qu.:2.0   3rd Qu.:53.2   3rd Qu.:60.7  
 Max.   :50.0   Max.   :2.0   Max.   :62.1   Max.   :68.6  
```

```r
psych::describe(ch3a) # summary by psych package
```

```
         vars  n  mean    sd median trimmed   mad   min   max range  skew kurtosis   se
PersonID    1 50 25.50 14.58  25.50   25.50 18.53  1.00 50.00 49.00  0.00    -1.27 2.06
group       2 50  1.50  0.51   1.50    1.50  0.74  1.00  2.00  1.00  0.00    -2.04 0.07
outcome1    3 50 49.92  5.16  49.53   49.75  5.34 37.53 62.13 24.60  0.16    -0.31 0.73
outcome2    4 50 56.76  5.57  56.78   56.83  5.56 44.56 68.62 24.05 -0.08    -0.56 0.79
```

```r
table(ch3a$group)# one-way table
```

```

 1  2 
25 25 
```


## Wide to Long

Our data were in a wide format, with each level of the variable *occasion of measurement* recorded in a separate variable. 

```r
head(ch3a)
```

```
  PersonID group outcome1 outcome2
1        1     1    55.96    57.81
2        2     1    53.75    58.94
3        3     1    53.26    59.15
4        4     1    44.80    44.56
5        5     1    47.16    50.09
6        6     1    45.03    46.90
```

We'd like to rectify this, reorganizing the data entries in such a way so that the **names** of the  variables *outcome1* and *outcome2* become the **data values** of the new variable *time*.


```r
# stack data into a new dataset
dsLong <- reshape2::melt(ch3a,id.vars=c("PersonID","group")) # id.vars are those NOT STACKED
head(dsLong) # inspect
```

```
  PersonID group variable value
1        1     1 outcome1 55.96
2        2     1 outcome1 53.75
3        3     1 outcome1 53.26
4        4     1 outcome1 44.80
5        5     1 outcome1 47.16
6        6     1 outcome1 45.03
```

```r
dsLong <- dsLong[order(dsLong$PersonID, dsLong$variable),] # sort for visual inspection
head(dsLong) # inspect
```

```
   PersonID group variable value
1         1     1 outcome1 55.96
51        1     1 outcome2 57.81
2         2     1 outcome1 53.75
52        2     1 outcome2 58.94
3         3     1 outcome1 53.26
53        3     1 outcome2 59.15
```

## Data tweaking

We've brought the data to the structure we want (we'll work mostly with data in long/stacked format), but there are several things we can tweak about the dataset to make it easier to work with.


```r
# substitute the repeating string in column "variable" by nothing
dsLong$variable <- gsub(pattern="outcome", replacement='', x=dsLong$variable) 
head(dsLong)
```

```
   PersonID group variable value
1         1     1        1 55.96
51        1     1        2 57.81
2         2     1        1 53.75
52        2     1        2 58.94
3         3     1        1 53.26
53        3     1        2 59.15
```

```r
dsLong <- plyr::rename(dsLong, c("variable"="time", "value"="outcome")) # rename with plyr package
# Alternatively: rename and order variables with dplyr package
# dsLong <- dplyr::select(dsLong, id=PersonID, group,time=variable, outcome=value)
head(dsLong)
```

```
   PersonID group time outcome
1         1     1    1   55.96
51        1     1    2   57.81
2         2     1    1   53.75
52        2     1    2   58.94
3         3     1    1   53.26
53        3     1    2   59.15
```

```r
ds <- dsLong # save with a more convenient name
```
The shorter the names of the objects you work with - the better. So we'll rename "ch3a" into "ds", to save on typing two extra characters. Also, it is a good idea to be using general names for datasets so that you can easier adapt the code for other projects. 


Closer inspection reveals that variables *group* is stored  stored as a numeric vector and *time* as a character vector  

```r
str(dsLong)
```

```
'data.frame':	100 obs. of  4 variables:
 $ PersonID: num  1 1 2 2 3 3 4 4 5 5 ...
 $ group   : num  1 1 1 1 1 1 1 1 1 1 ...
 $ time    : chr  "1" "2" "1" "2" ...
 $ outcome : num  56 57.8 53.8 58.9 53.3 ...
```
However, we understand that their values correspond to a particular category of response: *group* indicates *Treatment* (group=1) and *Control* (group=2), while *time* refers to *Pre-Test* (time=1) and *Post-Test* (time=2). We transform these variables into **ordered factors**, assigning the verbal descriptions (labels) to its values (labels). 



```r
ds$group <- ordered(ds$group, levels = c(1,2),
                              labels = c("Control","Treatment"))

ds$time <- ordered(ds$time, levels = c(1,2),
                            labels = c("Pre-Test","Post-Test"))
str(ds)
```

```
'data.frame':	100 obs. of  4 variables:
 $ PersonID: num  1 1 2 2 3 3 4 4 5 5 ...
 $ group   : Ord.factor w/ 2 levels "Control"<"Treatment": 1 1 1 1 1 1 1 1 1 1 ...
 $ time    : Ord.factor w/ 2 levels "Pre-Test"<"Post-Test": 1 2 1 2 1 2 1 2 1 2 ...
 $ outcome : num  56 57.8 53.8 58.9 53.3 ...
```

## Graphing

To better understand the patterns we visualize the data with a simple line plot

```r
p <- ggplot2::ggplot(ds,aes(x=time,y=outcome)) # map data dimension
p <- p + geom_line(aes(group=PersonID)) # draw lines and map unit of measurement
p
```

<img src="figure_rmd/GraphingData0.png" title="plot of chunk GraphingData0" alt="plot of chunk GraphingData0" width="490" />

To make the graph more readable and informative we can resort to a variety of customizations within the *ggplot2* package, such as representing groups by different colors, customizing the scales, axes labels, as well as adding titles to each axes and the graph overall:


```r
p <- ggplot2::ggplot(ds,aes(x=time,y=outcome))
p <- p + geom_line(aes(group=PersonID, color=group)) # map color
p <- p + scale_x_discrete(labels=c("Pre-Test","Post-Test")) # X axis
p <- p + scale_y_continuous(limits=c(30,70), 
                            breaks=seq(30,70, by=5)) # Y axis
p <- p + labs(list(
  title="Does treatment affect test performance?", # main title
  x="Times of observation", y="Test score")) # axes titles
p
```

<img src="figure_rmd/GraphingData1.png" title="plot of chunk GraphingData1" alt="plot of chunk GraphingData1" width="490" />

In fact, there are so many possible customizations that they will clutter the script. To save on space and make script more readable, multiple graph settings can be collected in a object call *theme*


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

and attached to the graph as a layer


```r
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
```

<img src="figure_rmd/GraphingData2.png" title="plot of chunk GraphingData2" alt="plot of chunk GraphingData2" width="490" />

We can also store it in a separate file and call for execution with *source()* command before attaching it to the graph
```
source("Scripts/Graphs/graphThemes.R")
```

To further disentangle the view of the pattenrs inside each group, we place individuals on separate graphs according to their group membership

```r
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
```

<img src="figure_rmd/GraphingData3.png" title="plot of chunk GraphingData3" alt="plot of chunk GraphingData3" width="980" />


## Modeling 

Now we are ready to recreate the data with statistical models. First, we'll recreate the mode from equation (3.1), also known as the "A Between-Person Empty Model".

### Run m3.1

```r
m3.1 <- nlme::gls(outcome ~ 1, data=ds, method="ML") # create model object
m3.1 # print basic info
```

```
Generalized least squares fit by maximum likelihood
  Model: outcome ~ 1 
  Data: ds 
  Log-likelihood: -326.3

Coefficients:
(Intercept) 
      53.34 

Degrees of freedom: 100 total; 99 residual
Residual standard error: 6.319 
```

```r
summary(m3.1) # print a bit more info
```

```
Generalized least squares fit by maximum likelihood
  Model: outcome ~ 1 
  Data: ds 
    AIC   BIC logLik
  656.5 661.7 -326.3

Coefficients:
            Value Std.Error t-value p-value
(Intercept) 53.34    0.6351   83.99       0

Standardized residuals:
     Min       Q1      Med       Q3      Max 
-2.50129 -0.81282 -0.01736  0.71776  2.41752 

Residual standard error: 6.319 
Degrees of freedom: 100 total; 99 residual
```

```r
# str(summary(m3.1)) # will get you all the elements of this model object, so you can extract what you need
ds$m3.1 <- predict(m3.1) # stores values predicted by the model
var(ds$outcome - ds$m3.1) # variance of the residual computed directly
```

```
[1] 40.34
```

```r
sd(ds$outcome - ds$m3.1) # standard deviation of the residual computed directly
```

```
[1] 6.351
```

As you notice, we used *gls()*  function from the *nlme* package, although *lm()* and *glm()* functions from *stats* package would also have done the trick, with some ideosyncrasies. These function construct model objects ( in which model outcomes are stored ) differently and retrieving information from them becomes a hassle. To standardize the experience with a greater number of models, we'll use *gls()* and not a simpler *lm()* to estimate models without random components. 
  

```r
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
```

```
Generalized least squares fit by maximum likelihood
  Model: outcome ~ 1 
  Data: ds 
  Log-likelihood: -326.3

Coefficients:
(Intercept) 
      53.34 

Degrees of freedom: 100 total; 99 residual
Residual standard error: 6.319 
```

```r
mo3.1 # model performance
```

```
           m3.1 Coefficient
logLik   -326.3      logLik
deviance  652.5    deviance
AIC       656.5         AIC
BIC       661.7         BIC
df.resid   99.0    df.resid
N         100.0           N
p           1.0           p
ids        50.0         ids
```

Now our dataset contains both observed data (ds$outcome) and data predicted by model 3.1 (ds$m3.1).

```r
head(ds)
```

```
   PersonID   group      time outcome  m3.1
1         1 Control  Pre-Test   55.96 53.34
51        1 Control Post-Test   57.81 53.34
2         2 Control  Pre-Test   53.75 53.34
52        2 Control Post-Test   58.94 53.34
3         3 Control  Pre-Test   53.26 53.34
53        3 Control Post-Test   59.15 53.34
```

### Graph m3.1
To better appreciate the implications of this statistical model, let us graph the prediction that this model made FOR EACH INDIVIDUAL. We can re-use the graph from before, only map it do different data

```r
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
```

<img src="figure_rmd/EmptyBP4.png" title="plot of chunk EmptyBP4" alt="plot of chunk EmptyBP4" width="490" />

All groups and occassions recieve identical prediction: the grand mean. 

The syntax of ggplot2 allows us building graphs layer by layer. Let us combine observed values (colored lines) and predicted values (black lines) on the same graph. We accomplish this by taking advantage of the plot- and geom- specific mapping of data


```r
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
```

<img src="figure_rmd/EmptyBP5.png" title="plot of chunk EmptyBP5" alt="plot of chunk EmptyBP5" width="490" />


### Run m3.2
Similarly, we can fit "A Within-Person Empty Model"




