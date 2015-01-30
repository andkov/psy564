---
title: "Reporting Statistical Models (demo)"
author: "Psyc 564"
date: "Friday, January 30, 2015"
output:
  html_document:
    css: ~/GitHub/psy564/libs/css/sidebar.css
    highlight: haddock
    theme: spacelab
    toc: yes
---

<!--  Set the working directory to the repository's base directory; this assumes the report is nested inside of only one directory.-->


<!-- Set the report-wide options, and point to the external script file. -->



# Resources

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

# Data prep

 Today we will look at the data example from chapter 5 of [Longitudinal Analysis](http://www.pilesofvariance.com/) by Lesa Hoffman. You will find all the necessary files at

```
 ./Chapters/05/R_Chapter5/
```
 after you [download and unzip](https://github.com/andkov/psy564/archive/master.zip) the folder of this repository.

```
 ./Chapters/05/R_Chapter5/hoffman-ch5.R
```
 contains code that conducts the analysis and 
   
```
 ./Chapters/03/R_Chapter3/hoffman-ch3.Rmd
```
contains the [report](https://github.com/andkov/psy564/blob/master/Chapters/03/R_Chapter3/hoffman-ch3.html). Html file must be compiled before it can be viewed. You can view .html files stored on GitHub.com by pasting their url address into [HTMLpreviewer](http://htmlpreview.github.io/?)

#### Working files
 - [hoffman-ch5.R](https://github.com/andkov/psy564/blob/master/Chapters/03/R_Chapter3/hoffman-ch3.R) contains the script that replicates **example 3a** from Chapter 3 of [Longitudinal Analysis](http://www.pilesofvariance.com/) by Lesa Hoffman. We fit empty between-person (3.1) and empty within-person (3.2) models to the supplied dataset containing 2 measures for 50 subjects.
 - [hoffman-ch5.Rmd](http://htmlpreview.github.io/?https://github.com/andkov/psy564/blob/master/Chapters/03/R_Chapter3/hoffman-ch3.html) contains annotations to what happens in <code>hoffman-ch5.R</code> script. HTML is "knitted" in RStudio by knitr package by combining .R and .Rmd files: explainations for computer and for humans, respectively.  This report is a self-contained account of two statistical models.  
 - [hoffman-ch5.html]() the webpage you are reading right now
 

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
# ds0   <- read.sas7bdat(pathFile, debug=TRUE) # import file 
ds0 <- readRDS("~/GitHub/psy564/Data/Raw/ELSA/ds0_ELSA.rds")
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
[1] 51531    32
```

```r
names(ds0) # what are column names?
```

```
 [1] "id"        "dob"       "sex"       "age"       "edu"       "ht"        "db"        "htdb"      "nocase"   
[10] "ang1A"     "chf1A"     "mi1A"      "stk1A"     "cogflg1A"  "evrsmk1A"  "htnw1A"    "dmw1A"     "time"     
[19] "irecall"   "animal"    "prospect"  "drecall"   "age80"     "edu11"     "hptn"      "dbts"      "condition"
[28] "wave"      "year"      "Age"       "waveF"     "female"   
```

```r
str(ds0) # what its structure?
```

```
'data.frame':	51531 obs. of  32 variables:
 $ id       : int  103712 103712 103712 103712 103712 103712 103713 103713 103713 103713 ...
 $ dob      : int  1947 1947 1947 1947 1947 1947 1931 1931 1931 1931 ...
 $ sex      : int  1 1 1 1 1 1 0 0 0 0 ...
 $ age      : int  55 55 55 55 55 55 71 71 71 71 ...
 $ edu      : int  10 10 10 10 10 10 9 9 9 9 ...
 $ ht       : int  0 0 0 0 0 0 0 0 0 0 ...
 $ db       : int  0 0 0 0 0 0 0 0 0 0 ...
 $ htdb     : int  1 1 1 1 1 1 0 0 0 0 ...
 $ nocase   : int  0 0 0 0 0 0 1 1 1 1 ...
 $ ang1A    : int  0 0 0 0 0 0 0 0 0 0 ...
 $ chf1A    : int  0 0 0 0 0 0 0 0 0 0 ...
 $ mi1A     : int  0 0 0 0 0 0 0 0 0 0 ...
 $ stk1A    : int  0 0 0 0 0 0 1 1 1 1 ...
 $ cogflg1A : int  0 0 0 0 0 0 0 0 0 0 ...
 $ evrsmk1A : int  1 1 1 1 1 1 1 1 1 1 ...
 $ htnw1A   : int  1 1 1 1 1 1 0 0 0 0 ...
 $ dmw1A    : int  1 1 1 1 1 1 0 0 0 0 ...
 $ time     : num  -4.54 0 2.5 4.33 6.08 ...
 $ irecall  : num  NA 6 6 10 7 NA NA 5 3 4 ...
 $ animal   : int  NA 18 24 15 16 NA NA 10 9 8 ...
 $ prospect : int  NA 2 5 5 5 5 NA 0 0 0 ...
 $ drecall  : int  NA 6 6 8 7 NA NA 1 4 2 ...
 $ age80    : num  -25 -25 -25 -25 -25 -25 -9 -9 -9 -9 ...
 $ edu11    : num  -1 -1 -1 -1 -1 -1 -2 -2 -2 -2 ...
 $ hptn     : Ord.factor w/ 2 levels "No HPTN"<"Hypertension": 2 2 2 2 2 2 1 1 1 1 ...
 $ dbts     : Ord.factor w/ 2 levels "No DBTS"<"Diabetes": 2 2 2 2 2 2 1 1 1 1 ...
 $ condition: Ord.factor w/ 4 levels "Hypertension"<..: 3 3 3 3 3 3 4 4 4 4 ...
 $ wave     : num  0 1 2 3 4 5 0 1 2 3 ...
 $ year     : num  2002 2004 2006 2008 2010 ...
 $ Age      : num  55 57 59 61 63 65 71 73 75 77 ...
 $ waveF    : Ord.factor w/ 6 levels "A"<"B"<"C"<"D"<..: 1 2 3 4 5 6 1 2 3 4 ...
 $ female   : Ord.factor w/ 2 levels "Male"<"Female": 2 2 2 2 2 2 1 1 1 1 ...
```

```r
head(ds0) # what do first few lines look like?
```

```
      id  dob sex age edu ht db htdb nocase ang1A chf1A mi1A stk1A cogflg1A evrsmk1A htnw1A dmw1A   time irecall animal
1 103712 1947   1  55  10  0  0    1      0     0     0    0     0        0        1      1     1 -4.542      NA     NA
2 103712 1947   1  55  10  0  0    1      0     0     0    0     0        0        1      1     1  0.000       6     18
3 103712 1947   1  55  10  0  0    1      0     0     0    0     0        0        1      1     1  2.500       6     24
4 103712 1947   1  55  10  0  0    1      0     0     0    0     0        0        1      1     1  4.333      10     15
5 103712 1947   1  55  10  0  0    1      0     0     0    0     0        0        1      1     1  6.083       7     16
6 103712 1947   1  55  10  0  0    1      0     0     0    0     0        0        1      1     1  8.167      NA     NA
  prospect drecall age80 edu11         hptn     dbts condition wave year Age waveF female
1       NA      NA   -25    -1 Hypertension Diabetes      Both    0 2002  55     A Female
2        2       6   -25    -1 Hypertension Diabetes      Both    1 2004  57     B Female
3        5       6   -25    -1 Hypertension Diabetes      Both    2 2006  59     C Female
4        5       8   -25    -1 Hypertension Diabetes      Both    3 2008  61     D Female
5        5       7   -25    -1 Hypertension Diabetes      Both    4 2010  63     E Female
6        5      NA   -25    -1 Hypertension Diabetes      Both    5 2012  65     F Female
```

```r
base::summary(ds0) # basic summary
```

```
       id              dob            sex           age            edu             ht              db        
 Min.   :103712   Min.   :1907   Min.   :0.0   Min.   :50.0   Min.   : 0.0   Min.   :0.000   Min.   :0.0000  
 1st Qu.:106732   1st Qu.:1930   1st Qu.:0.0   1st Qu.:56.0   1st Qu.:10.0   1st Qu.:0.000   1st Qu.:0.0000  
 Median :111733   Median :1938   Median :1.0   Median :63.0   Median :10.0   Median :0.000   Median :0.0000  
 Mean   :112304   Mean   :1937   Mean   :0.6   Mean   :64.4   Mean   :10.8   Mean   :0.328   Mean   :0.0243  
 3rd Qu.:118375   3rd Qu.:1946   3rd Qu.:1.0   3rd Qu.:72.0   3rd Qu.:12.0   3rd Qu.:1.000   3rd Qu.:0.0000  
 Max.   :121300   Max.   :1952   Max.   :1.0   Max.   :95.0   Max.   :14.0   Max.   :1.000   Max.   :1.0000  
                  NA's   :275    NA's   :532   NA's   :89     NA's   :1689                                   
      htdb            nocase          ang1A            chf1A             mi1A            stk1A           cogflg1A
 Min.   :0.0000   Min.   :0.000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0  
 1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0  
 Median :0.0000   Median :1.000   Median :0.0000   Median :0.0000   Median :0.0000   Median :0.0000   Median :0  
 Mean   :0.0438   Mean   :0.604   Mean   :0.0901   Mean   :0.0056   Mean   :0.0535   Mean   :0.0377   Mean   :0  
 3rd Qu.:0.0000   3rd Qu.:1.000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0  
 Max.   :1.0000   Max.   :1.000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :0  
                                                                                                                 
    evrsmk1A         htnw1A          dmw1A             time          irecall          animal         prospect    
 Min.   :0.000   Min.   :0.000   Min.   :0.0000   Min.   :-5.21   Min.   : 0      Min.   : 0      Min.   :0      
 1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.0000   1st Qu.: 0.00   1st Qu.: 5      1st Qu.:15      1st Qu.:2      
 Median :1.000   Median :0.000   Median :0.0000   Median : 2.25   Median : 6      Median :19      Median :5      
 Mean   :0.631   Mean   :0.372   Mean   :0.0681   Mean   : 2.07   Mean   : 6      Mean   :20      Mean   :4      
 3rd Qu.:1.000   3rd Qu.:1.000   3rd Qu.:0.0000   3rd Qu.: 5.00   3rd Qu.: 7      3rd Qu.:24      3rd Qu.:5      
 Max.   :1.000   Max.   :1.000   Max.   :1.0000   Max.   : 9.08   Max.   :10      Max.   :63      Max.   :5      
 NA's   :13                                                       NA's   :18253   NA's   :18246   NA's   :12791  
    drecall          age80           edu11                 hptn             dbts              condition    
 Min.   : 0      Min.   :-30.0   Min.   :-11.0   No HPTN     :32359   No DBTS :48021   Hypertension:16916  
 1st Qu.: 3      1st Qu.:-24.0   1st Qu.: -1.0   Hypertension:19172   Diabetes: 3510   Diabetes    : 1254  
 Median : 4      Median :-17.0   Median : -1.0                                         Both        : 2256  
 Mean   : 4      Mean   :-15.6   Mean   : -0.2                                         None        :31105  
 3rd Qu.: 6      3rd Qu.: -8.0   3rd Qu.:  1.0                                                             
 Max.   :10      Max.   : 15.0   Max.   :  3.0                                                             
 NA's   :18238   NA's   :89      NA's   :1689                                                              
      wave           year           Age        waveF        female     
 Min.   :0.00   Min.   :2002   Min.   : 50.0   A:11317   Male  :22684  
 1st Qu.:1.00   1st Qu.:2004   1st Qu.: 61.0   B:11266   Female:28315  
 Median :2.00   Median :2006   Median : 68.0   C: 9329   NA's  :  532  
 Mean   :2.05   Mean   :2006   Mean   : 68.8   D: 7849                 
 3rd Qu.:3.00   3rd Qu.:2008   3rd Qu.: 76.0   E: 6522                 
 Max.   :5.00   Max.   :2012   Max.   :104.0   F: 5248                 
                               NA's   :275                             
```

```r
psych::describe(ds0) # summary by psych package
```

```
           vars     n      mean      sd    median   trimmed     mad       min       max    range  skew kurtosis    se
id            1 51531 112303.70 5895.77 111733.00 112251.18 8628.73 103712.00 121300.00 17588.00  0.11    -1.56 25.97
dob           2 51256   1937.28    9.78   1938.00   1937.95   11.86   1907.00   1952.00    45.00 -0.49    -0.66  0.04
sex           3 50999      0.56    0.50      1.00      0.57    0.00      0.00      1.00     1.00 -0.22    -1.95  0.00
age           4 51442     64.36    9.77     63.00     63.69   11.86     50.00     95.00    45.00  0.49    -0.65  0.04
edu           5 49842     10.75    1.77     10.00     10.61    1.48      0.00     14.00    14.00 -0.25     4.75  0.01
ht            6 51531      0.33    0.47      0.00      0.29    0.00      0.00      1.00     1.00  0.73    -1.47  0.00
db            7 51531      0.02    0.15      0.00      0.00    0.00      0.00      1.00     1.00  6.17    36.12  0.00
htdb          8 51531      0.04    0.20      0.00      0.00    0.00      0.00      1.00     1.00  4.46    17.89  0.00
nocase        9 51531      0.60    0.49      1.00      0.63    0.00      0.00      1.00     1.00 -0.42    -1.82  0.00
ang1A        10 51531      0.09    0.29      0.00      0.00    0.00      0.00      1.00     1.00  2.86     6.19  0.00
chf1A        11 51531      0.01    0.07      0.00      0.00    0.00      0.00      1.00     1.00 13.26   173.93  0.00
mi1A         12 51531      0.05    0.22      0.00      0.00    0.00      0.00      1.00     1.00  3.97    13.75  0.00
stk1A        13 51531      0.04    0.19      0.00      0.00    0.00      0.00      1.00     1.00  4.85    21.55  0.00
cogflg1A     14 51531      0.00    0.00      0.00      0.00    0.00      0.00      0.00     0.00   NaN      NaN  0.00
evrsmk1A     15 51518      0.63    0.48      1.00      0.66    0.00      0.00      1.00     1.00 -0.54    -1.71  0.00
htnw1A       16 51531      0.37    0.48      0.00      0.34    0.00      0.00      1.00     1.00  0.53    -1.72  0.00
dmw1A        17 51531      0.07    0.25      0.00      0.00    0.00      0.00      1.00     1.00  3.43     9.75  0.00
time         18 51531      2.07    3.87      2.25      2.11    3.34     -5.21      9.08    14.29 -0.04    -0.94  0.02
irecall      19 33278      5.56    1.80      6.00      5.65    1.48      0.00     10.00    10.00 -0.51     0.41  0.01
animal       20 33285     19.63    6.65     19.00     19.51    5.93      0.00     63.00    63.00  0.17     0.85  0.04
prospect     21 38740      3.59    1.83      5.00      3.86    0.00      0.00      5.00     5.00 -0.78    -0.94  0.01
drecall      22 33293      4.18    2.13      4.00      4.26    1.48      0.00     10.00    10.00 -0.23    -0.33  0.01
age80        23 51442    -15.64    9.77    -17.00    -16.31   11.86    -30.00     15.00    45.00  0.49    -0.65  0.04
edu11        24 49842     -0.25    1.77     -1.00     -0.39    1.48    -11.00      3.00    14.00 -0.25     4.75  0.01
hptn*        25 51531      1.37    0.48      1.00      1.34    0.00      1.00      2.00     1.00  0.53    -1.72  0.00
dbts*        26 51531      1.07    0.25      1.00      1.00    0.00      1.00      2.00     1.00  3.43     9.75  0.00
condition*   27 51531      2.92    1.39      4.00      3.03    0.00      1.00      4.00     3.00 -0.59    -1.58  0.01
wave         28 51531      2.05    1.63      2.00      1.94    1.48      0.00      5.00     5.00  0.35    -1.06  0.01
year         29 51531   2006.11    3.27   2006.00   2005.88    2.97   2002.00   2012.00    10.00  0.35    -1.06  0.01
Age          30 51256     68.84   10.07     68.00     68.37   11.86     50.00    104.00    54.00  0.37    -0.65  0.04
waveF*       31 51531      3.05    1.63      3.00      2.94    1.48      1.00      6.00     5.00  0.35    -1.06  0.01
female*      32 50999      1.56    0.50      2.00      1.57    0.00      1.00      2.00     1.00 -0.22    -1.95  0.00
```

```r
table(ds0$wave)# one-way table
```

```

    0     1     2     3     4     5 
11317 11266  9329  7849  6522  5248 
```


## Wide to Long

Our data were in a long format, with each level of the variable *wave* recorded in a single column 

```r
head(ds0)
```

```
      id  dob sex age edu ht db htdb nocase ang1A chf1A mi1A stk1A cogflg1A evrsmk1A htnw1A dmw1A   time irecall animal
1 103712 1947   1  55  10  0  0    1      0     0     0    0     0        0        1      1     1 -4.542      NA     NA
2 103712 1947   1  55  10  0  0    1      0     0     0    0     0        0        1      1     1  0.000       6     18
3 103712 1947   1  55  10  0  0    1      0     0     0    0     0        0        1      1     1  2.500       6     24
4 103712 1947   1  55  10  0  0    1      0     0     0    0     0        0        1      1     1  4.333      10     15
5 103712 1947   1  55  10  0  0    1      0     0     0    0     0        0        1      1     1  6.083       7     16
6 103712 1947   1  55  10  0  0    1      0     0     0    0     0        0        1      1     1  8.167      NA     NA
  prospect drecall age80 edu11         hptn     dbts condition wave year Age waveF female
1       NA      NA   -25    -1 Hypertension Diabetes      Both    0 2002  55     A Female
2        2       6   -25    -1 Hypertension Diabetes      Both    1 2004  57     B Female
3        5       6   -25    -1 Hypertension Diabetes      Both    2 2006  59     C Female
4        5       8   -25    -1 Hypertension Diabetes      Both    3 2008  61     D Female
5        5       7   -25    -1 Hypertension Diabetes      Both    4 2010  63     E Female
6        5      NA   -25    -1 Hypertension Diabetes      Both    5 2012  65     F Female
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
```
Suffix "M" in "dsM" signifies that the data is ready for  **M**odeling

# Graphing

To better understand the patterns we visualize the data with a simple line plot
































