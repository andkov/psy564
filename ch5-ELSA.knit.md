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
ds0 <- readRDS("~/GitHub/psy564/Data/Derived/ELSA/dsM_ELSA.rds")
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
[1] 22744    32
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
'data.frame':	22744 obs. of  32 variables:
 $ id       : int  103712 103712 103712 103712 103714 103714 103714 103714 103720 103720 ...
 $ dob      : int  1947 1947 1947 1947 1950 1950 1950 1950 1950 1950 ...
 $ sex      : int  1 1 1 1 1 1 1 1 0 0 ...
 $ age      : int  55 55 55 55 51 51 51 51 51 51 ...
 $ edu      : int  10 10 10 10 11 11 11 11 11 11 ...
 $ ht       : int  0 0 0 0 1 1 1 1 0 0 ...
 $ db       : int  0 0 0 0 0 0 0 0 0 0 ...
 $ htdb     : int  1 1 1 1 0 0 0 0 0 0 ...
 $ nocase   : int  0 0 0 0 0 0 0 0 1 1 ...
 $ ang1A    : int  0 0 0 0 0 0 0 0 0 0 ...
 $ chf1A    : int  0 0 0 0 0 0 0 0 0 0 ...
 $ mi1A     : int  0 0 0 0 0 0 0 0 0 0 ...
 $ stk1A    : int  0 0 0 0 0 0 0 0 0 0 ...
 $ cogflg1A : int  0 0 0 0 0 0 0 0 0 0 ...
 $ evrsmk1A : int  1 1 1 1 0 0 0 0 1 1 ...
 $ htnw1A   : int  1 1 1 1 1 1 1 1 0 0 ...
 $ dmw1A    : int  1 1 1 1 0 0 0 0 0 0 ...
 $ time     : num  0 2.5 4.33 6.08 0 ...
 $ irecall  : num  6 6 10 7 8 6 7 7 9 9 ...
 $ animal   : int  18 24 15 16 33 27 19 28 21 22 ...
 $ prospect : int  2 5 5 5 0 5 5 5 5 1 ...
 $ drecall  : int  6 6 8 7 7 6 6 7 8 7 ...
 $ age80    : num  -25 -25 -25 -25 -29 -29 -29 -29 -29 -29 ...
 $ edu11    : num  -1 -1 -1 -1 0 0 0 0 0 0 ...
 $ hptn     : Ord.factor w/ 2 levels "No HPTN"<"Hypertension": 2 2 2 2 2 2 2 2 1 1 ...
 $ dbts     : Ord.factor w/ 2 levels "No DBTS"<"Diabetes": 2 2 2 2 1 1 1 1 1 1 ...
 $ condition: Ord.factor w/ 4 levels "Hypertension"<..: 3 3 3 3 1 1 1 1 4 4 ...
 $ wave     : num  1 2 3 4 1 2 3 4 1 2 ...
 $ year     : num  2004 2006 2008 2010 2004 ...
 $ Age      : num  57 59 61 63 54 56 58 60 54 56 ...
 $ waveF    : Ord.factor w/ 6 levels "A"<"B"<"C"<"D"<..: 2 3 4 5 2 3 4 5 2 3 ...
 $ female   : Ord.factor w/ 2 levels "Male"<"Female": 2 2 2 2 2 2 2 2 1 1 ...
```

```r
head(ds0) # what do first few lines look like?
```

```
      id  dob sex age edu ht db htdb nocase ang1A chf1A mi1A stk1A cogflg1A evrsmk1A htnw1A dmw1A  time irecall animal
1 103712 1947   1  55  10  0  0    1      0     0     0    0     0        0        1      1     1 0.000       6     18
2 103712 1947   1  55  10  0  0    1      0     0     0    0     0        0        1      1     1 2.500       6     24
3 103712 1947   1  55  10  0  0    1      0     0     0    0     0        0        1      1     1 4.333      10     15
4 103712 1947   1  55  10  0  0    1      0     0     0    0     0        0        1      1     1 6.083       7     16
8 103714 1950   1  51  11  1  0    0      0     0     0    0     0        0        0      1     0 0.000       8     33
9 103714 1950   1  51  11  1  0    0      0     0     0    0     0        0        0      1     0 2.333       6     27
  prospect drecall age80 edu11         hptn     dbts    condition wave year Age waveF female
1        2       6   -25    -1 Hypertension Diabetes         Both    1 2004  57     B Female
2        5       6   -25    -1 Hypertension Diabetes         Both    2 2006  59     C Female
3        5       8   -25    -1 Hypertension Diabetes         Both    3 2008  61     D Female
4        5       7   -25    -1 Hypertension Diabetes         Both    4 2010  63     E Female
8        0       7   -29     0 Hypertension  No DBTS Hypertension    1 2004  54     B Female
9        5       6   -29     0 Hypertension  No DBTS Hypertension    2 2006  56     C Female
```

```r
base::summary(ds0) # basic summary
```

```
       id              dob            sex             age            edu             ht             db        
 Min.   :103712   Min.   :1908   Min.   :0.000   Min.   :50.0   Min.   : 0.0   Min.   :0.00   Min.   :0.0000  
 1st Qu.:106784   1st Qu.:1932   1st Qu.:0.000   1st Qu.:55.0   1st Qu.:10.0   1st Qu.:0.00   1st Qu.:0.0000  
 Median :111779   Median :1940   Median :1.000   Median :62.0   Median :10.0   Median :0.00   Median :0.0000  
 Mean   :112413   Mean   :1938   Mean   :0.563   Mean   :63.2   Mean   :10.9   Mean   :0.32   Mean   :0.0197  
 3rd Qu.:118492   3rd Qu.:1946   3rd Qu.:1.000   3rd Qu.:70.0   3rd Qu.:12.0   3rd Qu.:1.00   3rd Qu.:0.0000  
 Max.   :121299   Max.   :1952   Max.   :1.000   Max.   :94.0   Max.   :14.0   Max.   :1.00   Max.   :1.0000  
                  NA's   :4                      NA's   :4      NA's   :540                                   
      htdb            nocase          ang1A            chf1A             mi1A            stk1A          cogflg1A
 Min.   :0.0000   Min.   :0.000   Min.   :0.0000   Min.   :0.0000   Min.   :0.0000   Min.   :0.000   Min.   :0  
 1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.0000   1st Qu.:0.000   1st Qu.:0  
 Median :0.0000   Median :1.000   Median :0.0000   Median :0.0000   Median :0.0000   Median :0.000   Median :0  
 Mean   :0.0397   Mean   :0.621   Mean   :0.0783   Mean   :0.0033   Mean   :0.0433   Mean   :0.028   Mean   :0  
 3rd Qu.:0.0000   3rd Qu.:1.000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.0000   3rd Qu.:0.000   3rd Qu.:0  
 Max.   :1.0000   Max.   :1.000   Max.   :1.0000   Max.   :1.0000   Max.   :1.0000   Max.   :1.000   Max.   :0  
                                                                                                                
    evrsmk1A         htnw1A          dmw1A             time         irecall          animal        prospect   
 Min.   :0.000   Min.   :0.000   Min.   :0.0000   Min.   :0.00   Min.   : 0.00   Min.   : 0.0   Min.   :0.00  
 1st Qu.:0.000   1st Qu.:0.000   1st Qu.:0.0000   1st Qu.:1.38   1st Qu.: 5.00   1st Qu.:16.0   1st Qu.:2.00  
 Median :1.000   Median :0.000   Median :0.0000   Median :3.21   Median : 6.00   Median :20.0   Median :5.00  
 Mean   :0.624   Mean   :0.359   Mean   :0.0594   Mean   :3.15   Mean   : 5.78   Mean   :20.4   Mean   :3.74  
 3rd Qu.:1.000   3rd Qu.:1.000   3rd Qu.:0.0000   3rd Qu.:5.27   3rd Qu.: 7.00   3rd Qu.:24.0   3rd Qu.:5.00  
 Max.   :1.000   Max.   :1.000   Max.   :1.0000   Max.   :7.25   Max.   :10.00   Max.   :63.0   Max.   :5.00  
                                                                                 NA's   :13     NA's   :177   
    drecall          age80           edu11                 hptn             dbts              condition    
 Min.   : 0.00   Min.   :-30.0   Min.   :-11.0   No HPTN     :14568   No DBTS :21392   Hypertension: 7272  
 1st Qu.: 3.00   1st Qu.:-25.0   1st Qu.: -1.0   Hypertension: 8176   Diabetes: 1352   Diabetes    :  448  
 Median : 5.00   Median :-18.0   Median : -1.0                                         Both        :  904  
 Mean   : 4.48   Mean   :-16.8   Mean   : -0.1                                         None        :14120  
 3rd Qu.: 6.00   3rd Qu.:-10.0   3rd Qu.:  1.0                                                             
 Max.   :10.00   Max.   : 14.0   Max.   :  3.0                                                             
 NA's   :9       NA's   :4       NA's   :540                                                               
      wave           year           Age        waveF       female     
 Min.   :1.00   Min.   :2004   Min.   : 52.0   A:   0   Male  : 9948  
 1st Qu.:1.75   1st Qu.:2006   1st Qu.: 61.0   B:5686   Female:12796  
 Median :2.50   Median :2007   Median : 67.0   C:5686                 
 Mean   :2.50   Mean   :2007   Mean   : 68.6   D:5686                 
 3rd Qu.:3.25   3rd Qu.:2008   3rd Qu.: 75.0   E:5686                 
 Max.   :4.00   Max.   :2010   Max.   :102.0   F:   0                 
                               NA's   :4                              
```

```r
psych::describe(ds0) # summary by psych package
```

```
           vars     n      mean      sd    median   trimmed     mad    min       max    range  skew kurtosis    se
id            1 22744 112413.21 5924.05 111779.00 112383.80 8670.99 103712 121299.00 17587.00  0.08    -1.57 39.28
dob           2 22740   1938.39    9.06   1940.00   1939.02   10.38   1908   1952.00    44.00 -0.50    -0.64  0.06
sex           3 22744      0.56    0.50      1.00      0.58    0.00      0      1.00     1.00 -0.25    -1.94  0.00
age           4 22740     63.24    9.04     62.00     62.61   10.38     50     94.00    44.00  0.50    -0.63  0.06
edu           5 22204     10.91    1.76     10.00     10.81    1.48      0     14.00    14.00 -0.22     4.06  0.01
ht            6 22744      0.32    0.47      0.00      0.27    0.00      0      1.00     1.00  0.77    -1.40  0.00
db            7 22744      0.02    0.14      0.00      0.00    0.00      0      1.00     1.00  6.91    45.78  0.00
htdb          8 22744      0.04    0.20      0.00      0.00    0.00      0      1.00     1.00  4.71    20.20  0.00
nocase        9 22744      0.62    0.49      1.00      0.65    0.00      0      1.00     1.00 -0.50    -1.75  0.00
ang1A        10 22744      0.08    0.27      0.00      0.00    0.00      0      1.00     1.00  3.14     7.86  0.00
chf1A        11 22744      0.00    0.06      0.00      0.00    0.00      0      1.00     1.00 17.21   294.24  0.00
mi1A         12 22744      0.04    0.20      0.00      0.00    0.00      0      1.00     1.00  4.49    18.16  0.00
stk1A        13 22744      0.03    0.16      0.00      0.00    0.00      0      1.00     1.00  5.73    30.79  0.00
cogflg1A     14 22744      0.00    0.00      0.00      0.00    0.00      0      0.00     0.00   NaN      NaN  0.00
evrsmk1A     15 22744      0.62    0.48      1.00      0.65    0.00      0      1.00     1.00 -0.51    -1.74  0.00
htnw1A       16 22744      0.36    0.48      0.00      0.32    0.00      0      1.00     1.00  0.59    -1.66  0.00
dmw1A        17 22744      0.06    0.24      0.00      0.00    0.00      0      1.00     1.00  3.73    11.88  0.00
time         18 22744      3.15    2.29      3.21      3.13    3.09      0      7.25     7.25 -0.04    -1.26  0.02
irecall      19 22744      5.78    1.71      6.00      5.87    1.48      0     10.00    10.00 -0.52     0.60  0.01
animal       20 22731     20.45    6.47     20.00     20.29    5.93      0     63.00    63.00  0.23     0.97  0.04
prospect     21 22567      3.74    1.75      5.00      4.03    0.00      0      5.00     5.00 -0.92    -0.68  0.01
drecall      22 22735      4.48    2.03      5.00      4.56    1.48      0     10.00    10.00 -0.27    -0.08  0.01
age80        23 22740    -16.76    9.04    -18.00    -17.39   10.38    -30     14.00    44.00  0.50    -0.63  0.06
edu11        24 22204     -0.09    1.76     -1.00     -0.19    1.48    -11      3.00    14.00 -0.22     4.06  0.01
hptn*        25 22744      1.36    0.48      1.00      1.32    0.00      1      2.00     1.00  0.59    -1.66  0.00
dbts*        26 22744      1.06    0.24      1.00      1.00    0.00      1      2.00     1.00  3.73    11.88  0.00
condition*   27 22744      2.96    1.38      4.00      3.08    0.00      1      4.00     3.00 -0.65    -1.51  0.01
wave         28 22744      2.50    1.12      2.50      2.50    1.48      1      4.00     3.00  0.00    -1.36  0.01
year         29 22744   2007.00    2.24   2007.00   2007.00    2.97   2004   2010.00     6.00  0.00    -1.36  0.01
Age          30 22740     68.61    9.33     67.00     68.06   10.38     52    102.00    50.00  0.46    -0.57  0.06
waveF*       31 22744      3.50    1.12      3.50      3.50    1.48      2      5.00     3.00  0.00    -1.36  0.01
female*      32 22744      1.56    0.50      2.00      1.58    0.00      1      2.00     1.00 -0.25    -1.94  0.00
```

```r
table(ds0$wave)# one-way table
```

```

   1    2    3    4 
5686 5686 5686 5686 
```


## Wide to Long

Our data were in a long format, with each level of the variable *wave* recorded in a single column 

```r
head(ds0)
```

```
      id  dob sex age edu ht db htdb nocase ang1A chf1A mi1A stk1A cogflg1A evrsmk1A htnw1A dmw1A  time irecall animal
1 103712 1947   1  55  10  0  0    1      0     0     0    0     0        0        1      1     1 0.000       6     18
2 103712 1947   1  55  10  0  0    1      0     0     0    0     0        0        1      1     1 2.500       6     24
3 103712 1947   1  55  10  0  0    1      0     0     0    0     0        0        1      1     1 4.333      10     15
4 103712 1947   1  55  10  0  0    1      0     0     0    0     0        0        1      1     1 6.083       7     16
8 103714 1950   1  51  11  1  0    0      0     0     0    0     0        0        0      1     0 0.000       8     33
9 103714 1950   1  51  11  1  0    0      0     0     0    0     0        0        0      1     0 2.333       6     27
  prospect drecall age80 edu11         hptn     dbts    condition wave year Age waveF female
1        2       6   -25    -1 Hypertension Diabetes         Both    1 2004  57     B Female
2        5       6   -25    -1 Hypertension Diabetes         Both    2 2006  59     C Female
3        5       8   -25    -1 Hypertension Diabetes         Both    3 2008  61     D Female
4        5       7   -25    -1 Hypertension Diabetes         Both    4 2010  63     E Female
8        0       7   -29     0 Hypertension  No DBTS Hypertension    1 2004  54     B Female
9        5       6   -29     0 Hypertension  No DBTS Hypertension    2 2006  56     C Female
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
dsM <- dplyr::filter(dsM, id %in% sample(unique(id),100))
```
Suffix "M" in "dsM" signifies that the data is ready for  **M**odeling

# Graphing

To better understand the patterns we visualize the data with a simple line plot

```r
p <- ggplot2::ggplot(dsM,aes(x=wave,y=irecall)) # map data dimension
p <- p + geom_line(aes(group=id)) # draw lines and map unit of measurement
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
p <- ggplot2::ggplot(dsM,aes(x=wave,y=irecall))
p <- p + geom_line(aes(group=id)) # draw lines
p <- p + theme1 
p <- p + scale_x_continuous(limits=c(1,4), breaks=c(1:4)) # X axis
p <- p + scale_y_continuous(limits=c(0,10), 
                            breaks=seq(0,10, by=1)) # Y axis
p <- p + labs(list(
  title="Does outcome change over time?", # main title
  x="Wave of measurement", y="Performance on the outcome")) # axes titles
p
```

<img src="figure_rmd/GraphingData1.png" title="plot of chunk GraphingData1" alt="plot of chunk GraphingData1" width="490" />

black lines give us a longitudinal perspective on the data, depicting the observed trajectories of individuals. To juxtapose is with cross-sectional perspective, we can add a different geom, styled to contrast the difference 


```r
p <- ggplot2::ggplot(dsM,aes(x=wave,y=irecall))
p <- p + geom_line(aes(group=id)) 
p <- p + geom_point(size=6, shape=21, fill="purple", color="black", alpha=.5)
p <- p + theme1 
p <- p + scale_x_continuous(limits=c(1,4), breaks=c(1:4)) # X axis
p <- p + scale_y_continuous(limits=c(0,10), 
                            breaks=seq(0,10, by=1)) # Y axis
p <- p + labs(list(
  title="Does outcome change over time?", # main title
  x="Wave of measurement", y="Performance on the outcome")) # axes titles
p
```

<img src="figure_rmd/GraphingData2.png" title="plot of chunk GraphingData2" alt="plot of chunk GraphingData2" width="490" />



# Modeling 

## 3.1
The baseline for the models to follow is the empty between-person model with no predictors


$\begin{array}{l}
{y_{ti}} = {\beta _{0i}} + {\varepsilon _{ti}}\\
{\beta _{0i}} = {\gamma _{00}}\\
{}\\
{y_{ti}} = {\gamma _{00}} + {\varepsilon _{ti}}
\end{array}$


### Run m3.1

```r
m3.1 <- nlme::gls(irecall ~ 1, data=dsM, method="ML") # create model object
dsM$m3.1 <- predict(m3.1) # stores values predicted by the model
summary(m3.1) # print a bit more info
```

```
Generalized least squares fit by maximum likelihood
  Model: irecall ~ 1 
  Data: dsM 
   AIC  BIC logLik
  1518 1526 -757.2

Coefficients:
            Value Std.Error t-value p-value
(Intercept) 5.872   0.08043   73.01       0

Standardized residuals:
     Min       Q1      Med       Q3      Max 
-3.03276 -0.54306  0.07936  0.70178  2.56905 

Residual standard error: 1.607 
Degrees of freedom: 400 total; 399 residual
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
  Model: irecall ~ 1 
  Data: dsM 
  Log-likelihood: -757.2

Coefficients:
(Intercept) 
      5.872 

Degrees of freedom: 400 total; 399 residual
Residual standard error: 1.607 
```

```r
mi3.1 #  model information
```

```
           m3.1 Coefficient
logLik   -757.2      logLik
deviance 1514.5    deviance
AIC      1518.5         AIC
BIC      1526.4         BIC
df.resid  399.0    df.resid
N         400.0           N
p           1.0           p
ids         0.0         ids
```

```r
head(dsM) # visual check
```

```
      id  dob sex age edu ht db htdb nocase ang1A chf1A mi1A stk1A cogflg1A evrsmk1A htnw1A dmw1A  time irecall animal
1 103724 1935   1  67  13  0  0    0      1     0     0    0     0        0        1      0     0 0.000       6     23
2 103724 1935   1  67  13  0  0    0      1     0     0    0     0        0        1      0     0 2.250       6     16
3 103724 1935   1  67  13  0  0    0      1     0     0    0     0        0        1      0     0 4.083       6     30
4 103724 1935   1  67  13  0  0    0      1     0     0    0     0        0        1      0     0 6.083       7     32
5 103986 1946   1  56  14  0  0    0      1     0     0    0     0        0        0      0     0 0.000       7     27
6 103986 1946   1  56  14  0  0    0      1     0     0    0     0        0        0      0     0 2.000       6     24
  prospect drecall age80 edu11    hptn    dbts condition wave year Age waveF female  m3.1
1        2       5   -13     2 No HPTN No DBTS      None    1 2004  69     B Female 5.872
2        5       5   -13     2 No HPTN No DBTS      None    2 2006  71     C Female 5.872
3        5       6   -13     2 No HPTN No DBTS      None    3 2008  73     D Female 5.872
4        5       6   -13     2 No HPTN No DBTS      None    4 2010  75     E Female 5.872
5        5       7   -24     3 No HPTN No DBTS      None    1 2004  58     B Female 5.872
6        5       5   -24     3 No HPTN No DBTS      None    2 2006  60     C Female 5.872
```

### Graph m3.1




















