* Prevent "more" messages from appearing
set more off
* Control line length
set linesize 150

********************************************************************************
*******             BEGIN DATA MANIPULATION FOR CHAPTER 2 EXAMPLE        *******
*******               CHANGE "filesave" to your directory                *******
********************************************************************************

* Defining global variable for file location to be replaced in code below
global filesave "C:\Dropbox\PilesOfVariance\Chapter2\STATA"

* Import chapter 2 data into temporary file and center predictors
use "$filesave\STATA_Chapter2.dta", clear
* Centering age at different points
gen age80 = age - 80
gen age85 = age - 85
gen age90 = age - 90
* Centering grip at different points
gen grip6 = grip - 6
gen grip9 = grip - 9
gen grip12 = grip - 12
* Re-coding sex so women are reference
gen sexwm = sexmw
recode sexwm (1=0) if sexmw==1
recode sexwm (0=1) if sexmw==0
* Creating all possible contrasts for dementia groups
gen demnf=0 
gen demnc=0 
gen demfn=0 
gen demfc=0 
gen demcn=0 
gen demcf=0
* Demgroup = none   
replace demnf=0 if (demgroup==1)
replace demnc=0 if (demgroup==1)
replace demfn=1 if (demgroup==1)
replace demfc=0 if (demgroup==1)
replace demcn=1 if (demgroup==1)
replace demcf=0 if (demgroup==1)
* Demgroup = future 
replace demnf=1 if (demgroup==2)
replace demnc=0 if (demgroup==2)
replace demfn=0 if (demgroup==2)
replace demfc=0 if (demgroup==2)
replace demcn=0 if (demgroup==2)
replace demcf=1 if (demgroup==2)
* Demgroup = current
replace demnf=0 if (demgroup==3)
replace demnc=1 if (demgroup==3)
replace demfn=0 if (demgroup==3)
replace demfc=1 if (demgroup==3)
replace demcn=0 if (demgroup==3)
replace demcf=0 if (demgroup==3)  
* Add value labels to demgroup
label define fdemgoup 1 "1None" 2 "2Future" 3 "3Current"
label values demgroup fdemgroup
* Labeling new variables
label variable age80  "age80: Age in Years (0=80)"
label variable age85  "age85: Age in Years (0=85)"
label variable age90  "age85: Age in Years (0=90)"
label variable grip6  "grip6: Grip Strength in Pounds (0=6)"
label variable grip9  "grip9: Grip Strength in Pounds (0=9)"
label variable grip12 "grip12: Grip Strength in Pounds (0=12)"
label variable sexwm  "sexwm: Sex (0=Women, 1=Men)"
label variable demnf  "demnf: Dementia Contrast for None=0 vs Future=1"
label variable demnc  "demnc: Dementia Contrast for None=0 vs Current=1"
label variable demfn  "demfn: Dementia Contrast for Future=0 vs None=1"
label variable demfc  "demfc: Dementia Contrast for Future=0 vs Current=1"
label variable demcn  "demcn: Dementia Contrast for Current=0 vs None=1"
label variable demcf  "demcf: Dementia Contrast for Current=0 vs Future=1"
    
********************************************************************************
*******                       BEGIN CHAPTER 2 MODELS                     *******
********************************************************************************

* Save results to separate file
log using $filesave\STATA_Chapter2_Output, replace name(STATA_Chapter2)

display as result "Chapter 2: Descriptive Statistics for Example Variables"
summarize age grip cognition
tabulate sexmw demgroup
corr age grip sexmw cognition 

display as result "Eq 2.3: Empty Means Model"
mixed cognition , ///
                  || personid: , noconstant variance reml covariance(unstructured),
      estat ic, n(550),

display as result "Eq 2.4: Adding Age (0=85)"
mixed cognition c.age85, ///
                  || personid: , noconstant variance reml covariance(unstructured),
      estat ic, n(550),

display as result "Eq 2.4: Adding Original Age Instead"
mixed cognition c.age, ///
                  || personid: , noconstant variance reml covariance(unstructured),
      estat ic, n(550),

display as result "Eq 2.6: Adding Grip (0=9)"
mixed cognition c.age85 c.grip9, ///
                  || personid: , noconstant variance reml covariance(unstructured),
      estat ic, n(550),
      * Model R2 Test
      test (c.age85=0) (c.grip9=0)

display as result "Eq 2.7: Adding Sex (0=M, 1=W)"
mixed cognition c.age85 c.grip9 c.sexmw, ///
                  || personid: , noconstant variance reml covariance(unstructured),
      estat ic, n(550),
      * Model R2 Test
      test (c.age85=0) (c.grip9=0) (c.sexmw=0)

display as result "Eq 2.7: Adding Sex (1=M 0=W)"
mixed cognition c.age85 c.grip9 c.sexwm, ///
                  || personid: , noconstant variance reml covariance(unstructured),
      estat ic, n(550),

display as result "Eq 2.8: Adding Dementia Group"
display as result "Using Manual Group Contrasts so Reference=None"
mixed cognition c.age85 c.grip9 c.sexmw c.demnf c.demnc, ///
                  || personid: , noconstant variance reml covariance(unstructured),
      estat ic, n(550),
      * Model R2 Test
      test (c.age85=0) (c.grip9=0) (c.sexmw=0) (c.demnf=0) (c.demnc=0)
      * Omnibus Dementia Group Test
      test (c.demnf=0) (c.demnc=0)
      lincom c.demnf*-1 + c.demnc*1 // Future vs Current

display as result "Eq 2.8: Adding Dementia Group"
display as result "Categorical Predictor for Dementia Group"
mixed cognition c.age85 c.grip9 c.sexmw i.demgroup, ///
                  || personid: , noconstant variance reml covariance(unstructured),
      estat ic, n(550),
      contrast i.demgroup, 
      margins  i.demgroup, at(c.age85=0 c.grip9=0 c.sexmw=0)
      margins  i.demgroup, pwcompare(pveffects)

display as result "Eq 2.9: Adding Age by Grip Interaction"
display as result "Age (0=85), Grip (0=9)"
mixed cognition c.age85 c.grip9 c.sexmw c.demnf c.demnc ///
                c.age85#c.grip9, ///
                  || personid: , noconstant variance reml covariance(unstructured),
      estat ic, n(550),
      estat vce,
      * Model R2 Test
      test (c.age85=0) (c.grip9=0) (c.sexmw=0) (c.demnf=0) (c.demnc=0) (c.age85#c.grip9=0)
      * Future vs Current
      lincom c.demnf*-1 + c.demnc*1
      * Age Slope at Grip Strength =  6
      lincom c.age85*1 + c.age85#c.grip9*-3
      * Age Slope at Grip Strength =  9
      lincom c.age85*1 + c.age85#c.grip9*0
      * Age Slope at Grip Strength = 12
      lincom c.age85*1 + c.age85#c.grip9*3
      * Grip Strength Slope at Age = 80
      lincom c.grip9*1 + c.age85#c.grip9*-5
      * Grip Strength Slope at Age = 85
      lincom c.grip9*1 + c.age85#c.grip9*0
      * Grip Strength Slope at Age = 90
      lincom c.grip9*1 + c.age85#c.grip9*5
      * Cognition at Grip = 12 Age = 80
      lincom _cons*1 + c.age85*-5 + c.grip9*3  + c.age85#c.grip9*-15
      * Cognition at Grip = 12 Age = 85
      lincom _cons*1 + c.age85*0  + c.grip9*3  + c.age85#c.grip9*0
      * Cognition at Grip = 12 Age = 90
      lincom _cons*1 + c.age85*5  + c.grip9*3  + c.age85#c.grip9*15
      * Cognition at Grip =  9 Age = 80
      lincom _cons*1 + c.age85*-5 + c.grip9*0  + c.age85#c.grip9*0
      * Cognition at Grip =  9 Age = 85
      lincom _cons*1 + c.age85*0  + c.grip9*0  + c.age85#c.grip9*0
      * Cognition at Grip =  9 Age = 90
      lincom _cons*1 + c.age85*5  + c.grip9*0  + c.age85#c.grip9*0
      * Cognition at Grip =  6 Age = 80
      lincom _cons*1 + c.age85*-5 + c.grip9*-3 + c.age85#c.grip9*15
      * Cognition at Grip =  6 Age = 85
      lincom _cons*1 + c.age85*0  + c.grip9*-3 + c.age85#c.grip9*0
      * Cognition at Grip =  6 Age = 90
      lincom _cons*1 + c.age85*5  + c.grip9*-3 + c.age85#c.grip9*-15
      margins, at (c.age85=(-5(5)5) c.grip9=(-3(3)3) c.sexmw=0 c.demnf=0 c.demnc=0) vsquish,

display as result "Eq 2.9: Adding Age by Grip Interaction"
display as result "Age (0=80), Grip (0=12)"
mixed cognition c.age80 c.grip12 c.sexmw c.demnf c.demnc ///
                c.age80#c.grip12, ///
                  || personid: , noconstant variance reml covariance(unstructured),
      estat ic, n(550),
      lincom c.demnf*-1 + c.demnc*1 // Future vs Current

display as result "Eq 2.9: Adding Age by Grip Interaction"
display as result "Age (0=90), Grip (0=6)"
mixed cognition c.age90 c.grip6 c.sexmw c.demnf c.demnc ///
                c.age90#c.grip6, ///
                  || personid: , noconstant variance reml covariance(unstructured),
      estat ic, n(550),
      lincom c.demnf*-1 + c.demnc*1 // Future vs Current

display as result "Eq 2.13: Adding Sex by Dementia Interaction"
display as result "Sex (0=Men), Dementia (0=None)"
mixed cognition c.age85 c.grip9 c.sexmw c.demnf c.demnc ///
                c.age85#c.grip9 c.sexmw#c.demnf c.sexmw#c.demnc, ///
                  || personid: , noconstant variance reml covariance(unstructured),
      estat ic, n(550),
      estat vce,
      * Model R2 Test
      test (c.age85=0) (c.grip9=0) (c.sexmw=0) (c.demnf=0) (c.demnc=0) (c.age85#c.grip9=0) (c.sexmw#c.demnf=0) (c.sexmw#c.demnc=0)
      * Omnibus Dementia*Sex Interaction Test
      test (c.sexmw#c.demnf=0) (c.sexmw#c.demnc=0)
      * Cognition for Men   None
      lincom _cons*1 + c.sexmw*0 + c.demnf*0 + c.demnc*0 + c.sexmw#c.demnf*0 + c.sexmw#c.demnc*0
      * Cognition for Women None
      lincom _cons*1 + c.sexmw*1 + c.demnf*0 + c.demnc*0 + c.sexmw#c.demnf*0 + c.sexmw#c.demnc*0
      * Cognition for Men   Future
      lincom _cons*1 + c.sexmw*0 + c.demnf*1 + c.demnc*0 + c.sexmw#c.demnf*0 + c.sexmw#c.demnc*0
      * Cognition for Women Future
      lincom _cons*1 + c.sexmw*1 + c.demnf*1 + c.demnc*0 + c.sexmw#c.demnf*1 + c.sexmw#c.demnc*0
      * Cognition for Men   Current
      lincom _cons*1 + c.sexmw*0 + c.demnf*0 + c.demnc*1 + c.sexmw#c.demnf*0 + c.sexmw#c.demnc*0
      * Cognition for Women Current
      lincom _cons*1 + c.sexmw*1 + c.demnf*0 + c.demnc*1 + c.sexmw#c.demnf*0 + c.sexmw#c.demnc*1
      * Sex Difference for No Dementia
      lincom c.sexmw*1 + c.sexmw#c.demnf*0 + c.sexmw#demnc*0
      * Sex Difference for Future Dementia
      lincom c.sexmw*1 + c.sexmw#c.demnf*1 + c.sexmw#demnc*0
      * Sex Difference for Current Dementia
      lincom c.sexmw*1 + c.sexmw#c.demnf*0 + c.sexmw#demnc*1
      * None-Future Difference for Men
      lincom c.demnf*1 + c.sexmw#c.demnf*0
      * None-Future Difference for Women
      lincom c.demnf*1 + c.sexmw#c.demnf*1
      * None-Current Difference for Men
      lincom c.demnc*1 + c.sexmw#c.demnc*0
      * None-Current Difference for Women
      lincom c.demnc*1 + c.sexmw#c.demnc*1
      * Future-Current Difference for Men
      lincom c.demnf*-1 + c.demnc*1 + c.sexmw#c.demnf*0  + c.sexmw#c.demnc*0
      * Future-Current Difference for Women
      lincom c.demnf*-1 + c.demnc*1 + c.sexmw#c.demnf*-1 + c.sexmw#c.demnc*1
      * None-Future Sex Difference
      lincom c.sexmw#c.demnf*1
      * None-Current Sex Difference
      lincom c.sexmw#c.demnc*1
      * Future-Current Sex Difference
      lincom c.sexmw#c.demnf*-1 + c.sexmw#c.demnc*1

display as result "Eq 2.13: Adding Sex by Dementia Interaction"
display as result "Sex (0=Women), Dementia (0=None)"
mixed cognition c.age85 c.grip9 c.sexwm c.demnf c.demnc ///
                c.age85#c.grip9 sexwm#c.demnf c.sexwm#c.demnc, ///
                  || personid: , noconstant variance reml covariance(unstructured),
      estat ic, n(550),

display as result "Eq 2.15: Adding Sex by Dementia Interaction"
display as result "Sex (0=Men), Dementia (0=Future)"
mixed cognition c.age85 c.grip9 c.sexmw c.demfn c.demfc ///
                c.age85#c.grip9 c.sexmw#c.demfn c.sexmw#c.demfc, ///
                  || personid: , noconstant variance reml covariance(unstructured),
      estat ic, n(550),

display as result "Eq 2.15: Adding Sex by Dementia Interaction"
display as result "Sex (0=Women), Dementia (0=Future)"
mixed cognition c.age85 c.grip9 c.sexwm c.demfn c.demfc ///
                c.age85#c.grip9 c.sexwm#c.demfn c.sexwm#c.demfc, ///
                  || personid: , noconstant variance reml covariance(unstructured),
      estat ic, n(550),

display as result "Eq 2.16: Adding Sex by Dementia Interaction"
display as result "Sex (0=Men), Dementia (0=Current)"
mixed cognition c.age85 c.grip9 c.sexmw c.demcn c.demcf ///
                c.age85#c.grip9 c.sexmw#c.demcn c.sexmw#c.demcf, ///
                  || personid: , noconstant variance reml covariance(unstructured),
      estat ic, n(550),

display as result "Eq 2.16: Adding Sex by Dementia Interaction"
display as result "Sex (0=Women), Dementia (0=Current)"
mixed cognition c.age85 c.grip9 c.sexwm c.demcn c.demcf ///
                c.age85#c.grip9 c.sexwm#c.demcn c.sexwm#c.demcf, ///
                  || personid: , noconstant variance reml covariance(unstructured),
      estat ic, n(550),

display as result "Eq 2.16: Adding Sex by Dementia Interaction"
display as result "Categorical Sex and Dementia"
mixed cognition c.age85 c.grip9 i.sexmw i.demgroup ///
                c.age85#c.grip9 i.sexmw#i.demgroup, ///
                  || personid: , noconstant variance reml covariance(unstructured),
      estat ic, n(550),
      contrast i.sexmw#i.demgroup, 
      margins  i.sexmw#i.demgroup, at(c.age85=0 c.grip9=0)
      margins  i.sexmw#i.demgroup, pwcompare(pveffects)
      margins  i.sexmw@i.demgroup, at(c.age85=0 c.grip9=0)
      margins  i.demgroup@i.sexmw, at(c.age85=0 c.grip9=0)
      * Sex by None-Future    Interaction
      contrast {i.sexmw#i.demgroup -1  1  0  1 -1  0}
      * Sex by None-Current   Interaction
      contrast {i.sexmw#i.demgroup -1  0  1  1  0 -1}
      * Sex by Future-Current Interaction
      contrast {i.sexmw#i.demgroup  0 -1  1  0  1 -1}

display as result "Eq 2.17: Adding Sex by Age and Sex by Grip Interactions"
display as result "Age (0=85), Grip (0=9), Sex (0=Men), Dementia (0=None)"
mixed cognition c.age85 c.grip9 c.sexmw c.demnf c.demnc ///
                c.age85#c.grip9 c.sexmw#c.demnf c.sexmw#c.demnc ///
                c.age85#c.sexmw c.grip9#c.sexmw, /// 
                  || personid: , noconstant variance reml covariance(unstructured),
      estat ic, n(550),
      * Age for Men
      lincom c.age85*1 + c.age85#c.sexmw*0
      * Age for Women
      lincom c.age85*1 + c.age85#c.sexmw*1
      * Grip for Men
      lincom c.grip9*1 + c.grip9#c.sexmw*0
      * Grip for Women
      lincom c.grip9*1 + c.grip9#c.sexmw*1
      * Sex for None
      lincom c.sexmw*1 + c.sexmw#c.demnf*0 + c.sexmw#c.demnc*0
      * Sex for Future
      lincom c.sexmw*1 + c.sexmw#c.demnf*1 + c.sexmw#c.demnc*0
      * Sex for Current
      lincom c.sexmw*1 + c.sexmw#c.demnf*0 + c.sexmw#c.demnc*1
      * Men: None vs Future
      lincom c.demnf*1  + c.demnc*0 + c.sexmw#c.demnf*0  + c.sexmw#c.demnc*0
      * Men: None vs Current
      lincom c.demnf*0  + c.demnc*1 + c.sexmw#c.demnf*0  + c.sexmw#c.demnc*0
      * Men: Future vs Current
      lincom c.demnf*-1 + c.demnc*1 + c.sexmw#c.demnf*0  + c.sexmw#c.demnc*0
      * Women: None vs Future
      lincom c.demnf*1  + c.demnc*0 + c.sexmw#c.demnf*1  + c.sexmw#c.demnc*0
      * Women: None vs Current
      lincom c.demnf*0  + c.demnc*1 + c.sexmw#c.demnf*0  + c.sexmw#c.demnc*1
      * Women: Future vs Current
      lincom c.demnf*-1 + c.demnc*1 + c.sexmw#c.demnf*-1 + c.sexmw#c.demnc*1
      * Sex by None vs Future
      lincom c.demnf#c.sexmw*1  + c.demnc#c.sexmw*0
      * Sex by None vs Current
      lincom c.demnf#c.sexmw*0  + c.demnc#c.sexmw*1
      * Sex by Future vs Current
      lincom c.demnf#c.sexmw*-1 + c.demnc#c.sexmw*1

display as result "Eq 2.18: Adding Sex by Age by Grip Three-Way Interaction"
display as result "Age (0=85), Grip (0=9), Sex (0=Men), Dementia (0=None)"
mixed cognition c.age85 c.grip9 c.sexmw c.demnf c.demnc ///
                c.age85#c.grip9 c.sexmw#c.demnf c.sexmw#c.demnc ///
                c.age85#c.sexmw c.grip9#c.sexmw c.age85#c.grip9#c.sexmw, /// 
                  || personid: , noconstant variance reml covariance(unstructured),
      estat ic, n(550),
      * Age for Men
      lincom c.age85*1  + c.age85#c.sexmw*0
      * Age for Women
      lincom c.age85*1  + c.age85#c.sexmw*1
      * Grip for Men
      lincom c.grip9*1  + c.grip9#c.sexmw*0
      * Grip for Women
      lincom c.grip9*1  + c.grip9#c.sexmw*1
      * Sex for None
      lincom c.sexmw*1  + c.sexmw#c.demnf*0 + c.sexmw#c.demnc*0
      * Sex for Future
      lincom c.sexmw*1  + c.sexmw#c.demnf*1 + c.sexmw#c.demnc*0
      * Sex for Current
      lincom c.sexmw*1  + c.sexmw#c.demnf*0 + c.sexmw#c.demnc*1
      * Men: None vs Future
      lincom c.demnf*1  + c.demnc*0 + c.sexmw#c.demnf*0  + sexmw#c.demnc*0
      * Men: None vs Current
      lincom c.demnf*0  + c.demnc*1 + c.sexmw#c.demnf*0  + sexmw#c.demnc*0
      * Men: Future vs Current
      lincom c.demnf*-1 + c.demnc*1 + c.sexmw#c.demnf*0  + sexmw#c.demnc*0
      * Women: None vs Future
      lincom c.demnf*1  + c.demnc*0 + c.sexmw#c.demnf*1  + sexmw#c.demnc*0
      * Women: None vs Current
      lincom c.demnf*0  + c.demnc*1 + c.sexmw#c.demnf*0  + sexmw#c.demnc*1
      * Women: Future vs Current
      lincom c.demnf*-1 + c.demnc*1 + c.sexmw#c.demnf*-1 + sexmw#c.demnc*1
      * Sex by None vs Future
      lincom c.demnf#c.sexmw*1  + c.demnc#c.sexmw*0
      * Sex by None vs Current
      lincom c.demnf#c.sexmw*0  + c.demnc#c.sexmw*1
      * Sex by Future vs Current
      lincom c.demnf#c.sexmw*-1 + c.demnc#c.sexmw*1
      * Age by Grip for Men
      lincom c.age85#c.grip9*1  + c.age85#c.grip9#c.sexmw*0
      * Age by Grip for Women
      lincom c.age85#c.grip9*1  + c.age85#c.grip9#c.sexmw*1
      margins, at (c.age85=(-5(5)5) c.grip9=(-3(3)3) c.sexmw=(0(1)1) c.demnf=0 c.demnc=0) vsquish,

display as result "Eq 2.13: Final Reported Model"
display as result "Age (0=85), Grip (0=9), Sex (0=Men), Dementia (0=None)"
mixed cognition c.age85 c.grip9 c.sexmw c.demnf c.demnc ///
                c.age85#c.grip9 c.sexmw#c.demnf c.sexmw#c.demnc, ///
                  || personid: , noconstant variance reml covariance(unstructured),
      estat ic, n(550),
      * Model R2 Test
      test (c.age85=0) (c.grip9=0) (c.sexmw=0) (c.demnf=0) (c.demnc=0) (c.age85#c.grip9=0) (c.sexmw#c.demnf=0) (c.sexmw#c.demnc=0)
      * Omnibus Dementia*Sex Interaction Test
      test (c.sexmw#c.demnf=0) (c.sexmw#c.demnc=0)
      * Sex Difference for No Dementia
      lincom c.sexmw*1  + c.sexmw#c.demnf*0 + c.sexmw#c.demnc*0
      * Sex Difference for Future Dementia
      lincom c.sexmw*1  + c.sexmw#c.demnf*1 + c.sexmw#c.demnc*0
      * Sex Difference for Current Dementia
      lincom c.sexmw*1  + c.sexmw#c.demnf*0 + c.sexmw#c.demnc*1
      * None-Future Difference for Men
      lincom c.demnf*1  + c.sexmw#c.demnf*0
      * None-Future Difference for Women
      lincom c.demnf*1  + c.sexmw#c.demnf*1
      * None-Current Difference for Men
      lincom c.demnc*1  + c.sexmw#c.demnc*0
      * None-Current Difference for Women
      lincom c.demnc*1  + c.sexmw#c.demnc*1
      * Future-Current Difference for Men
      lincom c.demnf*-1 + c.demnc*1 + c.sexmw#c.demnf*0  + c.sexmw#c.demnc*0
      * Future-Current Difference for Women
      lincom c.demnf*-1 + c.demnc*1 + c.sexmw#c.demnf*-1 + c.sexmw#c.demnc*1
      * None-Future Sex Difference
      lincom c.sexmw#c.demnf*1
      * None-Current Sex Difference
      lincom c.sexmw#c.demnc*1
      * Future-Current Sex Difference
      lincom c.sexmw#c.demnf*-1 + c.sexmw#c.demnc*1

display as result "Eq 2.13: Final Reported Model"
display as result "Categorical Variables for Sex and Dementia"
mixed cognition c.age85 c.grip9 i.sexmw i.demgroup ///
                c.age85#c.grip9 i.sexmw#i.demgroup, ///
                  || personid: , noconstant variance reml covariance(unstructured),
      estat ic, n(550),
      contrast i.sexmw#i.demgroup, 
      margins  i.sexmw#i.demgroup, at(c.age85=0 c.grip9=0)
      margins  i.sexmw#i.demgroup, pwcompare(pveffects)
      margins  i.sexmw@i.demgroup, at(c.age85=0 c.grip9=0)
      margins  i.demgroup@i.sexmw, at(c.age85=0 c.grip9=0)
 
****** END CHAPTER 2 MODELS ******

* Close log
log close STATA_Chapter2
* Convert log to html using custom downloaded package
log2html $filesave\STATA_Chapter2_Output, replace
