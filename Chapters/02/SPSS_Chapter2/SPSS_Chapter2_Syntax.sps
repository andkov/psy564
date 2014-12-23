* Stop syntax and large titles from printing to output.
SET PRINTBACK=NONE HEADER=NO.

***********************************************************************************.
*******            BEGIN DATA MANIPULATION FOR CHAPTER 2 EXAMPLE            *******.
*******                CHANGE "filesave" to your directory                  *******.
***********************************************************************************.

* Define location of files used in code below.
FILE HANDLE filesave /NAME = "C:\Dropbox\PilesOfVariance\Chapter2\SPSS".

* Import chapter 2 example data and center predictors.
GET FILE = "filesave/SPSS_Chapter2.sav".
DATASET NAME Chapter2 WINDOW=FRONT.

* Centering age at different points.
COMPUTE age80 = age - 80.
COMPUTE age85 = age - 85.
COMPUTE age90 = age - 90.
* Centering grip at different points.
COMPUTE grip6 = grip - 6.
COMPUTE grip9 = grip - 9.
COMPUTE grip12 = grip - 12.

* Re-coding sex so women are reference.
IF (sexMW=0) sexWM=1. 
IF (sexMW=1) sexWM=0.
 
* Creating all possible contrasts for dementia groups.
DO IF (demgroup=1).
COMPUTE demNF=0.
COMPUTE demNC=0.
COMPUTE demFN=1.
COMPUTE demFC=0.
COMPUTE demCN=1.
COMPUTE demCF=0.  
END IF.
DO IF (demgroup=2).
COMPUTE demNF=1.
COMPUTE demNC=0.
COMPUTE demFN=0.
COMPUTE demFC=0.
COMPUTE demCN=0.
COMPUTE demCF=1.  
END IF.
DO IF (demgroup=3).
COMPUTE demNF=0.
COMPUTE demNC=1.
COMPUTE demFN=0.
COMPUTE demFC=1.
COMPUTE demCN=0.
COMPUTE demCF=0.  
END IF.
* Add value labels to demgroup.
VALUE LABELS demgroup 1 "1None" 2 "2Future" 3 "3Current".
* Labeling new variables.
VARIABLE LABELS
age80  "age80: Age in Years (0=80)"
age85  "age85: Age in Years (0=85)"
age90  "age85: Age in Years (0=90)"
grip6  "grip6: Grip Strength in Pounds (0=6)"
grip9  "grip9: Grip Strength in Pounds (0=9)"
grip12 "grip12: Grip Strength in Pounds (0=12)"
sexWM  "sexWM: Sex (0=Women, 1=Men)"
demNF  "demNF: Dementia Contrast for None=0 vs Future=1"
demNC  "demNC: Dementia Contrast for None=0 vs Current=1"
demFN  "demFN: Dementia Contrast for Future=0 vs None=1"
demFC  "demFC: Dementia Contrast for Future=0 vs Current=1"
demCN  "demCN: Dementia Contrast for Current=0 vs None=1"
demCF  "demCF: Dementia Contrast for Current=0 vs Future=1". 
EXECUTE.

* Creating 'fake people' to show age*grip interaction.
* Each row is a fake person for which to create a predicted outcome.
DATA LIST FREE / PersonID grip9 age85 sexMW demNF demNC. 
BEGIN DATA.
-99  3 -5  0  0  0 
-99  3  0  0  0  0 
-99  3  5  0  0  0 
-99  0 -5  0  0  0 
-99  0  0  0  0  0 
-99  0  5  0  0  0 
-99 -3 -5  0  0  0 
-99 -3  0  0  0  0 
-99 -3  5  0  0  0 
END DATA.
DATASET NAME FakeAgeGrip.
* Merge with real data.
ADD FILES FILE=Chapter2 /FILE=FakeAgeGrip.
DATASET NAME PlotAgeGrip.
SORT CASES BY PersonID.
DATASET CLOSE FakeAgeGrip.

* Creating 'fake people' to show age*grip*sex interaction.
* Each row is a fake person for which to create a predicted outcome.
DATA LIST FREE / PersonID grip9 age85 sexMW demNF demNC. 
BEGIN DATA.
-99   3  -5  0  0  0 
-99   3   0  0  0  0 
-99   3   5  0  0  0 
-99   0  -5  0  0  0 
-99   0   0  0  0  0 
-99   0   5  0  0  0 
-99  -3  -5  0  0  0 
-99  -3   0  0  0  0 
-99  -3   5  0  0  0 
-99   3  -5  1  0  0 
-99   3   0  1  0  0 
-99   3   5  1  0  0 
-99   0  -5  1  0  0 
-99   0   0  1  0  0 
-99   0   5  1  0  0 
-99  -3  -5  1  0  0 
-99  -3   0  1  0  0 
-99  -3   5  1  0  0 
END DATA.
DATASET NAME FakeAgeGripSex.
* Merge with real data.
ADD FILES FILE=Chapter2 /FILE=FakeAgeGripSex.
DATASET NAME PlotAgeGripSex.
SORT CASES BY PersonID.
DATASET CLOSE FakeAgeGripSex.

***********************************************************************************.
*******                       BEGIN CHAPTER 2 MODELS                        *******.
***********************************************************************************.

* Open output directory.
OUTPUT NAME SPSS_Chapter2_Output.

ECHO "Chapter 2: Descriptive Statistics for Example Variables".
DESCRIPTIVES VARIABLES= age grip cognition.
CROSSTABS TABLES= sexMW BY demgroup.
CORRELATIONS VARIABLES= age grip sexMW cognition.

DATASET ACTIVATE Chapter2 WINDOW=FRONT.
ECHO 'Eq 2.3: Empty Means Model'.
MIXED cognition
     /METHOD   = REML
     /PRINT    = SOLUTION TESTCOV 
     /FIXED    =  
.
 
DATASET ACTIVATE Chapter2 WINDOW=FRONT.
ECHO 'Eq 2.4: Adding Age (0=85)'.
MIXED cognition WITH age85
     /METHOD   = REML
     /PRINT    = SOLUTION TESTCOV 
     /FIXED    = age85 
.
 
DATASET ACTIVATE Chapter2 WINDOW=FRONT.
ECHO 'Eq 2.4: Adding Original Age Instead'.
MIXED cognition WITH age
     /METHOD   = REML
     /PRINT    = SOLUTION TESTCOV 
     /FIXED    = age 
.
 
DATASET ACTIVATE Chapter2 WINDOW=FRONT.
ECHO 'Eq 2.6: Adding Grip (0=9)'.
MIXED cognition WITH age85 grip9
     /METHOD   = REML
     /PRINT    = SOLUTION TESTCOV 
     /FIXED    = age85 grip9 
     /TEST     = 'Model R2 Test' age85 1; grip9 1
.
 
DATASET ACTIVATE Chapter2 WINDOW=FRONT.
ECHO 'Eq 2.7: Adding Sex (0=M, 1=W)'.
MIXED cognition WITH age85 grip9 sexMW
     /METHOD   = REML
     /PRINT    = SOLUTION TESTCOV 
     /FIXED    = age85 grip9 sexMW 
     /TEST     = 'Model R2 Test' age85 1; grip9 1; sexMW 1
.
 
DATASET ACTIVATE Chapter2 WINDOW=FRONT.
ECHO 'Eq 2.7: Adding Sex (1=M 0=W)'.
MIXED cognition WITH age85 grip9 sexWM
     /METHOD   = REML
     /PRINT    = SOLUTION TESTCOV 
     /FIXED    = age85 grip9 sexWM 
.
 
DATASET ACTIVATE Chapter2 WINDOW=FRONT.
ECHO 'Eq 2.8: Adding Dementia Group;'.
ECHO 'Using Manual Group Contrasts so Reference=None'.
MIXED cognition WITH age85 grip9 sexMW demNF demNC
     /METHOD   = REML
     /PRINT    = SOLUTION TESTCOV 
     /FIXED    = age85 grip9 sexMW demNF demNC 
     /TEST     = 'Model R2 Test' age85 1; grip9 1; sexMW 1; demNF 1; demNC 1
     /TEST     = 'Omnibus Dementia Group Test' demNF 1; demNC 1
     /TEST     = 'Future vs Current' demNF -1 demNC 1
.
 
DATASET ACTIVATE Chapter2 WINDOW=FRONT.
ECHO 'Eq 2.8: Adding Dementia Group;'.
ECHO 'Categorical Predictor for Dementia Group'.
MIXED cognition BY demgroup WITH age85 grip9 sexMW
     /METHOD   = REML
     /PRINT    = SOLUTION TESTCOV 
     /FIXED    = age85 grip9 sexMW demgroup 
     /EMMEANS  = TABLES(demgroup) COMPARE(demgroup) WITH(age85=0 grip9=0 sexMW=0)
.
 
* Estimate model on data with fake people to make predictions.
DATASET ACTIVATE PlotAgeGrip WINDOW=FRONT.
ECHO 'Eq 2.9: Adding Age by Grip Interaction;'.
ECHO 'Age (0=85), Grip (0=9)'.
MIXED cognition WITH age85 grip9 sexMW demNF demNC
     /METHOD   = REML
     /PRINT    = SOLUTION TESTCOV COVB
     /FIXED    = age85 grip9 sexMW demNF demNC
                 age85*grip9 
     /TEST     = 'Model R2 Test' age85 1; grip9 1; sexMW 1; demNF 1; demNC 1; age85*grip9 1
     /TEST     = 'Future vs Current'                  demNF -1 demNC 1
     /TEST     = 'Age Slope at Grip Strength =  6'    age85 1 age85*grip9 -3
     /TEST     = 'Age Slope at Grip Strength =  9'    age85 1 age85*grip9 0
     /TEST     = 'Age Slope at Grip Strength = 12'    age85 1 age85*grip9 3
     /TEST     = 'Grip Strength Slope at Age = 80'    grip9 1 age85*grip9 -5
     /TEST     = 'Grip Strength Slope at Age = 85'    grip9 1 age85*grip9 0
     /TEST     = 'Grip Strength Slope at Age = 90'    grip9 1 age85*grip9 5
     /TEST     = 'Cognition at Grip = 12 Age = 80'    intercept 1 age85 -5 grip9 3  age85*grip9 -15
     /TEST     = 'Cognition at Grip = 12 Age = 85'    intercept 1 age85 0  grip9 3  age85*grip9 0
     /TEST     = 'Cognition at Grip = 12 Age = 90'    intercept 1 age85 5  grip9 3  age85*grip9 15
     /TEST     = 'Cognition at Grip =  9 Age = 80'    intercept 1 age85 -5 grip9 0  age85*grip9 0
     /TEST     = 'Cognition at Grip =  9 Age = 85'    intercept 1 age85 0  grip9 0  age85*grip9 0
     /TEST     = 'Cognition at Grip =  9 Age = 90'    intercept 1 age85 5  grip9 0  age85*grip9 0
     /TEST     = 'Cognition at Grip =  6 Age = 80'    intercept 1 age85 -5 grip9 -3 age85*grip9 15
     /TEST     = 'Cognition at Grip =  6 Age = 85'    intercept 1 age85 0  grip9 -3 age85*grip9 0
     /TEST     = 'Cognition at Grip =  6 Age = 90'    intercept 1 age85 5  grip9 -3 age85*grip9 -15
     /SAVE     = FIXPRED(PredAgeGrip)
.
 
DATASET ACTIVATE Chapter2 WINDOW=FRONT.
ECHO 'Eq 2.9: Adding Age by Grip Interaction;'.
ECHO 'Age (0=80), Grip (0=12)'.
MIXED cognition WITH age80 grip12 sexMW demNF demNC
     /METHOD   = REML
     /PRINT    = SOLUTION TESTCOV 
     /FIXED    = age80 grip12 sexMW demNF demNC
                 age80*grip12 
     /TEST     = 'Future vs Current' demNF -1 demNC 1
.
 
DATASET ACTIVATE Chapter2 WINDOW=FRONT.
ECHO 'Eq 2.9: Adding Age by Grip Interaction;'.
ECHO 'Age (0=90), Grip (0=6)'.
MIXED cognition WITH age90 grip6 sexMW demNF demNC
     /METHOD   = REML
     /PRINT    = SOLUTION TESTCOV 
     /FIXED    = age90 grip6 sexMW demNF demNC
                 age90*grip6 
     /TEST     = 'Future vs Current' demNF -1 demNC 1
.
 
DATASET ACTIVATE Chapter2 WINDOW=FRONT.
ECHO 'Eq 2.13: Adding Sex by Dementia Interaction;'.
ECHO 'Sex (0=Men), Dementia (0=None)'.
MIXED cognition WITH age85 grip9 sexMW demNF demNC
     /METHOD   = REML
     /PRINT    = SOLUTION TESTCOV COVB
     /FIXED    = age85 grip9 sexMW demNF demNC
                 age85*grip9 sexMW*demNF sexMW*demNC 
     /TEST     = 'Model R2 Test' age85 1; grip9 1; sexMW 1; demNF 1; demNC 1; age85*grip9 1; sexMW*demNF 1; sexMW*demNC 1
     /TEST     = 'Omnibus Dementia*Sex Interaction Test' sexMW*demNF 1; sexMW*demNC 1
     /TEST     = 'Cognition for Men   None'                intercept 1 sexMW 0 demNF 0 demNC 0 sexMW*demNF 0 sexMW*demNC 0
     /TEST     = 'Cognition for Women None'                intercept 1 sexMW 1 demNF 0 demNC 0 sexMW*demNF 0 sexMW*demNC 0
     /TEST     = 'Cognition for Men   Future'              intercept 1 sexMW 0 demNF 1 demNC 0 sexMW*demNF 0 sexMW*demNC 0
     /TEST     = 'Cognition for Women Future'              intercept 1 sexMW 1 demNF 1 demNC 0 sexMW*demNF 1 sexMW*demNC 0
     /TEST     = 'Cognition for Men   Current'             intercept 1 sexMW 0 demNF 0 demNC 1 sexMW*demNF 0 sexMW*demNC 0
     /TEST     = 'Cognition for Women Current'             intercept 1 sexMW 1 demNF 0 demNC 1 sexMW*demNF 0 sexMW*demNC 1
     /TEST     = 'Sex Difference for No Dementia'          sexMW 1 sexMW*demNF 0 sexMW*demNC 0
     /TEST     = 'Sex Difference for Future Dementia'      sexMW 1 sexMW*demNF 1 sexMW*demNC 0
     /TEST     = 'Sex Difference for Current Dementia'     sexMW 1 sexMW*demNF 0 sexMW*demNC 1
     /TEST     = 'None-Future Difference for Men'          demNF 1 sexMW*demNF 0
     /TEST     = 'None-Future Difference for Women'        demNF 1 sexMW*demNF 1
     /TEST     = 'None-Current Difference for Men'         demNC 1 sexMW*demNC 0
     /TEST     = 'None-Current Difference for Women'       demNC 1 sexMW*demNC 1
     /TEST     = 'Future-Current Difference for Men'       demNF -1 demNC 1 sexMW*demNF 0  sexMW*demNC 0
     /TEST     = 'Future-Current Difference for Women'     demNF -1 demNC 1 sexMW*demNF -1 sexMW*demNC 1
     /TEST     = 'None-Future Sex Difference'              sexMW*demNF 1
     /TEST     = 'None-Current Sex Difference'             sexMW*demNC 1
     /TEST     = 'Future-Current Sex Difference'           sexMW*demNF -1 sexMW*demNC 1
.
 
DATASET ACTIVATE Chapter2 WINDOW=FRONT.
ECHO 'Eq 2.13: Adding Sex by Dementia Interaction;'.
ECHO 'Sex (0=Women), Dementia (0=None)'.
MIXED cognition WITH age85 grip9 sexwm demNF demNC
     /METHOD   = REML
     /PRINT    = SOLUTION TESTCOV 
     /FIXED    = age85 grip9 sexwm demNF demNC
                 age85*grip9 sexwm*demNF sexwm*demNC 
.
 
DATASET ACTIVATE Chapter2 WINDOW=FRONT.
ECHO 'Eq 2.15: Adding Sex by Dementia Interaction;'.
ECHO 'Sex (0=Men), Dementia (0=Future)'.
MIXED cognition WITH age85 grip9 sexMW demFN demFC
     /METHOD   = REML
     /PRINT    = SOLUTION TESTCOV 
     /FIXED    = age85 grip9 sexMW demFN demFC
                 age85*grip9 sexMW*demFN sexMW*demFC 
.
 
DATASET ACTIVATE Chapter2 WINDOW=FRONT.
ECHO 'Eq 2.15: Adding Sex by Dementia Interaction;'.
ECHO 'Sex (0=Women), Dementia (0=Future)'.
MIXED cognition WITH age85 grip9 sexwm demFN demFC
     /METHOD   = REML
     /PRINT    = SOLUTION TESTCOV 
     /FIXED    = age85 grip9 sexwm demFN demFC
                 age85*grip9 sexwm*demFN sexwm*demFC 
.
 
DATASET ACTIVATE Chapter2 WINDOW=FRONT.
ECHO 'Eq 2.16: Adding Sex by Dementia Interaction;'.
ECHO 'Sex (0=Men), Dementia (0=Current)'.
MIXED cognition WITH age85 grip9 sexMW demCN demCF
     /METHOD   = REML
     /PRINT    = SOLUTION TESTCOV 
     /FIXED    = age85 grip9 sexMW demCN demCF
                 age85*grip9 sexMW*demCN sexMW*demCF 
.
 
DATASET ACTIVATE Chapter2 WINDOW=FRONT.
ECHO 'Eq 2.16: Adding Sex by Dementia Interaction;'.
ECHO 'Sex (0=Women), Dementia (0=Current)'.
MIXED cognition WITH age85 grip9 sexwm demCN demCF
     /METHOD   = REML
     /PRINT    = SOLUTION TESTCOV 
     /FIXED    = age85 grip9 sexwm demCN demCF
                 age85*grip9 sexwm*demCN sexwm*demCF 
.
 
DATASET ACTIVATE Chapter2 WINDOW=FRONT.
ECHO 'Eq 2.16: Adding Sex by Dementia Interaction;'.
ECHO 'Categorical Sex and Dementia'.
MIXED cognition BY sexMW demgroup WITH age85 grip9
     /METHOD   = REML
     /PRINT    = SOLUTION TESTCOV 
     /FIXED    = age85 grip9 sexMW demgroup
                 age85*grip9 sexMW*demgroup 
     /EMMEANS  = TABLES(sexMW*demgroup) COMPARE(demgroup) WITH(age85=0 grip9=0)
     /EMMEANS  = TABLES(sexMW*demgroup) COMPARE(sexMW)    WITH(age85=0 grip9=0)
     /TEST     = 'Sex by None-Future    Interaction'  sexMW*demgroup -1  1  0  1 -1  0
     /TEST     = 'Sex by None-Current   Interaction'  sexMW*demgroup -1  0  1  1  0 -1
     /TEST     = 'Sex by Future-Current Interaction'  sexMW*demgroup  0 -1  1  0  1 -1
.
 
DATASET ACTIVATE Chapter2 WINDOW=FRONT.
ECHO 'Eq 2.17: Adding Sex by Age and Sex by Grip Interactions;'.
ECHO 'Age (0=85), Grip (0=9), Sex (0=Men), Dementia (0=None)'.
MIXED cognition WITH age85 grip9 sexMW demNF demNC
     /METHOD   = REML
     /PRINT    = SOLUTION TESTCOV 
     /FIXED    = age85 grip9 sexMW demNF demNC
                 age85*grip9 sexMW*demNF sexMW*demNC
                 age85*sexMW grip9*sexMW 
     /TEST     = 'Age for Men'              age85 1 age85*sexMW 0
     /TEST     = 'Age for Women'            age85 1 age85*sexMW 1
     /TEST     = 'Grip for Men'             grip9 1 grip9*sexMW 0
     /TEST     = 'Grip for Women'           grip9 1 grip9*sexMW 1
     /TEST     = 'Sex for None'             sexMW 1 sexMW*demNF 0 sexMW*demNC 0
     /TEST     = 'Sex for Future'           sexMW 1 sexMW*demNF 1 sexMW*demNC 0
     /TEST     = 'Sex for Current'          sexMW 1 sexMW*demNF 0 sexMW*demNC 1
     /TEST     = 'Men: None vs Future'      demNF 1  demNC 0 sexMW*demNF 0  sexMW*demNC 0
     /TEST     = 'Men: None vs Current'     demNF 0  demNC 1 sexMW*demNF 0  sexMW*demNC 0
     /TEST     = 'Men: Future vs Current'   demNF -1 demNC 1 sexMW*demNF 0  sexMW*demNC 0
     /TEST     = 'Women: None vs Future'    demNF 1  demNC 0 sexMW*demNF 1  sexMW*demNC 0
     /TEST     = 'Women: None vs Current'   demNF 0  demNC 1 sexMW*demNF 0  sexMW*demNC 1
     /TEST     = 'Women: Future vs Current' demNF -1 demNC 1 sexMW*demNF -1 sexMW*demNC 1
     /TEST     = 'Sex by None vs Future'    demNF*sexMW 1  demNC*sexMW 0
     /TEST     = 'Sex by None vs Current'   demNF*sexMW 0  demNC*sexMW 1
     /TEST     = 'Sex by Future vs Current' demNF*sexMW -1 demNC*sexMW 1
.
 
* Estimate model on data with fake people to make predictions.
DATASET ACTIVATE PlotAgeGripSex WINDOW=FRONT.
ECHO 'Eq 2.18: Adding Sex by Age by Grip Three-Way Interaction;'.
ECHO 'Age (0=85), Grip (0=9), Sex (0=Men), Dementia (0=None)'.
MIXED cognition WITH age85 grip9 sexMW demNF demNC
     /METHOD   = REML
     /PRINT    = SOLUTION TESTCOV 
     /FIXED    = age85 grip9 sexMW demNF demNC
                 age85*grip9 sexMW*demNF sexMW*demNC
                 age85*sexMW grip9*sexMW age85*grip9*sexMW 
     /TEST     = 'Age for Men'              age85 1  age85*sexMW 0
     /TEST     = 'Age for Women'            age85 1  age85*sexMW 1
     /TEST     = 'Grip for Men'             grip9 1  grip9*sexMW 0
     /TEST     = 'Grip for Women'           grip9 1  grip9*sexMW 1
     /TEST     = 'Sex for None'             sexMW 1  sexMW*demNF 0 sexMW*demNC 0
     /TEST     = 'Sex for Future'           sexMW 1  sexMW*demNF 1 sexMW*demNC 0
     /TEST     = 'Sex for Current'          sexMW 1  sexMW*demNF 0 sexMW*demNC 1
     /TEST     = 'Men: None vs Future'      demNF 1  demNC 0 sexMW*demNF 0  sexMW*demNC 0
     /TEST     = 'Men: None vs Current'     demNF 0  demNC 1 sexMW*demNF 0  sexMW*demNC 0
     /TEST     = 'Men: Future vs Current'   demNF -1 demNC 1 sexMW*demNF 0  sexMW*demNC 0
     /TEST     = 'Women: None vs Future'    demNF 1  demNC 0 sexMW*demNF 1  sexMW*demNC 0
     /TEST     = 'Women: None vs Current'   demNF 0  demNC 1 sexMW*demNF 0  sexMW*demNC 1
     /TEST     = 'Women: Future vs Current' demNF -1 demNC 1 sexMW*demNF -1 sexMW*demNC 1
     /TEST     = 'Sex by None vs Future'    demNF*sexMW 1  demNC*sexMW 0
     /TEST     = 'Sex by None vs Current'   demNF*sexMW 0  demNC*sexMW 1
     /TEST     = 'Sex by Future vs Current' demNF*sexMW -1 demNC*sexMW 1
     /TEST     = 'Age by Grip for Men'      age85*grip9 1  age85*grip9*sexMW 0
     /TEST     = 'Age by Grip for Women'    age85*grip9 1  age85*grip9*sexMW 1
     /SAVE     = FIXPRED(Pred3AgeSexGrip)
.
 
DATASET ACTIVATE Chapter2 WINDOW=FRONT.
ECHO 'Eq 2.13: Final Reported Model;'.
ECHO 'Age (0=85), Grip (0=9), Sex (0=Men), Dementia (0=None)'.
MIXED cognition WITH age85 grip9 sexMW demNF demNC
     /METHOD   = REML
     /PRINT    = SOLUTION TESTCOV 
     /FIXED    = age85 grip9 sexMW demNF demNC
                 age85*grip9 sexMW*demNF sexMW*demNC 
     /TEST     = 'Model R2 Test' age85 1; grip9 1; sexMW 1; demNF 1; demNC 1; age85*grip9 1; sexMW*demNF 1; sexMW*demNC 1
     /TEST     = 'Omnibus Dementia*Sex Interaction Test' sexMW*demNF 1; sexMW*demNC 1
     /TEST     = 'Sex Difference for No Dementia'          sexMW 1  sexMW*demNF 0 sexMW*demNC 0
     /TEST     = 'Sex Difference for Future Dementia'      sexMW 1  sexMW*demNF 1 sexMW*demNC 0
     /TEST     = 'Sex Difference for Current Dementia'     sexMW 1  sexMW*demNF 0 sexMW*demNC 1
     /TEST     = 'None-Future Difference for Men'          demNF 1  sexMW*demNF 0
     /TEST     = 'None-Future Difference for Women'        demNF 1  sexMW*demNF 1
     /TEST     = 'None-Current Difference for Men'         demNC 1  sexMW*demNC 0
     /TEST     = 'None-Current Difference for Women'       demNC 1  sexMW*demNC 1
     /TEST     = 'Future-Current Difference for Men'       demNF -1 demNC 1 sexMW*demNF 0  sexMW*demNC 0
     /TEST     = 'Future-Current Difference for Women'     demNF -1 demNC 1 sexMW*demNF -1 sexMW*demNC 1
     /TEST     = 'None-Future Sex Difference'              sexMW*demNF 1
     /TEST     = 'None-Current Sex Difference'             sexMW*demNC 1
     /TEST     = 'Future-Current Sex Difference'           sexMW*demNF -1 sexMW*demNC 1
.
 
DATASET ACTIVATE Chapter2 WINDOW=FRONT.
ECHO 'Eq 2.13: Final Reported Model;'.
ECHO 'Categorical Variables for Sex and Dementia'.
MIXED cognition BY sexMW demgroup WITH age85 grip9
     /METHOD   = REML
     /PRINT    = SOLUTION TESTCOV 
     /FIXED    = age85 grip9 sexMW demgroup
                 age85*grip9 sexMW*demgroup 
     /EMMEANS  = TABLES(sexMW*demgroup) COMPARE(demgroup) WITH(age85=0 grip9=0)
     /EMMEANS  = TABLES(sexMW*demgroup) COMPARE(sexMW)    WITH(age85=0 grip9=0)
.
 
****** END CHAPTER 2 MODELS ******.

* Close output directory.
OUTPUT EXPORT NAME=SPSS_Chapter2_Output
     /CONTENTS EXPORT=VISIBLE LAYERS=VISIBLE MODELVIEWS=VISIBLE
     /HTML DOCUMENTFILE='C:\Dropbox\PilesOfVariance\Chapter2\SPSS\SPSS_Chapter2_Output.html'
           IMAGEFORMAT=PNG STYLING=YES.
