* Output options: all can be turned on/off by adding or removing the NO;
* page number, date, centering, or page breaks, page length and width;
OPTIONS NOnumber NOdate NOcenter FormDlim=' ' PageSize=MAX LineSize=MAX;
* Eliminate SAS default titles and names of tables in output (TRACE ON to show);
TITLE; ODS TRACE OFF; ODS LISTING CLOSE;

***********************************************************************************;
*******   MACRO PROGRAMS TO AUTOMATE CALCULATIONS FOR CHAPTER 10a EXAMPLE   *******;
*******               NOTHING IN HERE NEEDS TO BE CHANGED                   *******;
***********************************************************************************;

* To use FitTest macro;
* FitFewer = name of infocrit table for nested model;
* FitMore  = name of infocrit table for comparison model;
%MACRO FitTest(FitFewer=,FitMore=);
DATA &FitFewer.; LENGTH Name $30.; SET &FitFewer.; Name="&FitFewer."; RUN;
DATA &FitMore.;  LENGTH Name $30.; SET &FitMore.;  Name="&FitMore.";  RUN;
DATA FitCompare; LENGTH Name $30.; SET &FitFewer. &FitMore.; RUN;
DATA FitCompare; SET FitCompare; DevDiff=Lag1(Neg2LogLike)-Neg2LogLike;
     DFdiff=Parms-LAG1(Parms); Pvalue=1-PROBCHI(DevDiff,DFdiff);
     DROP AICC HQIC CAIC; RUN;
TITLE9 "Likelihood Ratio Test for &FitFewer. vs. &FitMore.";
PROC PRINT NOOBS DATA=FitCompare; RUN; TITLE9;
%MEND FitTest;

* To use TotalR2 macro;
* DV =        case-sensitive name of dependent variable;
* PredFewer = name of dataset of predicted outcomes for nested model;
* PredMore =  name of dataset of predicted outcomes for comparison model;
%MACRO TotalR2(DV=,PredFewer=,PredMore=);
PROC CORR NOPRINT NOSIMPLE DATA=&PredFewer. OUTP=CorrFewer; VAR pred &DV.; RUN;
PROC CORR NOPRINT NOSIMPLE DATA=&PredMore.  OUTP=CorrMore;  VAR pred &DV.; RUN;
DATA CorrFewer; LENGTH Name $30.; SET CorrFewer; Name="&PredFewer."; RUN;
DATA CorrMore;  LENGTH Name $30.; SET CorrMore;  Name="&PredMore.";  RUN;
DATA CorrCompare; LENGTH Name $30.; SET CorrFewer CorrMore; 
     PredCorr=Pred; TotalR2=PredCorr*PredCorr; 
     IF _NAME_="Pred" OR MISSING(_NAME_)=1 THEN DELETE; DROP Pred; RUN;
DATA CorrCompare; SET CorrCompare; TotalR2Diff=TotalR2-LAG1(TotalR2);
     KEEP Name PredCorr TotalR2 TotalR2Diff; RUN; 
TITLE9 "Total R2 (% Reduction) for &PredFewer. vs. &PredMore.";
PROC PRINT NOOBS DATA=CorrCompare; RUN; TITLE9; 
%MEND TotalR2;

* To use PseudoR2 macro;
* Ncov =     TOTAL # entries in covariance parameters output table;
* CovFewer = name of covparms table for nested model;
* CovMore =  name of covparms table for comparison modell;
%MACRO PseudoR2(NCov=,CovFewer=,CovMore=);
DATA &CovFewer.; LENGTH Name $30.; SET &CovFewer.; Name="&CovFewer."; RUN;
DATA &CovMore.;  LENGTH Name $30.; SET &CovMore.;  Name="&CovMore.";  RUN;
DATA CovCompare; LENGTH Name $30.; SET &CovFewer. &CovMore.; RUN;
DATA CovCompare; SET CovCompare; 
     PseudoR2=(LAG&Ncov.(Estimate)-Estimate)/LAG&Ncov.(Estimate); RUN;
DATA CovCompare; SET CovCompare; 
     IF CovParm IN("UN(2,1)","UN(3,1)","UN(4,1)","UN(3,2)","UN(4,2)","UN(4,3)") 
     THEN DELETE; RUN;
TITLE9 "PsuedoR2 (% Reduction) for &CovFewer. vs. &CovMore.";
PROC PRINT NOOBS DATA=CovCompare; RUN; TITLE9;
%MEND PseudoR2;

***********************************************************************************;
*******  BEGIN DATA MANIPULATION FOR CHAPTER 10a ALTERNATIVE TIME EXAMPLE   *******;
*******                 CHANGE "filesave" to your directory                 *******;
***********************************************************************************;

* Defining global variable for file location to be replaced in code below;
%LET filesave= C:\Dropbox\PilesOfVariance\Chapter10a\SAS;
* Location for SAS files for these models (uses macro variable filesave);
LIBNAME filesave "&filesave.";

* Import chapter 10 stacked data into work library;
DATA work.Chapter10a; SET filesave.SAS_Chapter10a; RUN;

* Create person means across time for descriptives;
PROC SORT DATA=work.Chapter10a; BY PersonID occasion; RUN;
PROC MEANS NOPRINT DATA=work.Chapter10a; BY PersonID;
     VAR ageT0 ytdeathT0;
     OUTPUT OUT=work.PersonMeans10a 
     MEAN(ageT0 ytdeathT0) = PMageT0 PMytdeathT0;
RUN;

* Create centered predictors for analysis;
DATA work.Chapter10a; SET work.Chapter10a;
* Time in study;
time = tvage-ageT0;
* Age (years since birth) variables;
roundage = ROUND(tvage,1);
tvage84 = tvage-84;
ageT084 = ageT0-84;
* Years to death variables;
roundytdeath = ROUND(tvytdeath,1);
tvytdeath7 = tvytdeath+7;
ytdeathT07 = ytdeathT0+7;
LABEL
time = "time: Years since Time 0"
roundage = "roundage: Age Rounded to Nearest Year"
tvage84 = "tvage84: Time-Varying Age (0=84 years)"
ageT084 = "ageT084: Age at Time 0 (0=84 years)"
roundytdeath = "roundytdeath: Years to Death Rounded to Nearest Year"
tvytdeath7 = "tvytdeath7: Time-Varying Years to Death (0=-7 years)"
ytdeathT07 = "ytdeathTo7: Years to Death at Time 0 (0=-7 years)";
* Subset sample to complete cases for all predictors;
IF NMISS(tvage, ageT0, ytdeathT0, tvytdeath, recall)>0 THEN DELETE;
RUN; 

* Creating 'fake people' to show unconditonal time trajectories;
* Each row is a fake person for which to create a predicted outcome;
* Each time metric will be used in a separate model;
DATA work.FakeUncTime; 
INPUT PersonID tvage tvage84 tvytdeath tvytdeath7 time; 
DATALINES;
-99  82 -2  -11  -4  0 
-99  84  0   -9  -2  2 
-99  86  2   -7   0  4 
-99  88  4   -5   2  6 
-99  90  6   -3   4  8 
-99  92  8   -1   6  . 
-99  94 10    .   .  . 
; RUN;
* Merge with real data;
DATA work.PlotUncTime; MERGE work.Chapter10a work.FakeUncTime; BY PersonID; RUN;

* Creating 'fake people' to show conditonal time trajectories;
* Each row is a fake person for which to create a predicted outcome;
* Each time metric will be used in a separate model;
DATA work.FakeCondTime; 
INPUT PersonID ageT084 tvage tvage84 ytdeathT07 tvytdeath tvytdeath7 time; 
DATALINES;
-99  -4 82 -2  -4 -11  -4  0 
-99  -4 84  0  -4  -9  -2  2 
-99  -4 86  2  -4  -7   0  4 
-99  -4 88  4  -4  -5   2  6 
-99  -4 90  6  -4  -3   4  8 
-99  -4 92  8  -4  -1   6  . 
-99  -4 94 10   .   .   .  .
-99   0 82 -2   0 -11  -4  0 
-99   0 84  0   0  -9  -2  2 
-99   0 86  2   0  -7   0  4 
-99   0 88  4   0  -5   2  6 
-99   0 90  6   0  -3   4  8 
-99   0 92  8   0  -1   6  . 
-99   0 94 10   .   .   .  .
-99   4 82 -2   4 -11  -4  0 
-99   4 84  0   4  -9  -2  2 
-99   4 86  2   4  -7   0  4 
-99   4 88  4   4  -5   2  6 
-99   4 90  6   4  -3   4  8 
-99   4 92  8   4  -1   6  . 
-99   4 94 10   .   .   .  .
; RUN;
* Merge with real data;
DATA work.PlotCondTime; MERGE work.Chapter10a work.FakeCondTime; BY PersonID; RUN;

* Creating 'fake people' to show conditonal time trajectories;
* Each row is a fake person for which to create a predicted outcome;
* Each time metric will be used in a separate model;
DATA work.FakeBothTime; 
INPUT PersonID ageT084 ytdeathT07 time; 
DATALINES;
-99  -4 -4 0 
-99  -4 -4 2
-99  -4 -4 4 
-99  -4 -4 6 
-99  -4 -4 8 
-99  -4  0 0 
-99  -4  0 2
-99  -4  0 4 
-99  -4  0 6 
-99  -4  0 8 
-99   0 -4 0 
-99   0 -4 2
-99   0 -4 4 
-99   0 -4 6 
-99   0 -4 8 
-99   0  0 0 
-99   0  0 2
-99   0  0 4 
-99   0  0 6 
-99   0  0 8 
-99   4 -4 0 
-99   4 -4 2
-99   4 -4 4 
-99   4 -4 6 
-99   4 -4 8 
-99   4  0 0 
-99   4  0 2
-99   4  0 4 
-99   4  0 6 
-99   4  0 8 
; RUN;
* Merge with real data;
DATA work.PlotBothTime; MERGE work.Chapter10a work.FakeBothTime; BY PersonID; RUN;

***********************************************************************************;
*******               BEGIN CHAPTER 10a ALTERNATIVE TIME MODELS             *******;
***********************************************************************************;

* Open output directory to save results to;
ODS HTML FILE="&filesave.\SAS_Chapter10a_Output.html"
         (URL="SAS_Chapter10a_Output.html") STYLE=HTMLBlue;

TITLE1 "Chapter 10a: Descriptive Statistics for Time-Invariant Variables";
PROC MEANS DATA=work.PersonMeans10a;
     VAR PMageT0 PMytdeathT0; 
RUN;
PROC CORR DATA=work.PersonMeans10a;
     VAR PMageT0 PMytdeathT0; 
RUN; TITLE1;

TITLE1 "Chapter 10a: Descriptive Statistics for Time-Varying Variables";
PROC MEANS DATA=work.Chapter10a; 
     VAR time tvage tvytdeath recall; 
RUN; TITLE1;

TITLE1 'Ch 10a: Empty Means, Random Intercept Model for Prose';
PROC MIXED DATA=work.Chapter10a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL recall =  / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredEmpty;
     RANDOM INTERCEPT / V VCORR TYPE=UN SUBJECT=PersonID;
RUN; TITLE1;
 
TITLE1 'Ch 10a: Saturated Means by Rounded Years in Study, Random Intercept Model';
PROC MIXED DATA=work.Chapter10a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID occasion;
     MODEL recall = occasion / NOINT SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
RUN; TITLE1;
 
TITLE1 'Ch 10a: Saturated Means by Rounded Age, Random Intercept Model';
PROC MIXED DATA=work.Chapter10a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID roundage;
     MODEL recall = roundage / NOINT SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
RUN; TITLE1;
 
TITLE1 'Ch 10a: Saturated Means by Rounded Years to Death, Random Intercept Model';
PROC MIXED DATA=work.Chapter10a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID roundytdeath;
     MODEL recall = roundytdeath / NOINT SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
RUN; TITLE1;
 
TITLE1 'Ch 10a: Empty Means, Random Intercept Model for Years since Birth';
PROC MIXED DATA=work.Chapter10a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL tvage =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / V VCORR TYPE=UN SUBJECT=PersonID;
RUN; TITLE1;
 
TITLE1 'Ch 10a: Empty Means, Random Intercept Model for Years to Death';
PROC MIXED DATA=work.Chapter10a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL tvytdeath =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / V VCORR TYPE=UN SUBJECT=PersonID;
RUN; TITLE1;
 
TITLE1 'Ch 10a: Empty Means, Random Intercept Model for Years in Study';
PROC MIXED DATA=work.Chapter10a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL time =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / V VCORR TYPE=UN SUBJECT=PersonID;
RUN; TITLE1;
 
TITLE1 'Ch 10a: Fixed Quadratic, Random Intercept Model for Years since Birth';
PROC MIXED DATA=work.Chapter10a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL recall = tvage84 tvage84*tvage84 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitFQRIAge;
RUN; TITLE1;
 
TITLE1 'Eq 10a.1: Random Linear Model for Years since Birth';
PROC MIXED DATA=work.Chapter10a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL recall = tvage84 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT tvage84 / TYPE=UN SUBJECT=PersonID;
RUN; TITLE1;
 
TITLE1 'Eq 10a.1: Fixed Quadratic, Random Linear Model for Years since Birth';
* Estimate model on data with fake people to make predictions;
PROC MIXED DATA=work.PlotUncTime COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL recall = tvage84 tvage84*tvage84 / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredFQRLAge;
     RANDOM INTERCEPT tvage84 / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitFQRLAge;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitFQRIAge, FitMore=FitFQRLAge);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=recall, PredFewer=PredEmpty, PredMore=PredFQRLAge);
     * Subset predicted outcomes data to fake people;
     DATA PredFQRLAge; SET PredFQRLAge; WHERE PersonID=-99; RUN;
     TITLE9 'Predicted Outcomes for Fake People';
     PROC PRINT NOOBS DATA=PredFQRLAge; VAR tvage84 pred; RUN;
 
TITLE1 'Ch 10a: Fixed Quadratic, Random Intercept Model for Years to Death';
PROC MIXED DATA=work.Chapter10a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL recall = tvytdeath7 tvytdeath7*tvytdeath7 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitFQRIYTD;
RUN; TITLE1;
 
TITLE1 'Eq 10a.1: Fixed Quadratic, Random Linear Model for Years to Death';
* Estimate model on data with fake people to make predictions;
PROC MIXED DATA=work.PlotUncTime COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL recall = tvytdeath7 tvytdeath7*tvytdeath7 / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredFQRLYTD;
     RANDOM INTERCEPT tvytdeath7 / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitFQRLYTD;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitFQRIYTD, FitMore=FitFQRLYTD);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=recall, PredFewer=PredEmpty, PredMore=PredFQRLYTD);
     * Subset predicted outcomes data to fake people;
     DATA PredFQRLYTD; SET PredFQRLYTD; WHERE PersonID=-99; RUN;
     TITLE9 'Predicted Outcomes for Fake People';
     PROC PRINT NOOBS DATA=PredFQRLYTD; VAR tvytdeath7 pred; RUN;
 
TITLE1 'Ch 10a: Fixed Quadratic, Random Intercept Model for Years in Study';
PROC MIXED DATA=work.Chapter10a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL recall = time time*time / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitFQRIYIS;
RUN; TITLE1;
 
TITLE1 'Eq 10a.1: Fixed Quadratic, Random Linear Model for Years in Study';
* Estimate model on data with fake people to make predictions;
PROC MIXED DATA=work.PlotUncTime COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL recall = time time*time / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredFQRLYIS;
     RANDOM INTERCEPT time / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitFQRLYIS;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitFQRIYIS, FitMore=FitFQRLYIS);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=recall, PredFewer=PredEmpty, PredMore=PredFQRLYIS);
     * Subset predicted outcomes data to fake people;
     DATA PredFQRLYIS; SET PredFQRLYIS; WHERE PersonID=-99; RUN;
     TITLE9 'Predicted Outcomes for Fake People';
     PROC PRINT NOOBS DATA=PredFQRLYIS; VAR time pred; RUN;
 
TITLE1 'Eq 10a.2: Fixed Quadratic, Random Linear Model for Years since Birth';
TITLE2 'Controlling for Birth Cohort';
* Estimate model on data with fake people to make predictions;
PROC MIXED DATA=work.PlotCondTime COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL recall = tvage84 tvage84*tvage84
                    ageT084 ageT084*ageT084 tvage84*ageT084
                     / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredCohAge;
     RANDOM INTERCEPT tvage84 / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitCohAge;
     CONTRAST 'Multivariate Test of Birth Cohort Contextual Effects' ageT084 1, ageT084*ageT084 1, tvage84*ageT084 1 / CHISQ;
     ESTIMATE 'Contextual Linear Birth Cohort on Intercept'          ageT084 1 / CL;
     ESTIMATE 'Contextual Quadratic Birth Cohort on Intercept'       ageT084*ageT084 1 / CL;
     ESTIMATE 'Contextual Linear Birth Cohort on Linear Slope'       tvage84*ageT084 1 / CL;
     ESTIMATE 'Total Linear Birth Cohort on Intercept'               ageT084 1 tvage84 1 / CL;
     ESTIMATE 'Total Quadratic Birth Cohort on Intercept'            ageT084*ageT084 1 tvage84*ageT084 1 tvage84*tvage84 1 / CL;
     ESTIMATE 'Total Linear Birth Cohort on Linear Slope'            tvage84*ageT084 1 tvage84*tvage84 2 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitFQRLAge, FitMore=FitCohAge);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=recall, PredFewer=PredFQRLAge, PredMore=PredCohAge);
     * Subset predicted outcomes data to fake people;
     DATA PredCohAge; SET PredCohAge; WHERE PersonID=-99; RUN;
     TITLE9 'Predicted Outcomes for Fake People';
     PROC PRINT NOOBS DATA=PredCohAge; VAR tvage84 ageT084 pred; RUN;
 
TITLE1 'Eq 10a.2: Fixed Quadratic, Random Linear Model for Years in Study';
TITLE2 'Controlling for Birth Cohort';
* Estimate model on data with fake people to make predictions;
PROC MIXED DATA=work.PlotCondTime COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL recall = time time*time
                    ageT084 ageT084*ageT084 time*ageT084
                     / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredCohAgeYIS;
     RANDOM INTERCEPT time / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitCohAgeYIS;
     CONTRAST 'Multivariate Test of Birth Cohort Total Effects' ageT084 1, ageT084*ageT084 1, time*ageT084 1 / CHISQ;
     ESTIMATE 'Contextual Linear Birth Cohort on Intercept'          ageT084 1 time -1 / CL;
     ESTIMATE 'Contextual Quadratic Birth Cohort on Intercept'       ageT084*ageT084 1 time*ageT084 -1 time*time 1 / CL;
     ESTIMATE 'Contextual Linear Birth Cohort on Linear Slope'       time*ageT084 1 time*time -2 / CL;
     ESTIMATE 'Total Linear Birth Cohort on Intercept'               ageT084 1 / CL;
     ESTIMATE 'Total Quadratic Birth Cohort on Intercept'            ageT084*ageT084 1 / CL;
     ESTIMATE 'Total Linear Birth Cohort on Linear Slope'            time*ageT084 1 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitFQRLYIS, FitMore=FitCohAgeYIS);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=recall, PredFewer=PredFQRLYIS, PredMore=PredCohAgeYIS);
     * Subset predicted outcomes data to fake people;
     DATA PredCohAgeYIS; SET PredCohAgeYIS; WHERE PersonID=-99; RUN;
     TITLE9 'Predicted Outcomes for Fake People';
     PROC PRINT NOOBS DATA=PredCohAgeYIS; VAR time ageT084 pred; RUN;
 
TITLE1 'Eq 10a.2: Fixed Quadratic, Random Linear Model for Years to Death';
TITLE2 'Controlling for Death Cohort';
* Estimate model on data with fake people to make predictions;
PROC MIXED DATA=work.PlotCondTime COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL recall = tvytdeath7 tvytdeath7*tvytdeath7
                    ytdeathT07 ytdeathT07*ytdeathT07 tvytdeath7*ytdeathT07
                     / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredCohYTD;
     RANDOM INTERCEPT tvytdeath7 / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitCohYTD;
     CONTRAST 'Multivariate Test of Death Cohort Contextual Effects' ytdeathT07 1, ytdeathT07*ytdeathT07 1, tvytdeath7*ytdeathT07 1 / CHISQ;
     ESTIMATE 'Contextual Linear Death Cohort on Intercept'          ytdeathT07 1 / CL;
     ESTIMATE 'Contextual Quadratic Death Cohort on Intercept'       ytdeathT07*ytdeathT07 1 / CL;
     ESTIMATE 'Contextual Linear Death Cohort on Linear Slope'       tvytdeath7*ytdeathT07 1 / CL;
     ESTIMATE 'Total Linear Death Cohort on Intercept'               ytdeathT07 1 tvytdeath7 1 / CL;
     ESTIMATE 'Total Quadratic Death Cohort on Intercept'            ytdeathT07*ytdeathT07 1 tvytdeath7*ytdeathT07 1 tvytdeath7*tvytdeath7 1 / CL;
     ESTIMATE 'Total Linear Death Cohort on Linear Slope'            tvytdeath7*ytdeathT07 1 tvytdeath7*tvytdeath7 2 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitFQRLYTD, FitMore=FitCohYTD);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=recall, PredFewer=PredFQRLYTD, PredMore=PredCohYTD);
     * Subset predicted outcomes data to fake people;
     DATA PredCohYTD; SET PredCohYTD; WHERE PersonID=-99; RUN;
     TITLE9 'Predicted Outcomes for Fake People';
     PROC PRINT NOOBS DATA=PredCohYTD; VAR tvytdeath7 ytdeathT07 pred; RUN;
 
TITLE1 'Eq 10a.2: Fixed Quadratic, Random Linear Model for Years in Study';
TITLE2 'Controlling for Death Cohort';
* Estimate model on data with fake people to make predictions;
PROC MIXED DATA=work.PlotCondTime COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL recall = time time*time
                    ytdeathT07 ytdeathT07*ytdeathT07 time*ytdeathT07
                     / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredCohYTDYIS;
     RANDOM INTERCEPT time / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitCohYTDYIS;
     CONTRAST 'Multivariate Test of Death Cohort Total Effects' ytdeathT07 1, ytdeathT07*ytdeathT07 1, time*ytdeathT07 1 / CHISQ;
     ESTIMATE 'Contextual Linear Birth Cohort on Intercept'          ytdeathT07 1 time -1 / CL;
     ESTIMATE 'Contextual Quadratic Birth Cohort on Intercept'       ytdeathT07*ytdeathT07 1 time*ytdeathT07 -1 time*time 1 / CL;
     ESTIMATE 'Contextual Linear Birth Cohort on Linear Slope'       time*ytdeathT07 1 time*time -2 / CL;
     ESTIMATE 'Total Linear Birth Cohort on Intercept'               ytdeathT07 1 / CL;
     ESTIMATE 'Total Quadratic Birth Cohort on Intercept'            ytdeathT07*ytdeathT07 1 / CL;
     ESTIMATE 'Total Linear Birth Cohort on Linear Slope'            time*ytdeathT07 1 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitFQRLYIS, FitMore=FitCohYTDYIS);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=recall, PredFewer=PredFQRLYIS, PredMore=PredCohYTDYIS);
     * Subset predicted outcomes data to fake people;
     DATA PredCohYTDYIS; SET PredCohYTDYIS; WHERE PersonID=-99; RUN;
     TITLE9 'Predicted Outcomes for Fake People';
     PROC PRINT NOOBS DATA=PredCohYTDYIS; VAR time ytdeathT07 pred; RUN;
 
TITLE1 'Eq 10a.4: Fixed Quadratic, Random Linear Model for Years in Study';
TITLE2 'Controlling for Birth Cohort and Death Cohort';
* Estimate model on data with fake people to make predictions;
PROC MIXED DATA=work.PlotBothTime COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL recall = time time*time
                    ageT084 ageT084*ageT084 time*ageT084
                    ytdeathT07 ytdeathT07*ytdeathT07 time*ytdeathT07
                     / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredBothYIS;
     RANDOM INTERCEPT time / TYPE=UN SUBJECT=PersonID;
RUN; TITLE1; TITLE2;
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=recall, PredFewer=PredFQRLYIS, PredMore=PredBothYIS);
     * Subset predicted outcomes data to fake people;
     DATA PredBothYIS; SET PredBothYIS; WHERE PersonID=-99; RUN;
     TITLE9 'Predicted Outcomes for Fake People';
     PROC PRINT NOOBS DATA=PredBothYIS; VAR time ageT084 ytdeathT07 pred; RUN;
 
****** END CHAPTER 10a MODELS ******;

* Close output directory;
ODS HTML CLOSE;

