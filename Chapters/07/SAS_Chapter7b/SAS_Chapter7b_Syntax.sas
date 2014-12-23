* Output options: all can be turned on/off by adding or removing the NO;
* page number, date, centering, or page breaks, page length and width;
OPTIONS NOnumber NOdate NOcenter FormDlim=' ' PageSize=MAX LineSize=MAX;
* Eliminate SAS default titles and names of tables in output (TRACE ON to show);
TITLE; ODS TRACE OFF; ODS LISTING CLOSE;

***********************************************************************************;
*******  MACRO PROGRAMS TO AUTOMATE CALCULATIONS FOR CHAPTER 7b EXAMPLE     *******;
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
*******        BEGIN DATA MANIPULATION FOR CHAPTER 7b CHANGE EXAMPLE        *******;
*******                 CHANGE "filesave" to your directory                 *******;
***********************************************************************************;

* Defining global variable for file location to be replaced in code below;
%LET filesave= C:\Dropbox\PilesOfVariance\Chapter7b\SAS;
* Location for SAS files for these models (uses macro variable filesave);
LIBNAME filesave "&filesave.";

* Import chapter 7b multivariate data in work library;
DATA work.Chapter7bmultiv; SET filesave.SAS_Chapter7b; RUN;

* Stack multivariate data;
* Also make a copy of monitor18 for later;
DATA work.Chapter7b; SET work.Chapter7bmultiv;
copymonitor18 = monitor18;
occasion=12; age=age12; risky=risky12; monitor=monitor12; OUTPUT;
occasion=13; age=age13; risky=risky13; monitor=monitor13; OUTPUT;
occasion=14; age=age14; risky=risky14; monitor=monitor14; OUTPUT;
occasion=15; age=age15; risky=risky15; monitor=monitor15; OUTPUT;
occasion=16; age=age16; risky=risky16; monitor=monitor16; OUTPUT;
occasion=17; age=age17; risky=risky17; monitor=monitor17; OUTPUT;
occasion=18; age=age18; risky=risky18; monitor=monitor18; OUTPUT;
* Drop old unnecessary multivariate variables;
DROP age12-age18 risky12-risky18 monitor12-monitor18;
LABEL   
occasion = "occasion: Occasion of Measurement (12-18)"
age = "age: Exact Age at Occasion"
risky = "risky: Risky Behavior at Occasion"
monitor = "monitor: Monitoring at Occasion";
RUN;

* Center predictors for analysis in stacked data;
DATA work.Chapter7b; SET work.Chapter7b;
agec18 = age - 18;
att4 = attitude12 - 4;
LABEL
agec18 = "agec18: Exact Age (0=18)"
att4 = "att4: Age 12 Attitudes (0=4)";
* Subset sample to complete cases for all eventual predictors in chapter 9;
IF NMISS(agec18, att4, risky, monitor)>0 THEN DELETE;
RUN;

* Creating 'fake people' to show age*age*attitudes interaction;
* Each row is a fake person for which to create a predicted outcome;
DATA work.FakeAgeAtt; INPUT PersonID agec18 att4; 
DATALINES;
-99 -6 -2
-99 -5 -2
-99 -4 -2
-99 -3 -2
-99 -2 -2
-99 -1 -2
-99  0 -2
-99 -6  1
-99 -5  1
-99 -4  1
-99 -3  1
-99 -2  1
-99 -1  1
-99  0  1  
; RUN;
* Merge with real data;
DATA work.PlotAgeAtt; MERGE work.Chapter7b work.FakeAgeAtt; BY PersonID; RUN;

***********************************************************************************;
*******                    BEGIN CHAPTER 7b CHANGE MODELS                   *******;
***********************************************************************************;

* Open output directory to save results to;
ODS HTML FILE="&filesave.\SAS_Chapter7b_Output.html"
         (URL="SAS_Chapter7b_Output.html") STYLE=HTMLBlue;

TITLE1 "Chapter 7b: Descriptive Statistics for Time-Invariant Variables";
PROC MEANS DATA=work.Chapter7bmultiv; 
     VAR attitude12; 
RUN; TITLE1;

TITLE1 "Chapter 7b: Descriptive Statistics for Time-Varying Variables";
PROC MEANS DATA=work.Chapter7b; 
     VAR age risky; 
RUN; TITLE1;

TITLE1 'Ch 7b: Empty Means, Random Intercept Model';
PROC MIXED DATA=work.Chapter7b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL risky =  / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredEmpty;
     RANDOM INTERCEPT / V VCORR TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovEmpty;
RUN; TITLE1;
 
TITLE1 'Ch 7b: Saturated Means by Rounded Occasion, Unstructured Variance Model';
PROC MIXED DATA=work.Chapter7b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID occasion;
     MODEL risky = occasion / SOLUTION CL CHISQ DDFM=Satterthwaite;
     REPEATED occasion / R RCORR TYPE=UN SUBJECT=PersonID;
     LSMEANS occasion / DIFF=ALL CL;
RUN; TITLE1;
 
TITLE1 'Ch 7b: Fixed Linear Age, Random Intercept Model';
PROC MIXED DATA=work.Chapter7b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL risky = agec18 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovFixLin InfoCrit=FitFixLin;
     ESTIMATE 'Intercept at Age=12'    intercept 1 agec18 -6 / CL;
     ESTIMATE 'Intercept at Age=13'    intercept 1 agec18 -5 / CL;
     ESTIMATE 'Intercept at Age=14'    intercept 1 agec18 -4 / CL;
     ESTIMATE 'Intercept at Age=15'    intercept 1 agec18 -3 / CL;
     ESTIMATE 'Intercept at Age=16'    intercept 1 agec18 -2 / CL;
     ESTIMATE 'Intercept at Age=17'    intercept 1 agec18 -1 / CL;
     ESTIMATE 'Intercept at Age=18'    intercept 1 agec18 0 / CL;
RUN; TITLE1;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=2, CovFewer=CovEmpty, CovMore=CovFixLin);
 
TITLE1 'Ch 7b: Random Linear Age Model';
PROC MIXED DATA=work.Chapter7b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL risky = agec18 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT agec18 / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovRandLin InfoCrit=FitRandLin;
     ESTIMATE 'Intercept at Age=12'    intercept 1 agec18 -6 / CL;
     ESTIMATE 'Intercept at Age=13'    intercept 1 agec18 -5 / CL;
     ESTIMATE 'Intercept at Age=14'    intercept 1 agec18 -4 / CL;
     ESTIMATE 'Intercept at Age=15'    intercept 1 agec18 -3 / CL;
     ESTIMATE 'Intercept at Age=16'    intercept 1 agec18 -2 / CL;
     ESTIMATE 'Intercept at Age=17'    intercept 1 agec18 -1 / CL;
     ESTIMATE 'Intercept at Age=18'    intercept 1 agec18 0 / CL;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitFixLin, FitMore=FitRandLin);
 
TITLE1 'Eq 7b.8: Fixed Quadratic, Random Linear Age Model';
PROC MIXED DATA=work.Chapter7b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL risky = agec18 agec18*agec18 / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredAge;
     RANDOM INTERCEPT agec18 / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovFixQuad InfoCrit=FitFixQuad;
     ESTIMATE 'Intercept at Age=12'              intercept 1 agec18 -6 agec18*agec18 36 / CL;
     ESTIMATE 'Intercept at Age=13'              intercept 1 agec18 -5 agec18*agec18 25 / CL;
     ESTIMATE 'Intercept at Age=14'              intercept 1 agec18 -4 agec18*agec18 16 / CL;
     ESTIMATE 'Intercept at Age=15'              intercept 1 agec18 -3 agec18*agec18 9 / CL;
     ESTIMATE 'Intercept at Age=16'              intercept 1 agec18 -2 agec18*agec18 4 / CL;
     ESTIMATE 'Intercept at Age=17'              intercept 1 agec18 -1 agec18*agec18 1 / CL;
     ESTIMATE 'Intercept at Age=18'              intercept 1 agec18 0 agec18*agec18 0 / CL;
     ESTIMATE 'Linear Slope at Age=12'           agec18 1 agec18*agec18 -12 / CL;
     ESTIMATE 'Linear Slope at Age=13'           agec18 1 agec18*agec18 -10 / CL;
     ESTIMATE 'Linear Slope at Age=14'           agec18 1 agec18*agec18 -8 / CL;
     ESTIMATE 'Linear Slope at Age=15'           agec18 1 agec18*agec18 -6 / CL;
     ESTIMATE 'Linear Slope at Age=16'           agec18 1 agec18*agec18 -4 / CL;
     ESTIMATE 'Linear Slope at Age=17'           agec18 1 agec18*agec18 -2 / CL;
     ESTIMATE 'Linear Slope at Age=18'           agec18 1 agec18*agec18 0 / CL;
RUN; TITLE1;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=4, CovFewer=CovRandLin, CovMore=CovFixQuad);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=risky, PredFewer=PredEmpty, PredMore=PredAge);
 
TITLE1 'Ch 7b: Random Quadratic Age Model';
PROC MIXED DATA=work.Chapter7b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL risky = agec18 agec18*agec18 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT agec18 agec18*agec18 / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitRandQuad;
     ESTIMATE 'Intercept at Age=12'              intercept 1 agec18 -6 agec18*agec18 36 / CL;
     ESTIMATE 'Intercept at Age=13'              intercept 1 agec18 -5 agec18*agec18 25 / CL;
     ESTIMATE 'Intercept at Age=14'              intercept 1 agec18 -4 agec18*agec18 16 / CL;
     ESTIMATE 'Intercept at Age=15'              intercept 1 agec18 -3 agec18*agec18 9 / CL;
     ESTIMATE 'Intercept at Age=16'              intercept 1 agec18 -2 agec18*agec18 4 / CL;
     ESTIMATE 'Intercept at Age=17'              intercept 1 agec18 -1 agec18*agec18 1 / CL;
     ESTIMATE 'Intercept at Age=18'              intercept 1 agec18 0 agec18*agec18 0 / CL;
     ESTIMATE 'Linear Slope at Age=12'           agec18 1 agec18*agec18 -12 / CL;
     ESTIMATE 'Linear Slope at Age=13'           agec18 1 agec18*agec18 -10 / CL;
     ESTIMATE 'Linear Slope at Age=14'           agec18 1 agec18*agec18 -8 / CL;
     ESTIMATE 'Linear Slope at Age=15'           agec18 1 agec18*agec18 -6 / CL;
     ESTIMATE 'Linear Slope at Age=16'           agec18 1 agec18*agec18 -4 / CL;
     ESTIMATE 'Linear Slope at Age=17'           agec18 1 agec18*agec18 -2 / CL;
     ESTIMATE 'Linear Slope at Age=18'           agec18 1 agec18*agec18 0 / CL;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitFixQuad, FitMore=FitRandQuad);
 
TITLE1 'Ch 7b: Fixed Cubic, Random Quadratic Age Model';
PROC MIXED DATA=work.Chapter7b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL risky = agec18 agec18*agec18 agec18*agec18*agec18 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT agec18 / TYPE=UN SUBJECT=PersonID;
RUN; TITLE1;
 
TITLE1 'Ch 7b: Fixed Quadratic, Random Linear Age Model';
TITLE2 'Attitudes Predicting Intercept';
PROC MIXED DATA=work.Chapter7b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL risky = agec18 agec18*agec18
                   att4
                    / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredAttInt;
     RANDOM INTERCEPT agec18 / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovAttInt;
RUN; TITLE1; TITLE2;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=4, CovFewer=CovFixQuad, CovMore=CovAttInt);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=risky, PredFewer=PredAge, PredMore=PredAttInt);
 
TITLE1 'Ch 7b: Fixed Quadratic, Random Linear Age Model';
TITLE2 'Attitudes Predicting Linear Age Slope';
* Estimate model on data with fake people to make predictions;
PROC MIXED DATA=work.PlotAgeAtt COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL risky = agec18 agec18*agec18
                   att4 agec18*att4
                    / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredAttLin;
     RANDOM INTERCEPT agec18 / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovAttLin;
RUN; TITLE1; TITLE2;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=4, CovFewer=CovAttInt, CovMore=CovAttLin);
     %PseudoR2(NCov=4, CovFewer=CovFixQuad, CovMore=CovAttLin);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=risky, PredFewer=PredAge, PredMore=PredAttLin);
     * Subset predicted outcomes data to fake people;
     DATA PredAttLin; SET PredAttLin; WHERE PersonID=-99; RUN;
     TITLE9 'Predicted Outcomes for Fake People';
     PROC PRINT NOOBS DATA=PredAttLin; VAR agec18 att4 pred; RUN;
 
TITLE1 'Eq 7b.9: Fixed Quadratic, Random Linear Age Model';
TITLE2 'Attitudes Predicting Quadratic Age Slope';
PROC MIXED DATA=work.Chapter7b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL risky = agec18 agec18*agec18
                   att4 agec18*att4 agec18*agec18*att4
                    / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredAttQuad;
     RANDOM INTERCEPT agec18 / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovAttQuad;
RUN; TITLE1; TITLE2;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=4, CovFewer=CovAttLin, CovMore=CovAttQuad);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=risky, PredFewer=PredAge, PredMore=PredAttQuad);
 
****** END CHAPTER 7b MODELS ******;

* Close output directory;
ODS HTML CLOSE;

