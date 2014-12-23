* Output options: all can be turned on/off by adding or removing the NO;
* page number, date, centering, or page breaks, page length and width;
OPTIONS NOnumber NOdate NOcenter FormDlim=' ' PageSize=MAX LineSize=MAX;
* Eliminate SAS default titles and names of tables in output (TRACE ON to show);
TITLE; ODS TRACE OFF; ODS LISTING CLOSE;

***********************************************************************************;
*******   MACRO PROGRAMS TO AUTOMATE CALCULATIONS FOR CHAPTER 5 EXAMPLE     *******;
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
*******             BEGIN DATA MANIPULATION OF CHAPTER 5 EXAMPLE            *******;
*******               CHANGE "filesave" to your directory                   *******;
***********************************************************************************;

* Defining global variable for file location to be replaced in code below;
%LET filesave= C:\Dropbox\PilesOfVariance\Chapter5\SAS;
* Location for SAS files for these models (uses macro variable filesave);
LIBNAME filesave "&filesave.";
    
* Import chapter 5 stacked data and center time;
DATA work.Chapter5; SET filesave.SAS_Chapter5; 
time = wave - 1; LABEL time= "time: Time in Study (0=1)";
RUN;

***********************************************************************************;
*******                       BEGIN CHAPTER 5 MODELS                        *******;
***********************************************************************************;

* Open output directory to save results to;
ODS HTML FILE="&filesave.\SAS_Chapter5_Output.html"
         (URL="SAS_Chapter5_Output.html") STYLE=HTMLBlue;

TITLE1 "Chapter 5 Example: Means by Wave for outcome";
* CLASS= means per wave, WAYS= means overall=0 and per wave=1;
PROC MEANS MEAN STDDEV MIN MAX DATA=work.Chapter5; 
     CLASS wave;
     WAYS 0 1;
     VAR outcome;
RUN; TITLE1;

TITLE1 'Eq 5.1: Empty Means, Random Intercept Model';
PROC MIXED DATA=work.Chapter5 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID wave;
     MODEL outcome =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / G GCORR V VCORR TYPE=UN SUBJECT=PersonID;
     REPEATED wave / R RCORR TYPE=VC SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovEmpty;
RUN; TITLE1;
 
TITLE1 'Eq 5.3: Fixed Linear Time, Random Intercept Model';
PROC MIXED DATA=work.Chapter5 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID wave;
     MODEL outcome = time / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / G GCORR V VCORR TYPE=UN SUBJECT=PersonID;
     REPEATED wave / R RCORR TYPE=VC SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovFixLin InfoCrit=FitFixLin;
     ESTIMATE 'Intercept at Time=0'              intercept 1 time 0 / CL;
     ESTIMATE 'Intercept at Time=1'              intercept 1 time 1 / CL;
     ESTIMATE 'Intercept at Time=2'              intercept 1 time 2 / CL;
     ESTIMATE 'Intercept at Time=3'              intercept 1 time 3 / CL;
RUN; TITLE1;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=2, CovFewer=CovEmpty, CovMore=CovFixLin);
 
TITLE1 'Eq 5.5: Random Linear Time Model';
PROC MIXED DATA=work.Chapter5 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID wave;
     MODEL outcome = time / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time / G GCORR V VCORR TYPE=UN SUBJECT=PersonID;
     REPEATED wave / R RCORR TYPE=VC SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitRandLin;
     ESTIMATE 'Intercept at Time=0'              intercept 1 time 0 / CL;
     ESTIMATE 'Intercept at Time=1'              intercept 1 time 1 / CL;
     ESTIMATE 'Intercept at Time=2'              intercept 1 time 2 / CL;
     ESTIMATE 'Intercept at Time=3'              intercept 1 time 3 / CL;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitFixLin, FitMore=FitRandLin);
 
TITLE1 'Ch 5: Saturated Means, Unstructured Variance Model';
TITLE2 'ANSWER KEY for both sides of the model';
PROC MIXED DATA=work.Chapter5 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID wave;
     MODEL outcome = wave / SOLUTION CL CHISQ DDFM=Satterthwaite;
     REPEATED wave / R RCORR TYPE=UN SUBJECT=PersonID;
     LSMEANS wave / DIFF=ALL CL;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 5: Random Linear Time Model with AR1 R Matrix';
PROC MIXED DATA=work.Chapter5 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID wave;
     MODEL outcome = time / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time / G GCORR V VCORR TYPE=UN SUBJECT=PersonID;
     REPEATED wave / R RCORR TYPE=AR(1) SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitRandLinAR1;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitRandLin, FitMore=FitRandLinAR1);
 
TITLE1 'Ch 5: Random Linear Time Model with TOEP2 R Matrix';
PROC MIXED DATA=work.Chapter5 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID wave;
     MODEL outcome = time / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time / G GCORR V VCORR TYPE=UN SUBJECT=PersonID;
     REPEATED wave / R RCORR TYPE=TOEP(2) SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitRandLinTOEP2;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitRandLin, FitMore=FitRandLinTOEP2);
 
****** END CHAPTER 5 MODELS ******;

* Close output directory;
ODS HTML CLOSE;

