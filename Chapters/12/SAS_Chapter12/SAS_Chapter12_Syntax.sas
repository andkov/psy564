* Output options: all can be turned on/off by adding or removing the NO;
* page number, date, centering, or page breaks, page length and width;
OPTIONS NOnumber NOdate NOcenter FormDlim=' ' PageSize=MAX LineSize=MAX;
* Eliminate SAS default titles and names of tables in output (TRACE ON to show);
TITLE; ODS TRACE OFF; ODS LISTING CLOSE;

***********************************************************************************;
*******   MACRO PROGRAMS TO AUTOMATE CALCULATIONS FOR CHAPTER 12 EXAMPLE    *******;
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
*******   BEGIN DATA MANIPULATION FOR CHAPTER 12 REPEATED MEASURES EXAMPLE  *******;
*******                 CHANGE "filesave" to your directory                 *******;
***********************************************************************************;

* Defining global variable for file location to be replaced in code below;
%LET filesave= C:\Dropbox\PilesOfVariance\Chapter12\SAS;
* Location for SAS files for these models (uses macro variable filesave);
LIBNAME filesave "&filesave.";

* Import chapter 12 stacked data to work library and create variables for analysis;
DATA work.Chapter12; SET filesave.SAS_Chapter12; 
* Log of response time;
logrt = LOG(rt);
* Make copy of 'older' to be used in heterogeneous variance models;
agegroup = older;
* Piecewise effect of age;
IF older=0 THEN yrs65=0; 
IF older=1 THEN yrs65=age-65;
* Item predictors;
rel3 = relevance - 3; 
sal3 = salience - 3; 
LABEL
logrt = "logrt: Natural Log RT in Seconds"
agegroup = "agegroup: Younger=0, Older=1"
yrs65 = "yrs65: Subject Years of Age Over 65 (0=65)"
rel3  = "rel3: Item Relevance (0=3)"
sal3  = "sal3: Item Salience (0=3)"; 
* Subset sample to complete cases for all predictors;
IF NMISS(older, yrs65, relevance, salience, logrt)>0 THEN DELETE;
RUN;

* Aggregate to subject-level data for subject descriptives;
PROC SORT DATA=work.Chapter12; BY SubjectID; RUN;
PROC MEANS NOPRINT DATA=work.Chapter12; BY SubjectID;
     VAR older yrs65;
     OUTPUT OUT=work.SubjectMeans12
     MEAN(older age) = SMolder SMage;
RUN;

* Aggregate to item-level data for item descriptives;
PROC SORT DATA=work.Chapter12; BY ItemID; RUN;
PROC MEANS NOPRINT DATA=work.Chapter12; BY ItemID;
     VAR relevance salience;
     OUTPUT OUT=work.ItemMeans12
     MEAN(relevance salience) = IMrelevance IMsalience;
RUN;

* Sort back to subjects, then items;
PROC SORT DATA=work.Chapter12; BY SubjectID ItemID; RUN;

* Creating 'fake people' to show final model interactions;
* Each row is a fake person for which to create a predicted outcome;
DATA work.FakePeople; INPUT SubjectID ItemID yrs65 older agegroup rel3 sal3; 
DATALINES;
-99  -99 15  1  1 -2 -2 
-99  -99 15  1  1 -1 -2
-99  -99 15  1  1  0 -2
-99  -99 15  1  1  1 -2
-99  -99 15  1  1  2 -2
-99  -99  0  1  1 -2 -2 
-99  -99  0  1  1 -1 -2
-99  -99  0  1  1  0 -2
-99  -99  0  1  1  1 -2
-99  -99  0  1  1  2 -2
-99  -99  0  0  0 -2 -2 
-99  -99  0  0  0 -1 -2
-99  -99  0  0  0  0 -2
-99  -99  0  0  0  1 -2
-99  -99  0  0  0  2 -2
-99  -99 15  1  1 -2  1 
-99  -99 15  1  1 -1  1
-99  -99 15  1  1  0  1
-99  -99 15  1  1  1  1
-99  -99 15  1  1  2  1
-99  -99  0  1  1 -2  1 
-99  -99  0  1  1 -1  1
-99  -99  0  1  1  0  1
-99  -99  0  1  1  1  1
-99  -99  0  1  1  2  1
-99  -99  0  0  0 -2  1 
-99  -99  0  0  0 -1  1
-99  -99  0  0  0  0  1
-99  -99  0  0  0  1  1
-99  -99  0  0  0  2  1
; RUN;
* Merge with real data;
DATA work.PlotFakePeople; MERGE work.Chapter12 work.FakePeople; 
     BY SubjectID ItemID; RUN;
 
***********************************************************************************;
*****               BEGIN CHAPTER 12 CROSSED RANDOM EFFECTS MODELS            *****;
***********************************************************************************;

* Open output directory to save results to;
ODS HTML FILE="&filesave.\SAS_Chapter12_Output.html"
         (URL="SAS_Chapter12_Output.html") STYLE=HTMLBlue;

TITLE1 "Chapter 12: Descriptive Statistics for Subject-Level Variables";
PROC MEANS DATA=work.SubjectMeans12;
     CLASS SMolder; 
     VAR SMage;
RUN; TITLE1;

TITLE1 "Chapter 12: Descriptive Statistics for Item-Level Variables";
PROC MEANS DATA=work.ItemMeans12; 
     VAR IMrelevance IMsalience; 
RUN; TITLE1;

TITLE1 "Chapter 12: Descriptive Statistics for Trial-Level Variables";
PROC MEANS DATA=work.Chapter12; 
     VAR rt logrt; 
RUN; TITLE1;

TITLE1 'Eq 12.8: Empty Means, Single-Level Model for Log RT';
PROC MIXED DATA=work.Chapter12 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     MODEL logrt =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     ODS OUTPUT InfoCrit=FitEmpty1;
RUN; TITLE1;
 
TITLE1 'Eq 12.9: Add Subject Random Intercept Variance';
PROC MIXED DATA=work.Chapter12 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS SubjectID;
     MODEL logrt =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=SubjectID;
     ODS OUTPUT InfoCrit=FitEmpty2;
RUN; TITLE1;
 
TITLE1 'Eq 12.10: Add Item Random Intercept Variance';
PROC MIXED DATA=work.Chapter12 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS SubjectID ItemID;
     MODEL logrt =  / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredEmpty;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=SubjectID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ItemID;
     ODS OUTPUT CovParms=CovEmpty InfoCrit=FitEmpty3;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitEmpty2, FitMore=FitEmpty3);
 
TITLE1 'Eq 12.11: Add Fixed Effects of Item Predictors';
PROC MIXED DATA=work.Chapter12 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS SubjectID ItemID;
     MODEL logrt = rel3 sal3 rel3*sal3 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=SubjectID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ItemID;
     ODS OUTPUT CovParms=CovItem InfoCrit=FitItem;
     CONTRAST 'Multivariate Test of 3 Item Predictor Effects' rel3 1, sal3 1, rel3*sal3 1 / CHISQ;
RUN; TITLE1;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=3, CovFewer=CovEmpty, CovMore=CovItem);
 
TITLE1 'Ch 12: Remove Item Random Intercept Variance';
PROC MIXED DATA=work.Chapter12 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS SubjectID;
     MODEL logrt = rel3 sal3 rel3*sal3 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=SubjectID;
     ODS OUTPUT InfoCrit=FitNoRandItem;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitNoRandItem, FitMore=FitItem);
 
TITLE1 'Eq 12.12: Add Fixed Main Effects of Subject Predictors';
PROC MIXED DATA=work.Chapter12 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS SubjectID ItemID;
     MODEL logrt = rel3 sal3 rel3*sal3 older yrs65 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=SubjectID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ItemID;
     ODS OUTPUT CovParms=CovSubject InfoCrit=FitSubject;
     CONTRAST 'Multivariate Test of 2 Subject Predictor Effects' older 1, yrs65 1 / CHISQ;
RUN; TITLE1;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=3, CovFewer=CovItem, CovMore=CovSubject);
 
TITLE1 'Ch 12: Remove Subject Random Intercept Variance';
PROC MIXED DATA=work.Chapter12 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ItemID;
     MODEL logrt = rel3 sal3 rel3*sal3 older yrs65 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ItemID;
     ODS OUTPUT InfoCrit=FitNoRandSubject;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitNoRandSubject, FitMore=FitSubject);
 
TITLE1 'Eq 12.13: Add Subject Random Salience Slope Variance and Covariance';
PROC MIXED DATA=work.Chapter12 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS SubjectID ItemID;
     MODEL logrt = rel3 sal3 rel3*sal3 older yrs65 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT sal3 / TYPE=UN SUBJECT=SubjectID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ItemID;
     ODS OUTPUT CovParms=CovRandSal InfoCrit=FitRandSal;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitSubject, FitMore=FitRandSal);
 
TITLE1 'Ch 12: Add Subject Random Relevance Slope Variance and Covariances';
PROC MIXED DATA=work.Chapter12 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS SubjectID ItemID;
     MODEL logrt = rel3 sal3 rel3*sal3 older yrs65 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT sal3 rel3 / TYPE=UN SUBJECT=SubjectID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ItemID;
     ODS OUTPUT InfoCrit=FitRandMean;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitRandSal, FitMore=FitRandMean);
 
TITLE1 'Eq 12.14: Add All Possible Fixed Effect Interactions';
PROC MIXED DATA=work.Chapter12 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS SubjectID ItemID;
     MODEL logrt = rel3 sal3 rel3*sal3 older yrs65
                   rel3*older sal3*older rel3*sal3*older
                   rel3*yrs65 sal3*yrs65 rel3*sal3*yrs65
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT sal3 / TYPE=UN SUBJECT=SubjectID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ItemID;
     ODS OUTPUT CovParms=CovAllInt;
RUN; TITLE1;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=5, CovFewer=CovRandSal, CovMore=CovAllInt);
 
TITLE1 'Eq 12.15: Keep Significant Fixed Effect Interactions';
PROC MIXED DATA=work.Chapter12 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS SubjectID ItemID;
     MODEL logrt = rel3 sal3 rel3*sal3 older yrs65
                   rel3*older sal3*older rel3*sal3*older
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT sal3 / TYPE=UN SUBJECT=SubjectID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ItemID;
     ODS OUTPUT CovParms=CovSigInt InfoCrit=FitSigInt;
RUN; TITLE1;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=5, CovFewer=CovRandSal, CovMore=CovSigInt);
 
TITLE1 'Ch 12: Remove Subject Random Slope Variance and Covariance';
PROC MIXED DATA=work.Chapter12 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS SubjectID ItemID;
     MODEL logrt = rel3 sal3 rel3*sal3 older yrs65
                   rel3*older sal3*older rel3*sal3*older
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=SubjectID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ItemID;
     ODS OUTPUT InfoCrit=FitNoRandSal;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitNoRandSal, FitMore=FitSigInt);
 
TITLE1 'Ch 12: Add Separate Residual Variances by Age Group';
PROC MIXED DATA=work.Chapter12 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS SubjectID ItemID agegroup;
     MODEL logrt = rel3 sal3 rel3*sal3 older yrs65
                   rel3*older sal3*older rel3*sal3*older
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT sal3 / TYPE=UN SUBJECT=SubjectID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ItemID;
     REPEATED / TYPE=VC SUBJECT=SubjectID*ItemID GROUP=agegroup;
     ODS OUTPUT InfoCrit=FitHetResVar;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitSigInt, FitMore=FitHetResVar);
 
TITLE1 'Ch 12: Add Separate Subject Random Effects Variances and Covariance by Age Group';
* Estimate model on data with fake people to make predictions;
PROC MIXED DATA=work.PlotFakePeople COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS SubjectID ItemID agegroup;
     MODEL logrt = rel3 sal3 rel3*sal3 older yrs65
                   rel3*older sal3*older rel3*sal3*older
                    / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredFinal;
     RANDOM INTERCEPT sal3 / TYPE=UN SUBJECT=SubjectID GROUP=agegroup;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ItemID;
     REPEATED / TYPE=VC SUBJECT=SubjectID*ItemID GROUP=agegroup;
     ODS OUTPUT InfoCrit=FitHetRandVar;
     ESTIMATE 'Relevance Effect at Salience=3 for Younger' rel3 1 rel3*older 0 rel3*sal3 0 rel3*sal3*older 0 / CL;
     ESTIMATE 'Relevance Effect at Salience=3 for Older'   rel3 1 rel3*older 1 rel3*sal3 0 rel3*sal3*older 0 / CL;
     ESTIMATE 'Salience Effect at Relevance=3 for Younger' sal3 1 sal3*older 0 rel3*sal3 0 rel3*sal3*older 0 / CL;
     ESTIMATE 'Salience Effect at Relevance=3 for Older'   sal3 1 sal3*older 1 rel3*sal3 0 rel3*sal3*older 0 / CL;
     ESTIMATE 'Relevance by Salience for Younger'          rel3*sal3 1 rel3*sal3*older 0 / CL;
     ESTIMATE 'Relevance by Salience for Older'            rel3*sal3 1 rel3*sal3*older 1 / CL;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitHetResVar, FitMore=FitHetRandVar);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=lgrt, PredFewer=PredEmpty, PredMore=PredFinal);
     * Subset predicted outcomes data to fake people;
     DATA PredFinal; SET PredFinal; WHERE SubjectID=-99; RUN;
     TITLE9 'Predicted Outcomes for Fake People';
     PROC PRINT NOOBS DATA=PredFinal; VAR rel3 sal3 older yrs65 agegroup pred; RUN;
 
****** END CHAPTER 12 MODELS ******;

* Close output directory;
ODS HTML CLOSE;

