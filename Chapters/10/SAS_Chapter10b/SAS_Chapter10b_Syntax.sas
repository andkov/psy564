* Output options: all can be turned on/off by adding or removing the NO;
* page number, date, centering, or page breaks, page length and width;
OPTIONS NOnumber NOdate NOcenter FormDlim=' ' PageSize=MAX LineSize=MAX;
* Eliminate SAS default titles and names of tables in output (TRACE ON to show);
TITLE; ODS TRACE OFF; ODS LISTING CLOSE;

***********************************************************************************;
*******   MACRO PROGRAMS TO AUTOMATE CALCULATIONS FOR CHAPTER 10b EXAMPLE   *******;
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
*******  BEGIN DATA MANIPULATION FOR CHAPTER 10b THREE-LEVEL TIME EXAMPLE   *******;
*******                 CHANGE "filesave" to your directory                 *******;
***********************************************************************************;

* Defining global variable for file location to be replaced in code below;
%LET filesave= C:\Dropbox\PilesOfVariance\Chapter10b\SAS;
* Location for SAS files for these models (uses macro variable filesave);
LIBNAME filesave "&filesave.";

* Import chapter 10 stacked data into work library;
DATA work.Chapter10b; SET filesave.SAS_Chapter10b; RUN;

* Create centered predictors for analysis;
DATA work.Chapter10b; SET work.Chapter10b;
* Piecewise slopes for session;
     IF session = 1 THEN DO; slope12 = -1; slope26 = 0; END;
ELSE IF session = 2 THEN DO; slope12 =  0; slope26 = 0; END;
ELSE IF session = 3 THEN DO; slope12 =  0; slope26 = 1; END;
ELSE IF session = 4 THEN DO; slope12 =  0; slope26 = 2; END;
ELSE IF session = 5 THEN DO; slope12 =  0; slope26 = 3; END;
ELSE IF session = 6 THEN DO; slope12 =  0; slope26 = 4; END;
* Linear slope for burst;
burst1 = burst-1;
* Difference between early and later bursts;
     IF burst=1 OR burst=2 THEN b1or2=1; 
ELSE IF burst>2 THEN b1or2=0;
LABEL 
slope12 = "slope12: Initial Slope (Session 1-2)"
slope26 = "slope26: Later Slope (Session 2-6)"
burst1 = "burst1: Measurement Burst (0=1)"
b1or2 = "b1or2: Is Burst 1 or 2 (0=no, 1=yes)";
* Subset sample to complete cases for all predictors;
IF NMISS(session, burst)>0 THEN DELETE;
RUN; 

* Creating 'fake people' to show predicted session*burst means;
* Each row is a fake person for which to create a predicted outcome;
DATA work.FakeMeans; 
INPUT PersonID burst burst1 b1or2 session slope12 slope26; 
DATALINES;
-99 1 0 1 1 -1 0
-99 1 0 1 2  0 0
-99 1 0 1 3  0 1
-99 1 0 1 4  0 2
-99 1 0 1 5  0 3
-99 1 0 1 6  0 4
-99 2 1 1 1 -1 0
-99 2 1 1 2  0 0
-99 2 1 1 3  0 1
-99 2 1 1 4  0 2
-99 2 1 1 5  0 3
-99 2 1 1 6  0 4
-99 3 2 0 1 -1 0
-99 3 2 0 2  0 0
-99 3 2 0 3  0 1
-99 3 2 0 4  0 2
-99 3 2 0 5  0 3
-99 3 2 0 6  0 4
-99 4 3 0 1 -1 0
-99 4 3 0 2  0 0
-99 4 3 0 3  0 1
-99 4 3 0 4  0 2
-99 4 3 0 5  0 3
-99 4 3 0 6  0 4
-99 5 4 0 1 -1 0
-99 5 4 0 2  0 0
-99 5 4 0 3  0 1
-99 5 4 0 4  0 2
-99 5 4 0 5  0 3
-99 5 4 0 6  0 4
; RUN;
* Merge with real data;
DATA work.PlotMeans; MERGE work.Chapter10b work.FakeMeans; BY PersonID; RUN;

***********************************************************************************;
*******               BEGIN CHAPTER 10b THREE-LEVEL TIME MODELS             *******;
***********************************************************************************;

* Open output directory to save results to;
ODS HTML FILE="&filesave.\SAS_Chapter10b_Output.html"
         (URL="SAS_Chapter10b_Output.html") STYLE=HTMLBlue;

TITLE1 "Chapter 10b: Descriptive Statistics for Time-Varying Variables";
PROC MEANS DATA=work.Chapter10b; 
     VAR symptoms posaff; 
RUN; TITLE1;

TITLE1 'Ch 10b: Empty Means, Single-Level Model for the Variance for Symptoms';
TITLE2 'Independent Observations';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL symptoms =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     ODS OUTPUT InfoCrit=FitEmpty1S;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 10b: Empty Means, Two-Level Model for the Variance for Symptoms';
TITLE2 'Sessions Within Burst*Persons';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL symptoms =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / VCORR TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitEmpty2S;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 10b.5: Empty Means, Three-Level Model for the Variance for Symptoms';
TITLE2 'Level-1 Sessions Within Level-2 Bursts Within Level-3 Persons';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL symptoms =  / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredEmpty3S;
     RANDOM INTERCEPT / V VCORR TYPE=UN SUBJECT=PersonID;
     RANDOM INTERCEPT / VCORR TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitEmpty3S;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitEmpty2S, FitMore=FitEmpty3S);
 
TITLE1 'Ch 10b: Empty Means, Single-Level Model for the Variance for Positive Affect';
TITLE2 'Independent Observations';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL posaff =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     ODS OUTPUT InfoCrit=FitEmpty1P;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 10b: Empty Means, Two-Level Model for the Variance for Positive Affect';
TITLE2 'Sessions Within Burst*Persons';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL posaff =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / VCORR TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitEmpty2P;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 10b.5: Empty Means, Three-Level Model for the Variance for Positive Affect';
TITLE2 'Level-1 Sessions Within Level-2 Bursts Within Level-3 Persons';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL posaff =  / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredEmpty3P;
     RANDOM INTERCEPT / V VCORR TYPE=UN SUBJECT=PersonID;
     RANDOM INTERCEPT / VCORR TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitEmpty3P;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitEmpty2P, FitMore=FitEmpty3P);
 
TITLE1 'Eq 10b.6: Saturated Means for Burst by Session';
TITLE2 'Three-Level Model for the Variance for Symptoms';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst session;
     MODEL symptoms = session burst session*burst / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitSatAllS;
     LSMEANS session burst session*burst / CL;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 10b.7: Piecewise Session Slopes by Observed Burst';
TITLE2 'Three-Level Model for the Variance for Symptoms';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL symptoms = slope12 slope26 burst
                      slope12*burst slope26*burst
                       / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitPiecebyBurstMeansS;
     ESTIMATE 'Session 2 at Burst 1'        intercept 1 burst 1 0 0 0 0 / CL;
     ESTIMATE 'Session 2 at Burst 2'        intercept 1 burst 0 1 0 0 0 / CL;
     ESTIMATE 'Session 2 at Burst 3'        intercept 1 burst 0 0 1 0 0 / CL;
     ESTIMATE 'Session 2 at Burst 4'        intercept 1 burst 0 0 0 1 0 / CL;
     ESTIMATE 'Session 2 at Burst 5'        intercept 1 burst 0 0 0 0 1 / CL;
     ESTIMATE 'Slope12 at Burst 1'          slope12 1 slope12*burst 1 0 0 0 0 / CL;
     ESTIMATE 'Slope12 at Burst 2'          slope12 1 slope12*burst 0 1 0 0 0 / CL;
     ESTIMATE 'Slope12 at Burst 3'          slope12 1 slope12*burst 0 0 1 0 0 / CL;
     ESTIMATE 'Slope12 at Burst 4'          slope12 1 slope12*burst 0 0 0 1 0 / CL;
     ESTIMATE 'Slope12 at Burst 5'          slope12 1 slope12*burst 0 0 0 0 1 / CL;
     ESTIMATE 'Slope26 at Burst 1'          slope26 1 slope26*burst 1 0 0 0 0 / CL;
     ESTIMATE 'Slope26 at Burst 2'          slope26 1 slope26*burst 0 1 0 0 0 / CL;
     ESTIMATE 'Slope26 at Burst 3'          slope26 1 slope26*burst 0 0 1 0 0 / CL;
     ESTIMATE 'Slope26 at Burst 4'          slope26 1 slope26*burst 0 0 0 1 0 / CL;
     ESTIMATE 'Slope26 at Burst 5'          slope26 1 slope26*burst 0 0 0 0 1 / CL;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 10b.8: Piecewise Session Slopes by Quadratic Burst';
TITLE2 'Three-Level Model for the Variance for Symptoms';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL symptoms = burst1 burst1*burst1 slope12 slope26
                      slope12*burst1 slope26*burst1
                      slope12*burst1*burst1 slope26*burst1*burst1
                       / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitPiecebyQuadBurstS;
     ESTIMATE 'Slope12 at Burst 1'          slope12 1 slope12*burst1 0 slope12*burst1*burst1 0 / CL;
     ESTIMATE 'Slope12 at Burst 2'          slope12 1 slope12*burst1 1 slope12*burst1*burst1 1 / CL;
     ESTIMATE 'Slope12 at Burst 3'          slope12 1 slope12*burst1 2 slope12*burst1*burst1 4 / CL;
     ESTIMATE 'Slope12 at Burst 4'          slope12 1 slope12*burst1 3 slope12*burst1*burst1 9 / CL;
     ESTIMATE 'Slope12 at Burst 5'          slope12 1 slope12*burst1 4 slope12*burst1*burst1 16 / CL;
     ESTIMATE 'Slope26 at Burst 1'          slope26 1 slope26*burst1 0 slope26*burst1*burst1 0 / CL;
     ESTIMATE 'Slope26 at Burst 2'          slope26 1 slope26*burst1 1 slope26*burst1*burst1 1 / CL;
     ESTIMATE 'Slope26 at Burst 3'          slope26 1 slope26*burst1 2 slope26*burst1*burst1 4 / CL;
     ESTIMATE 'Slope26 at Burst 4'          slope26 1 slope26*burst1 3 slope26*burst1*burst1 9 / CL;
     ESTIMATE 'Slope26 at Burst 5'          slope26 1 slope26*burst1 4 slope26*burst1*burst1 16 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitPiecebyQuadBurstS, FitMore=FitPiecebyBurstMeansS);
 
TITLE1 'Eq 10b.9: Piecewise Session Slopes (Slope26 by Quadratic Burst Only)';
TITLE2 'Three-Level Model for the Variance for Symptoms';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL symptoms = burst1 burst1*burst1 slope12 slope26
                      slope26*burst1 slope26*burst1*burst1
                       / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitPieceQuadBurst26S;
     ESTIMATE 'Slope26 at Burst 1'          slope26 1 slope26*burst1 0 slope26*burst1*burst1 0 / CL;
     ESTIMATE 'Slope26 at Burst 2'          slope26 1 slope26*burst1 1 slope26*burst1*burst1 1 / CL;
     ESTIMATE 'Slope26 at Burst 3'          slope26 1 slope26*burst1 2 slope26*burst1*burst1 4 / CL;
     ESTIMATE 'Slope26 at Burst 4'          slope26 1 slope26*burst1 3 slope26*burst1*burst1 9 / CL;
     ESTIMATE 'Slope26 at Burst 5'          slope26 1 slope26*burst1 4 slope26*burst1*burst1 16 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitPieceQuadBurst26S, FitMore=FitPiecebyBurstMeansS);
     %FitTest(FitFewer=FitPieceQuadBurst26S, FitMore=FitSatAllS);
 
TITLE1 'Ch 10b: Piecewise Session Slopes (Slope26 by Quadratic Burst Only)';
TITLE2 'Add Random Linear Burst Slope across Persons for Symptoms';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL symptoms = burst1 burst1*burst1 slope12 slope26
                      slope26*burst1 slope26*burst1*burst1
                       / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT burst1 / G GCORR TYPE=UN SUBJECT=PersonID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitRandBurstLin3S;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitPieceQuadBurst26S, FitMore=FitRandBurstLin3S);
 
TITLE1 'Ch 10b: Piecewise Session Slopes (Slope26 by Quadratic Burst Only)';
TITLE2 'Add Random Quadratic Burst Slope across Persons for Symptoms';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL symptoms = burst1 burst1*burst1 slope12 slope26
                      slope26*burst1 slope26*burst1*burst1
                       / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT burst1 burst1*burst1 / G GCORR TYPE=UN SUBJECT=PersonID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitRandBurstQuad3S;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitRandBurstLin3S, FitMore=FitRandBurstQuad3S);
 
TITLE1 'Ch 10b: Piecewise Session Slopes (Slope26 by Quadratic Burst Only)';
TITLE2 'Add Random Linear Slope12 Across Bursts for Symptoms';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL symptoms = burst1 burst1*burst1 slope12 slope26
                      slope26*burst1 slope26*burst1*burst1
                       / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT burst1 / G GCORR TYPE=UN SUBJECT=PersonID;
     RANDOM INTERCEPT slope12 / TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitRandSlope12at2S;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitRandBurstLin3S, FitMore=FitRandSlope12at2S);
 
TITLE1 'Ch 10b: Piecewise Session Slopes (Slope26 by Quadratic Burst Only)';
TITLE2 'Add Random Linear Slope12 Across Persons for Symptoms';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL symptoms = burst1 burst1*burst1 slope12 slope26
                      slope26*burst1 slope26*burst1*burst1
                       / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT burst1 slope12 / G GCORR TYPE=UN SUBJECT=PersonID;
     RANDOM INTERCEPT slope12 / TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitRandSlope12at23S;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitRandSlope12at2S, FitMore=FitRandSlope12at23S);
 
TITLE1 'Eq 10b.10: Piecewise Session Slopes (Slope26 by Quadratic Burst Only)';
TITLE2 'Add Random Linear Slope26 Across Bursts for Symptoms';
* Estimate model on data with fake people to make predictions;
PROC MIXED DATA=work.PlotMeans COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL symptoms = burst1 burst1*burst1 slope12 slope26
                      slope26*burst1 slope26*burst1*burst1
                       / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredUncS;
     RANDOM INTERCEPT burst1 / G GCORR TYPE=UN SUBJECT=PersonID;
     RANDOM INTERCEPT slope12 slope26 / TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitRandSlope26at2S;
     ESTIMATE 'Slope26 at Burst 1'          slope26 1 slope26*burst1 0 slope26*burst1*burst1 0 / CL;
     ESTIMATE 'Slope26 at Burst 2'          slope26 1 slope26*burst1 1 slope26*burst1*burst1 1 / CL;
     ESTIMATE 'Slope26 at Burst 3'          slope26 1 slope26*burst1 2 slope26*burst1*burst1 4 / CL;
     ESTIMATE 'Slope26 at Burst 4'          slope26 1 slope26*burst1 3 slope26*burst1*burst1 9 / CL;
     ESTIMATE 'Slope26 at Burst 5'          slope26 1 slope26*burst1 4 slope26*burst1*burst1 16 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitRandSlope12at2S, FitMore=FitRandSlope26at2S);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=symptoms, PredFewer=PredEmpty3S, PredMore=PredUncS);
     * Subset predicted outcomes data to fake people;
     DATA PredUncS; SET PredUncS; WHERE PersonID=-99; RUN;
     TITLE9 'Predicted Outcomes for Fake People';
     PROC PRINT NOOBS DATA=PredUncS; VAR burst1 slope12 slope26 pred; RUN;
 
TITLE1 'Ch 10b: Piecewise Session Slopes (Slope26 by Quadratic Burst Only)';
TITLE2 'Add Random Linear Slope26 Across Persons for Symptoms';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL symptoms = burst1 burst1*burst1 slope12 slope26
                      slope26*burst1 slope26*burst1*burst1
                       / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT burst1 slope26 / G GCORR TYPE=UN SUBJECT=PersonID;
     RANDOM INTERCEPT slope12 slope26 / TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitRandSlope26at23S;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitRandSlope26at2S, FitMore=FitRandSlope26at23S);
 
TITLE1 'Ch 10b: Final Unconditional Model for Symptoms';
TITLE2 'Remove Level-2 Random Effects Variances and Covariances';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL symptoms = burst1 burst1*burst1 slope12 slope26
                      slope26*burst1 slope26*burst1*burst1
                       / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT burst1 / G GCORR TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitNo2S;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitNo2S, FitMore=FitRandSlope26at2S);
 
TITLE1 'Eq 10b.6: Saturated Means for Burst by Session';
TITLE2 'Three-Level Model for the Variance for Positive Affect';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst session;
     MODEL posaff = session burst session*burst / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitSatAllP;
     LSMEANS session burst session*burst / CL;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 10b.7: Piecewise Session Slopes by Observed Burst';
TITLE2 'Three-Level Model for the Variance for Positive Affect';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL posaff = slope12 slope26 burst
                    slope12*burst slope26*burst
                     / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitPiecebyBurstMeansP;
     ESTIMATE 'Session 2 at Burst 1'        intercept 1 burst 1 0 0 0 0 / CL;
     ESTIMATE 'Session 2 at Burst 2'        intercept 1 burst 0 1 0 0 0 / CL;
     ESTIMATE 'Session 2 at Burst 3'        intercept 1 burst 0 0 1 0 0 / CL;
     ESTIMATE 'Session 2 at Burst 4'        intercept 1 burst 0 0 0 1 0 / CL;
     ESTIMATE 'Session 2 at Burst 5'        intercept 1 burst 0 0 0 0 1 / CL;
     ESTIMATE 'Slope12 at Burst 1'          slope12 1 slope12*burst 1 0 0 0 0 / CL;
     ESTIMATE 'Slope12 at Burst 2'          slope12 1 slope12*burst 0 1 0 0 0 / CL;
     ESTIMATE 'Slope12 at Burst 3'          slope12 1 slope12*burst 0 0 1 0 0 / CL;
     ESTIMATE 'Slope12 at Burst 4'          slope12 1 slope12*burst 0 0 0 1 0 / CL;
     ESTIMATE 'Slope12 at Burst 5'          slope12 1 slope12*burst 0 0 0 0 1 / CL;
     ESTIMATE 'Slope26 at Burst 1'          slope26 1 slope26*burst 1 0 0 0 0 / CL;
     ESTIMATE 'Slope26 at Burst 2'          slope26 1 slope26*burst 0 1 0 0 0 / CL;
     ESTIMATE 'Slope26 at Burst 3'          slope26 1 slope26*burst 0 0 1 0 0 / CL;
     ESTIMATE 'Slope26 at Burst 4'          slope26 1 slope26*burst 0 0 0 1 0 / CL;
     ESTIMATE 'Slope26 at Burst 5'          slope26 1 slope26*burst 0 0 0 0 1 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitPiecebyBurstMeansP, FitMore=FitSatAllP);
 
TITLE1 'Eq 10b.11and12: Piecewise Session Slopes, Linear Burst, Slope12 by Burst1or2';
TITLE2 'Three-Level Model for the Variance for Positive Affect';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL posaff = burst1 slope12 slope12*b1or2 slope26 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitPieceBurst1or2P;
     ESTIMATE 'Slope12 at Burst 1 or 2' slope12 1 slope12*b1or2 1 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitPieceBurst1or2P, FitMore=FitPiecebyBurstMeansP);
     %FitTest(FitFewer=FitPieceBurst1or2P, FitMore=FitSatAllP);
 
TITLE1 'Ch 10b: Piecewise Session Slopes, Linear Burst, Slope12 by Burst1or2';
TITLE2 'Add Random Linear Burst across Persons for Positive Affect';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL posaff = burst1 slope12 slope12*b1or2 slope26 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT burst1 / G GCORR TYPE=UN SUBJECT=PersonID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitRandBurstLin3P;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitPieceBurst1or2P, FitMore=FitRandBurstLin3P);
 
TITLE1 'Ch 10b: Piecewise Session Slopes, Linear Burst, Slope12 by Burst1or2';
TITLE2 'Add Fixed and Random Quadratic Burst across Persons for Positive Affect';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL posaff = burst1 slope12 slope12*b1or2 slope26
                    burst1*burst1
                     / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT burst1 burst1*burst1 / G GCORR TYPE=UN SUBJECT=PersonID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitRandBurstQuad3P;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitRandBurstLin3P, FitMore=FitRandBurstQuad3P);
 
TITLE1 'Ch 10b: Piecewise Session Slopes, Linear Burst, Slope12 by Burst1or2';
TITLE2 'Add Random Slope12 across Bursts for Positive Affect';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL posaff = burst1 slope12 slope12*b1or2 slope26 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT burst1 / G GCORR TYPE=UN SUBJECT=PersonID;
     RANDOM INTERCEPT slope12 / TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitRandSlope12at2P;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitRandBurstLin3P, FitMore=FitRandSlope12at2P);
 
TITLE1 'Ch 10b: Piecewise Session Slopes, Linear Burst, Slope12 by Burst1or2';
TITLE2 'Add Random Slope12 across Persons for Positive Affect';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL posaff = burst1 slope12 slope12*b1or2 slope26 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT burst1 slope12 / G GCORR TYPE=UN SUBJECT=PersonID;
     RANDOM INTERCEPT slope12 / TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitRandSlope12at23P;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitRandSlope12at2P, FitMore=FitRandSlope12at23P);
 
TITLE1 'Ch 10b: Piecewise Session Slopes, Linear Burst, Slope12 by Burst1or2';
TITLE2 'Add Random Slope12*b1or2 across Persons for Positive Affect';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL posaff = burst1 slope12 slope12*b1or2 slope26 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT burst1 slope12 slope12*b1or2 / G GCORR TYPE=UN SUBJECT=PersonID;
     RANDOM INTERCEPT slope12 / TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitRandSlope12Bat3P;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitRandSlope12at23P, FitMore=FitRandSlope12Bat3P);
 
TITLE1 'Ch 10b: Piecewise Session Slopes, Linear Burst, Slope12 by Burst1or2';
TITLE2 'Add Random Slope26 across Bursts for Positive Affect';
PROC MIXED DATA=work.Chapter10b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL posaff = burst1 slope12 slope12*b1or2 slope26 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT burst1 slope12 / G GCORR TYPE=UN SUBJECT=PersonID;
     RANDOM INTERCEPT slope12 slope26 / TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitRandSlope26at2P;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitRandSlope12at23P, FitMore=FitRandSlope26at2P);
 
TITLE1 'Eq 10b.13: Piecewise Session Slopes, Linear Burst, Slope12 by Burst1or2';
TITLE2 'Add Random Slope26 across Persons for Positive Affect';
* Estimate model on data with fake people to make predictions;
PROC MIXED DATA=work.PlotMeans COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL posaff = burst1 slope12 slope12*b1or2 slope26 / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredUncP;
     RANDOM INTERCEPT burst1 slope12 slope26 / G GCORR TYPE=UN SUBJECT=PersonID;
     RANDOM INTERCEPT slope12 slope26 / TYPE=UN SUBJECT=PersonID*burst;
     ODS OUTPUT InfoCrit=FitRandSlope26at23P;
     ESTIMATE 'Slope12 at Burst 1 or 2' slope12 1 slope12*b1or2 1 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitRandSlope26at2P, FitMore=FitRandSlope26at23P);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=posaff, PredFewer=PredEmpty3P, PredMore=PredUncP);
     * Subset predicted outcomes data to fake people;
     DATA PredUncP; SET PredUncP; WHERE PersonID=-99; RUN;
     TITLE9 'Predicted Outcomes for Fake People';
     PROC PRINT NOOBS DATA=PredUncP; VAR burst1 slope12 slope26 b1or2 pred; RUN;
 
TITLE1 'Ch 10b: Final Unconditional Model for Positive Affect';
TITLE2 'Removing Level-2 Random Effects Variances and Covariances';
PROC MIXED DATA=work.PlotMeans COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst;
     MODEL posaff = burst1 slope12 slope12*b1or2 slope26 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT burst1 slope12 slope26 / G GCORR TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitNo2P;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitNo2P, FitMore=FitRandSlope26at23P);
 
* Create double-stacked dataset for multivariate analysis;
* Also create dummy codes to serve as intercepts;
DATA work.Chapter10bdoublestack; SET work.Chapter10b;
DV="1symptoms"; dvS=1; dvP=0; outcome=symptoms; OUTPUT;
DV="2posaff  "; dvS=0; dvP=1; outcome=posaff;   OUTPUT;
LABEL
DV = "DV: Categorical indicator for which DV the row is for"
dvS = "dvS: Intercept 1=symptoms, 0=posaff"
dvP = "dvP: Intercept 0=symptoms, 1=posaff"
outcome = "outcome: Combined outcome variable column";
RUN;

TITLE1 'Ch 10b: Multivariate Model of Symptoms and Positive Affect';
TITLE2 'Tricking Univariate Software';
PROC MIXED DATA=work.Chapter10bdoublestack COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID burst session DV;
     MODEL outcome = dvS dvP dvS*burst1 dvS*burst1*burst1 dvS*slope12 dvS*slope26 
                     dvS*slope26*burst1 dvS*slope26*burst1*burst1 
                     dvP*burst1 dvP*slope12 dvP*slope12*b1or2 dvP*slope26 
                       / NOINT SOLUTION CHISQ CL DDFM=Satterthwaite;
     RANDOM dvS dvS*burst1 dvP dvP*burst1 dvP*slope12 dvP*slope26   / G GCORR TYPE=UN SUBJECT=PersonID;
     RANDOM dvS dvS*slope12 dvS*slope26 dvP dvP*slope12 dvP*slope26 / G GCORR TYPE=UN SUBJECT=PersonID*burst;
     REPEATED DV / R=2 RCORR=2 TYPE=UN SUBJECT=PersonID*burst*session;
RUN; TITLE1; TITLE2;
****** END CHAPTER 10b MODELS ******;

* Close output directory;
ODS HTML CLOSE;

