* Output options: all can be turned on/off by adding or removing the NO;
* page number, date, centering, or page breaks, page length and width;
OPTIONS NOnumber NOdate NOcenter FormDlim=' ' PageSize=MAX LineSize=MAX;
* Eliminate SAS default titles and names of tables in output (TRACE ON to show);
TITLE; ODS TRACE OFF; ODS LISTING CLOSE;

***********************************************************************************;
*******   MACRO PROGRAMS TO AUTOMATE CALCULATIONS FOR CHAPTER 9 EXAMPLE     *******;
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
*******             BEGIN DATA MANIPULATION FOR CHAPTER 9 EXAMPLE           *******;
*******                 CHANGE "filesave" to your directory                 *******;
***********************************************************************************;

* Defining global variable for file location to be replaced in code below;
%LET filesave= C:\Dropbox\PilesOfVariance\Chapter9\SAS;
* Location for SAS files for these models (uses macro variable filesave);
LIBNAME filesave "&filesave.";

* Import chapter 9 multivariate data in work library;
* Also make person mean monitoring;
DATA work.Chapter9multiv; SET filesave.SAS_Chapter9;
PMmonitor = MEAN(OF monitor12-monitor18);
LABEL PMmonitor = "PMmonitor: Person Mean Monitoring";
RUN;

* Stack multivariate data;
* Also make a copy of monitor18 for later;
DATA work.Chapter9; SET work.Chapter9multiv;
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
copymonitor18 = "copymonitor18: Monitoring at Age 18 (copy)"
occasion = "occasion: Occasion of Measurement (12-18)"
age = "age: Exact Age at Occasion"
risky = "risky: Risky Behavior at Occasion"
monitor = "monitor: Monitoring at Occasion";
RUN;

* Center predictors for analysis in stacked data;
DATA work.Chapter9; SET work.Chapter9;
agec18 = age - 18;
att4 = attitude12 - 4;
mon3 = monitor - 3;
PMmon3 = PMmonitor - 3;
WPmon = monitor - PMmonitor;
Age18mon3 = copymonitor18 - 3;
Change18mon = monitor - copymonitor18;
LABEL
agec18 = "agec18: Exact Age (0=18)"
att4 = "att4: Age 12 Attitudes (0=4)"
mon3 = "mon3: Monitoring (0=3)"
PMmon3 = "PMmon3: Person Mean Monitoring (0=3)"
WPmon = "WPmon: Within-Person Monitoring (0=PM)"
Age18mon3 = "Age18mon3: BP Monitoring at Age 18 (0=3)"
Change18mon = "Change18mon: WP Monitoring from Age 18 (0=Age18)";
* Subset sample to complete cases for all predictors;
IF NMISS(agec18, att4, risky, PMmonitor, Age18mon3, monitor)>0 THEN DELETE;
RUN;

***********************************************************************************;
*****                         BEGIN CHAPTER 9 MODELS                          *****;
*****  NOTE: Truly multivariate longitudinal models are not possible in MIXED  ****;
***********************************************************************************;

* Open output directory to save results to;
ODS HTML FILE="&filesave.\SAS_Chapter9_Output.html"
         (URL="SAS_Chapter9_Output.html") STYLE=HTMLBlue;

TITLE1 "Chapter 9: Descriptive Statistics for Time-Invariant Variables";
PROC MEANS DATA=work.Chapter9multiv; 
     VAR attitude12 PMmonitor monitor18; 
RUN; 
PROC CORR DATA=work.Chapter9multiv;
     VAR PMmonitor monitor12 monitor18;
RUN; TITLE1;

TITLE1 "Chapter 9: Descriptive Statistics for Time-Varying Variables";
PROC MEANS DATA=work.Chapter9; 
     VAR age risky monitor WPmon Change18mon; 
RUN; TITLE1;
PROC CORR DATA=work.Chapter9;
     VAR PMmon3 Age18mon3 WPmon Change18mon;
RUN; TITLE1;

TITLE1 'Ch 9: Empty Means, Random Intercept Model for Monitoring';
PROC MIXED DATA=work.Chapter9 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL monitor =  / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredEmpty;
     RANDOM INTERCEPT / V VCORR TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovEmpty;
RUN; TITLE1;
 
TITLE1 'Ch 9: Fixed Linear Age, Random Intercept Model for Monitoring';
PROC MIXED DATA=work.Chapter9 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL monitor = agec18 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovFixLin InfoCrit=FitFixLin;
RUN; TITLE1;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=2, CovFewer=CovEmpty, CovMore=CovFixLin);
 
TITLE1 'Ch 9: Random Linear Age Model for Monitoring';
PROC MIXED DATA=work.Chapter9 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL monitor = agec18 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT agec18 / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovRandLin InfoCrit=FitRandLin;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitFixLin, FitMore=FitRandLin);
 
TITLE1 'Ch 9: Fixed Quadratic, Random Linear Age Model for Monitoring';
PROC MIXED DATA=work.Chapter9 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL monitor = agec18 agec18*agec18 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT agec18 / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovFixQuad InfoCrit=FitFixQuad;
RUN; TITLE1;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=4, CovFewer=CovRandLin, CovMore=CovFixQuad);
 
TITLE1 'Ch 9: Random Quadratic Age Model for Monitoring';
PROC MIXED DATA=work.Chapter9 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL monitor = agec18 agec18*agec18 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT agec18 agec18*agec18 / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitRandQuad;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitFixQuad, FitMore=FitRandQuad);
 
TITLE1 'Ch 9: Fixed Quadratic, Random Linear Age Model';
TITLE2 'Conditional Baseline with Attitudes Predicting Linear Age Slope';
PROC MIXED DATA=work.Chapter9 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL risky = agec18 agec18*agec18
                   att4 agec18*att4
                    / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredAttOnly;
     RANDOM INTERCEPT agec18 / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovAttOnly;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 9.1: Predicting Quadratic Change in Risky Behavior';
TITLE2 'From Person Mean Monitoring as Between-Person Monitoring';
PROC MIXED DATA=work.Chapter9 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL risky = agec18 agec18*agec18 att4 agec18*att4
                   PMmon3 agec18*PMmon3 agec18*agec18*PMmon3
                    / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredPMBP;
     RANDOM INTERCEPT agec18 / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovPMBP;
     ESTIMATE 'Effect of PM Monitoring at Age 12'          PMmon3 1 agec18*PMmon3 -6 agec18*agec18*PMmon3 36 / CL;
     ESTIMATE 'Effect of PM Monitoring at Age 14'          PMmon3 1 agec18*PMmon3 -4 agec18*agec18*PMmon3 16 / CL;
     ESTIMATE 'Effect of PM Monitoring at Age 16'          PMmon3 1 agec18*PMmon3 -2 agec18*agec18*PMmon3 4 / CL;
     ESTIMATE 'Effect of PM Monitoring at Age 18'          PMmon3 1 agec18*PMmon3 0  agec18*agec18*PMmon3 0 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=4, CovFewer=CovAttOnly, CovMore=CovPMBP);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=risky, PredFewer=PredAttOnly, PredMore=PredPMBP);
 
TITLE1 'Eq 9.1: Predicting Quadratic Change in Risky Behavior';
TITLE2 'From Monitoring at Age 18 as Between-Person Monitoring';
PROC MIXED DATA=work.Chapter9 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL risky = agec18 agec18*agec18 att4 agec18*att4
                   Age18mon3 agec18*Age18mon3 agec18*agec18*Age18mon3
                    / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=Pred18BP;
     RANDOM INTERCEPT agec18 / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=Cov18BP;
     ESTIMATE 'Effect of Age 18 Monitoring at Age 12'      Age18mon3 1 agec18*Age18mon3 -6 agec18*agec18*Age18mon3 36 / CL;
     ESTIMATE 'Effect of Age 18 Monitoring at Age 14'      Age18mon3 1 agec18*Age18mon3 -4 agec18*agec18*Age18mon3 16 / CL;
     ESTIMATE 'Effect of Age 18 Monitoring at Age 16'      Age18mon3 1 agec18*Age18mon3 -2 agec18*agec18*Age18mon3 4 / CL;
     ESTIMATE 'Effect of Age 18 Monitoring at Age 18'      Age18mon3 1 agec18*Age18mon3 0  agec18*agec18*Age18mon3 0 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=4, CovFewer=CovAttOnly, CovMore=Cov18BP);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=risky, PredFewer=PredAttOnly, PredMore=Pred18BP);
 
TITLE1 'Eq 9.2: Adding Within-Person Monitoring by Quadratic Age';
TITLE2 'Using Deviation from Person Mean Monitoring as Within-Person Monitoring';
PROC MIXED DATA=work.Chapter9 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL risky = agec18 agec18*agec18 att4 agec18*att4
                   PMmon3 agec18*PMmon3 agec18*agec18*PMmon3
                   WPmon agec18*WPmon agec18*agec18*WPmon
                    / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredPMBPWP;
     RANDOM INTERCEPT agec18 / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovPMBPWP;
     ESTIMATE 'Effect of PM Monitoring at Age 12'          PMmon3 1 agec18*PMmon3 -6 agec18*agec18*PMmon3 36 / CL;
     ESTIMATE 'Effect of PM Monitoring at Age 14'          PMmon3 1 agec18*PMmon3 -4 agec18*agec18*PMmon3 16 / CL;
     ESTIMATE 'Effect of PM Monitoring at Age 16'          PMmon3 1 agec18*PMmon3 -2 agec18*agec18*PMmon3 4 / CL;
     ESTIMATE 'Effect of PM Monitoring at Age 18'          PMmon3 1 agec18*PMmon3 0  agec18*agec18*PMmon3 0 / CL;
     ESTIMATE 'Effect of WP Monitoring at Age 12'          WPmon 1 agec18*WPmon -6 agec18*agec18*WPmon 36 / CL;
     ESTIMATE 'Effect of WP Monitoring at Age 14'          WPmon 1 agec18*WPmon -4 agec18*agec18*WPmon 16 / CL;
     ESTIMATE 'Effect of WP Monitoring at Age 16'          WPmon 1 agec18*WPmon -2 agec18*agec18*WPmon 4 / CL;
     ESTIMATE 'Effect of WP Monitoring at Age 18'          WPmon 1 agec18*WPmon 0  agec18*agec18*WPmon 0 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=4, CovFewer=CovPMBP, CovMore=CovPMBPWP);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=risky, PredFewer=PredPMBP, PredMore=PredPMBPWP);
 
TITLE1 'Eq 9.2: Adding Within-Person Monitoring by Quadratic Age';
TITLE2 'Using Change from Age 18 Monitoring as Within-Person Monitoring';
PROC MIXED DATA=work.Chapter9 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL risky = agec18 agec18*agec18 att4 agec18*att4
                   Age18mon3 agec18*Age18mon3 agec18*agec18*Age18mon3
                   Change18mon agec18*Change18mon agec18*agec18*Change18mon
                    / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=Pred18BPWP;
     RANDOM INTERCEPT agec18 / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=Cov18BPWP;
     ESTIMATE 'Effect of Age 18 Monitoring at Age 12'           Age18mon3 1 agec18*Age18mon3 -6 agec18*agec18*Age18mon3 36 / CL;
     ESTIMATE 'Effect of Age 18 Monitoring at Age 14'           Age18mon3 1 agec18*Age18mon3 -4 agec18*agec18*Age18mon3 16 / CL;
     ESTIMATE 'Effect of Age 18 Monitoring at Age 16'           Age18mon3 1 agec18*Age18mon3 -2 agec18*agec18*Age18mon3 4 / CL;
     ESTIMATE 'Effect of Age 18 Monitoring at Age 18'           Age18mon3 1 agec18*Age18mon3 0  agec18*agec18*Age18mon3 0 / CL;
     ESTIMATE 'Effect of Change in Monitoring at Age 12'        Change18mon 1 agec18*Change18mon -6 agec18*agec18*Change18mon 36 / CL;
     ESTIMATE 'Effect of Change in Monitoring at Age 14'        Change18mon 1 agec18*Change18mon -4 agec18*agec18*Change18mon 16 / CL;
     ESTIMATE 'Effect of Change in Monitoring at Age 16'        Change18mon 1 agec18*Change18mon -2 agec18*agec18*Change18mon 4 / CL;
     ESTIMATE 'Effect of Change in Monitoring at Age 18'        Change18mon 1 agec18*Change18mon 0  agec18*agec18*Change18mon 0 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=4, CovFewer=Cov18BP, CovMore=Cov18BPWP);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=risky, PredFewer=Pred18BP, PredMore=Pred18BPWP);
 
* Create double-stacked dataset for multivariate analysis;
* Also create dummy codes to serve as intercepts;
DATA work.Chapter9doublestack; SET work.Chapter9;
DV="1risky  "; dvR=1; dvM=0; outcome=risky;   OUTPUT;
DV="2monitor"; dvR=0; dvM=1; outcome=monitor; OUTPUT;
LABEL
DV = "DV: Categorical indicator for which DV the row is for"
dvR = "dvR: Intercept 1=risky, 0=monitor"
dvM = "dvM: Intercept 0=risky, 1=monitor"
outcome = "outcome: Combined outcome variable column";
RUN;

TITLE1 'Ch 9 Eq 9.3: Multivariate Model of Risky Behavior and Monitoring';
TITLE2 'Tricking Univariate Software';
PROC MIXED DATA=work.Chapter9doublestack COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID occasion DV;
     MODEL outcome = dvR dvM dvR*agec18 dvM*agec18 dvR*agec18*agec18 
                     dvR*att4 dvR*agec18*att4
                       / NOINT SOLUTION CHISQ CL DDFM=Satterthwaite;
     RANDOM dvR dvM dvR*agec18 dvM*agec18 / G GCORR TYPE=UN SUBJECT=PersonID;
     REPEATED DV / R RCORR TYPE=UN SUBJECT=PersonID*occasion;
RUN; TITLE1; TITLE2;

TITLE1 'Ch 9: Random Linear Age Model for Monitoring';
TITLE2 'Saving Predicted Random Effects and Residuals as Data';
* SOLUTION on RANDOM line asks for predicted Us (to be saved to data);
PROC MIXED DATA=work.Chapter9 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL monitor = agec18  / SOLUTION CHISQ CL DDFM=Satterthwaite OUTP=work.MonERandom;
     RANDOM INTERCEPT agec18 / SOLUTION TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT SolutionR=work.MonURandom;
RUN; TITLE1; TITLE2;

* Save random intercept centered at 3 to new dataset (have to add fixed effect);
DATA work.Uint; SET work.MonURandom; 
     WHERE INDEX(Effect,"Intercept")>0;
     monUint=Estimate+3.0650-3; KEEP PersonID monUint; RUN;
* Save random slope to new dataset uncentered since fixed effect is near 0;
DATA work.Uage; SET work.MonURandom; 
     WHERE INDEX(Effect,"agec18")>0;
     monUage=Estimate; KEEP PersonID monUage; RUN;
* Save residuals to new dataset;
DATA work.Ures; SET work.MonERandom; 
     monEres=Resid; KEEP PersonID occasion monEres; RUN;
* Merge back into original data;
DATA work.Chapter9; MERGE work.Chapter9 work.Uint work.Uage; BY PersonID; RUN;
DATA work.Chapter9; MERGE work.Chapter9 work.Ures; BY PersonID occasion; RUN;

TITLE1 'Descriptives for Random Effects and Residuals';
PROC MEANS N MEAN VAR MIN MAX DATA=work.Chapter9; 
     VAR monUint monUage monEres; 
RUN; TITLE1;

TITLE1 'Ch 9 Eq. 9.6: Predicting Risky Behavior from Monitoring Random Effects and Residuals';
PROC MIXED DATA=work.Chapter9 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL risky = agec18 agec18*agec18 att4 agec18*att4 
                   monUint monUint*agec18 monUage monUage*agec18 monEres
                   / SOLUTION CHISQ CL DDFM=Satterthwaite OUTPM=PredMonRandom;
     RANDOM INTERCEPT agec18 / G GCORR TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitUWPasfixed;
RUN; TITLE1;
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=risky, PredFewer=PredAttOnly, PredMore=PredMonRandom);

TITLE1 'Ch 9 Eq. 9.6: Predicting Risky Behavior from Monitoring Random Effects and Residuals';
TITLE2 'Adding Random Effect of WP Monitoring Residual';
PROC MIXED DATA=work.Chapter9 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL risky = agec18 agec18*agec18 att4 agec18*att4 
                   monUint monUint*agec18 monUage monUage*agec18 monEres
                   / SOLUTION CHISQ CL DDFM=Satterthwaite;
     RANDOM INTERCEPT agec18 monEres / G GCORR TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitUWPasrandom;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitUWPasfixed, FitMore=FitUWPasrandom);

TITLE1 'Ch 9 Eq. 9.6: Predicting Risky Behavior from Monitoring Random Effects and Residuals';
TITLE2 'Adding WP Monitoring by Age';
PROC MIXED DATA=work.Chapter9 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL risky = agec18 agec18*agec18 att4 agec18*att4 
                   monUint monUint*agec18 monUage monUage*agec18 
                   monEres agec18*monEres
                   / SOLUTION CHISQ CL DDFM=Satterthwaite;
     RANDOM INTERCEPT agec18 / G GCORR TYPE=UN SUBJECT=PersonID;
RUN; TITLE1; TITLE2;

TITLE1 'Ch 9: Saving Predicted Fixed Effects and Residuals for Monitoring as Data';
* ID on CLASS creates fixed effects per ID variable;
* SolutionF saves to dataset;
PROC MIXED DATA=work.Chapter9 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL monitor = PersonID PersonID*agec18  
                     / NOINT SOLUTION CHISQ CL DDFM=Satterthwaite OUTP=work.MonEFixed;
     ODS OUTPUT SolutionF=work.MonGFixed;
RUN; TITLE1;

* Save fixed intercept centered at 3 to new dataset (have to add fixed effect);
DATA work.Gint; SET work.MonGFixed; 
     WHERE INDEX(Effect,"PersonID")>0 AND INDEX(Effect,"agec18")=0;
     monGint=Estimate-3; KEEP PersonID monGint; RUN;
* Save fixed slope to new dataset uncentered since fixed effect is near 0;
DATA work.Gage; SET work.MonGFixed; 
     WHERE INDEX(Effect,"agec18")>0;
     monGage=Estimate; KEEP PersonID monGage; RUN;
* Save residuals to new dataset;
DATA work.Gres; SET work.MonEFixed; 
     monGres=Resid; KEEP PersonID occasion monGres; RUN;
* Merge back into original data;
DATA work.Chapter9; MERGE work.Chapter9 work.Gint work.Gage; BY PersonID; RUN;
DATA work.Chapter9; MERGE work.Chapter9 work.Gres; BY PersonID occasion; RUN;

TITLE1 'Descriptives for Fixed Effects and Residuals';
PROC MEANS N MEAN VAR MIN MAX DATA=work.Chapter9; 
     VAR monGint monGage monGres; 
RUN; TITLE1;

TITLE1 'Ch 9 Eq. 9.6: Predicting Risky Behavior from Monitoring Fixed Effects and Residuals';
PROC MIXED DATA=work.Chapter9 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL risky = agec18 agec18*agec18 att4 agec18*att4 
                   monGint monGint*agec18 monGage monGage*agec18 monGres
                   / SOLUTION CHISQ CL DDFM=Satterthwaite OUTPM=PredMonFixed;
     RANDOM INTERCEPT agec18 / G GCORR TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitGWPasfixed;
RUN; TITLE1;
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=risky, PredFewer=PredAttOnly, PredMore=PredMonFixed);

TITLE1 'Ch 9 Eq. 9.6: Predicting Risky Behavior from Monitoring Fixed Effects and Residuals';
TITLE2 'Adding Random Effect of WP Monitoring Residual';
PROC MIXED DATA=work.Chapter9 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL risky = agec18 agec18*agec18 att4 agec18*att4 
                   monGint monGint*agec18 monGage monGage*agec18 monGres
                   / SOLUTION CHISQ CL DDFM=Satterthwaite;
     RANDOM INTERCEPT agec18 monGres / G GCORR TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitGWPasrandom;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitGWPasfixed, FitMore=FitGWPasrandom);

TITLE1 'Ch 9 Eq. 9.6: Predicting Risky Behavior from Monitoring Fixed Effects and Residuals';
TITLE2 'Adding WP Monitoring by Age';
PROC MIXED DATA=work.Chapter9 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL risky = agec18 agec18*agec18 att4 agec18*att4 
                   monGint monGint*agec18 monGage monGage*agec18 
                   monGres agec18*monGres
                   / SOLUTION CHISQ CL DDFM=Satterthwaite;
     RANDOM INTERCEPT agec18 / G GCORR TYPE=UN SUBJECT=PersonID;
RUN; TITLE1; TITLE2;
****** END CHAPTER 9 MODELS ******;

* Close output directory;
ODS HTML CLOSE;

