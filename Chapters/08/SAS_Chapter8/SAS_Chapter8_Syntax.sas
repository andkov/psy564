* Output options: all can be turned on/off by adding or removing the NO;
* page number, date, centering, or page breaks, page length and width;
OPTIONS NOnumber NOdate NOcenter FormDlim=' ' PageSize=MAX LineSize=MAX;
* Eliminate SAS default titles and names of tables in output (TRACE ON to show);
TITLE; ODS TRACE OFF; ODS LISTING CLOSE;

***********************************************************************************;
*******   MACRO PROGRAMS TO AUTOMATE CALCULATIONS FOR CHAPTER 8 EXAMPLE     *******;
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
* DV = case-sensitive name of dependent variable;
* PredFewer = name of dataset of predicted outcomes for nested model;
* PredMore = name of dataset of predicted outcomes for comparison model;
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
*******             BEGIN DATA MANIPULATION FOR CHAPTER 8 EXAMPLE           *******;
*******                 CHANGE "filesave" to your directory                 *******;
***********************************************************************************;

* Defining global variable for file location to be replaced in code below;
%LET filesave= C:\Dropbox\PilesOfVariance\Chapter8\SAS;
* Location for SAS files for these models (uses macro variable filesave);
LIBNAME filesave "&filesave.";

* Import chapter 8 stacked data into work library;
DATA work.Chapter8; SET filesave.SAS_Chapter8; RUN;

* Create person means across time;
PROC SORT DATA=work.Chapter8; BY PersonID session; RUN;
PROC MEANS NOPRINT DATA=work.Chapter8; BY PersonID;
     VAR women baseage mood stressor;
     OUTPUT OUT=work.PersonMeans8 
     MEAN(women baseage mood stressor) = PMwomen PMbaseage PMmood PMstressor;
RUN;

* Merge person means back into original data;
DATA work.Chapter8; 
MERGE work.Chapter8 work.PersonMeans8(DROP= _TYPE_ _FREQ_ PMwomen PMbaseage); 
BY PersonID;
LABEL   
PMmood = "PMmood: Person Mean Negative Mood"
PMstressor = "PMstressor: Person Mean Stressors";
* Create centered predictors for analysis;
age80 = baseage - 80;
mood2 = mood - 2;
WPmood = mood - PMmood;
PMmood2 = PMmood - 2;
WPstressor = stressor - PMstressor;
PMstressor40 = PMstressor - .40;
LABEL
age80 = "age80: Baseline Age (0=80)"
mood2 = "mood2: Daily Negative Mood (0=2)"
WPmood = "WPmood: Within-Person Negative Mood (0=PM)"
PMmood2 = "PMmood2: Person Mean Negative Mood (0=2)"
WPstressor = "WPstressor: Within-Person Stressors (0=PM)"
PMstressor40 = "PMstressor40: Mean #Days with Stressor (0=0.40)";
* Subset sample to complete cases for all predictors;
IF NMISS(women, baseage, symptoms, mood, stressor, mood)>0 THEN DELETE;
* Subset sample to study days within a two-week period;
IF studyday>14 THEN DELETE;
RUN; 

***********************************************************************************;
*******                         BEGIN CHAPTER 8 MODELS                      *******;
***********************************************************************************;

* Open output directory to save results to;
ODS HTML FILE="&filesave.\SAS_Chapter8_Output.html"
         (URL="SAS_Chapter8_Output.html") STYLE=HTMLBlue;

TITLE1 "Chapter 8: Descriptive Statistics for Time-Invariant Variables";
PROC MEANS DATA=work.PersonMeans8; 
     VAR PMwomen PMbaseage PMmood PMstressor; 
RUN; TITLE1;

TITLE1 "Chapter 8: Descriptive Statistics for Time-Varying Variables";
PROC MEANS DATA=work.Chapter8;                  
     VAR symptoms mood stressor WPmood WPstressor; 
RUN; TITLE1;

TITLE1 'Eq 8.1: Conditional Baseline Model';
TITLE2 'Just Sex and Age in the Model for the Means';
PROC MIXED DATA=work.Chapter8 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL symptoms = women age80 women*age80 / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredSexAge;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovSexAge InfoCrit=FitSexAge;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 8: Empty Means, Random Intercept Model for Negative Mood Predictor';
PROC MIXED DATA=work.Chapter8 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL mood =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / V VCORR TYPE=UN SUBJECT=PersonID;
RUN; TITLE1;
 
TITLE1 'Eq 8.3: Adding BP and WP Effects of Negative Mood to the Model for the Means';
TITLE2 'Using Person-Mean-Centering for Negative Mood';
PROC MIXED DATA=work.Chapter8 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL symptoms = women age80 women*age80
                      PMmood2 WPmood
                       / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredBPWPMood;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovBPWPMood InfoCrit=FitBPWPMood;
     CONTRAST 'Multivariate Test of BP and WP Negative Mood Fixed Effects' PMmood2 1, WPmood 1 / CHISQ;
     ESTIMATE 'Age Slope for Men'                               age80 1 women*age80 0 / CL;
     ESTIMATE 'Age Slope for Women'                             age80 1 women*age80 1 / CL;
     ESTIMATE 'Contextual Negative Mood Effect'                 WPmood -1 PMmood2 1 / CL;
     ESTIMATE 'Predicted Symptoms for PMmood=2 WPmood=-1'       intercept 1 PMmood2 0 WPmood -1 / CL;
     ESTIMATE 'Predicted Symptoms for PMmood=2 WPmood=0'        intercept 1 PMmood2 0 WPmood 0 / CL;
     ESTIMATE 'Predicted Symptoms for PMmood=2 WPmood=1'        intercept 1 PMmood2 0 WPmood 1 / CL;
     ESTIMATE 'Predicted Symptoms for PMmood=3 WPmood=-1'       intercept 1 PMmood2 1 WPmood -1 / CL;
     ESTIMATE 'Predicted Symptoms for PMmood=3 WPmood=0'        intercept 1 PMmood2 1 WPmood 0 / CL;
     ESTIMATE 'Predicted Symptoms for PMmood=3 WPmood=1'        intercept 1 PMmood2 1 WPmood 1 / CL;
     ESTIMATE 'Predicted Symptoms for PMmood=1 WPmood=0'        intercept 1 PMmood2 -1 WPmood 0 / CL;
     ESTIMATE 'Predicted Symptoms for PMmood=2 WPmood=0'        intercept 1 PMmood2 0 WPmood 0 / CL;
     ESTIMATE 'Predicted Symptoms for PMmood=3 WPmood=0'        intercept 1 PMmood2 1 WPmood 0 / CL;
     ESTIMATE 'Predicted Symptoms for PMmood=4 WPmood=0'        intercept 1 PMmood2 2 WPmood 0 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitSexAge, FitMore=FitBPWPMood);
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=2, CovFewer=CovSexAge, CovMore=CovBPWPMood);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=symptoms, PredFewer=PredSexAge, PredMore=PredBPWPMood);
 
TITLE1 'Eq 8.5: Adding Smushed Effect of Negative Mood to the Model for the Means';
TITLE2 'Using Grand-Mean-Centering for Negative Mood';
PROC MIXED DATA=work.Chapter8 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL symptoms = women age80 women*age80
                      mood2
                       / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovSmushMood;
     ESTIMATE 'Age Slope for Men'                age80 1 women*age80 0 / CL;
     ESTIMATE 'Age Slope for Women'              age80 1 women*age80 1 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=2, CovFewer=CovSexAge, CovMore=CovSmushMood);
 
TITLE1 'Eq 8.7: Adding a Contextual Effect of Negative Mood to the Model for the Means';
TITLE2 'Using Person-Mean-Centering for Negative Mood';
PROC MIXED DATA=work.Chapter8 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL symptoms = women age80 women*age80
                      mood2 PMmood2
                       / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredBPWPMood2;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovBPWPMood InfoCrit=FitBPWPMood;
     ESTIMATE 'Age Slope for Men'                     age80 1 women*age80 0 / CL;
     ESTIMATE 'Age Slope for Women'                   age80 1 women*age80 1 / CL;
     ESTIMATE 'Between-Person Negative Mood Effect'   mood2 1 PMmood2 1 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=2, CovFewer=CovSmushMood, CovMore=CovBPWPMood);
     %PseudoR2(NCov=2, CovFewer=CovSexAge, CovMore=CovBPWPMood);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=symptoms, PredFewer=PredSexAge, PredMore=PredBPWPMood2);
 
TITLE1 'Eq 8.9: Adding a Random Effect of Negative Mood to the Model for the Variance';
TITLE2 'Using Person-Mean-Centering for Negative Mood';
PROC MIXED DATA=work.Chapter8 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL symptoms = women age80 women*age80
                      PMmood2 WPmood
                       / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT WPmood / G GCORR TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitRandPMC;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitBPWPMood, FitMore=FitRandPMC);
 
TITLE1 'Eq 8.9: Adding a Random Effect of Negative Mood to the Model for the Variance';
TITLE2 'Using Grand-Mean-Centering for Negative Mood';
PROC MIXED DATA=work.Chapter8 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL symptoms = women age80 women*age80
                      mood2 PMmood2
                       / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT mood2 / G GCORR TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitRandGMC;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitBPWPMood, FitMore=FitRandGMC);
 
TITLE1 'Eq 8.10: Adding Contextual and WP Effects of Stressors to the Model for the Means';
TITLE2 'Using Person-Mean-Centering for Negative Mood, Grand-Mean-Centering for Stressors';
PROC MIXED DATA=work.Chapter8 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL symptoms = women age80 women*age80
                      PMmood2 WPmood PMstressor40 stressor
                       / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredBPWPStressor;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovBPWPStressor InfoCrit=FitBPWPStressor;
     CONTRAST 'Multivariate Test of Stressor Fixed Effects' PMstressor40 1, stressor 1 / CHISQ;
     ESTIMATE 'Intercept for Women'                             intercept 1 women 1 / CL;
     ESTIMATE 'Age Slope for Women'                             age80 1 women*age80 1 / CL;
     ESTIMATE 'Contextual Negative Mood Effect'                 WPmood -1 PMmood2 1 / CL;
     ESTIMATE 'Between-Person Stressor Effect'                  stressor 1 PMstressor40 1 / CL;
     ESTIMATE 'Contextual Negative Mood Effect Per Day'         PMmood2 .2 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitBPWPMood, FitMore=FitBPWPStressor);
     %FitTest(FitFewer=FitSexAge, FitMore=FitBPWPStressor);
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=2, CovFewer=CovBPWPMood, CovMore=CovBPWPStressor);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=symptoms, PredFewer=PredBPWPMood, PredMore=PredBPWPStressor);
     %TotalR2(DV=symptoms, PredFewer=PredSexAge, PredMore=PredBPWPStressor);
 
TITLE1 'Ch 8: Adding Random WP Effect of Stressors to the Model for the Variance';
TITLE2 'Using Person-Mean-Centering for Negative Mood, Grand-Mean-Centering for Stressors';
PROC MIXED DATA=work.Chapter8 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL symptoms = women age80 women*age80
                      PMmood2 WPmood PMstressor40 stressor
                       / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT stressor / G GCORR TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitBPWPStressorR;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitBPWPStressor, FitMore=FitBPWPStressorR);
 
TITLE1 'Eq 8.11: Adding 4 Sex*Negative Mood and Sex*Stressors Interactions to the Model for the Means';
TITLE2 'Using Person-Mean-Centering for Negative Mood, Grand-Mean-Centering for Stressors';
PROC MIXED DATA=work.Chapter8 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL symptoms = women age80 women*age80
                      PMmood2 WPmood PMstressor40 stressor
                      women*PMmood2 women*WPmood women*PMstressor40 women*stressor
                       / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredSex4;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitSex4;
     CONTRAST 'Multivariate Test of 4 Sex Interactions' women*PMmood2 1, women*WPmood 1, women*PMstressor40 1, women*stressor 1 / CHISQ;
     ESTIMATE 'Intercept: Men'                             intercept 1 women 0 / CL;
     ESTIMATE 'Intercept: Women'                           intercept 1 women 1 / CL;
     ESTIMATE 'Intercept: Sex Diff'                        women 1 / CL;
     ESTIMATE 'Age Slope: Men'                             age80 1 women*age80 0 / CL;
     ESTIMATE 'Age Slope: Women'                           age80 1 women*age80 1 / CL;
     ESTIMATE 'Age Slope: Sex Diff'                        women*age80 1 / CL;
     ESTIMATE 'BP Negative Mood: Men'                      PMmood2 1 women*PMmood2 0 / CL;
     ESTIMATE 'BP Negative Mood: Women'                    PMmood2 1 women*PMmood2 1 / CL;
     ESTIMATE 'BP Negative Mood: Sex Diff'                 women*PMmood2 1 / CL;
     ESTIMATE 'Contextual Negative Mood: Men'              WPmood -1 women*WPmood  0 PMmood2 1 women*PMmood2 0 / CL;
     ESTIMATE 'Contextual Negative Mood: Women'            WPmood -1 women*WPmood -1 PMmood2 1 women*PMmood2 1 / CL;
     ESTIMATE 'Contextual Negative Mood: Sex Diff'         women*WPmood -1 women*PMmood2 1 / CL;
     ESTIMATE 'WP Negative Mood: Men'                      WPmood 1 women*WPmood 0 / CL;
     ESTIMATE 'WP Negative Mood: Women'                    WPmood 1 women*WPmood 1 / CL;
     ESTIMATE 'WP Negative Mood: Sex Diff'                 women*WPmood 1 / CL;
     ESTIMATE 'BP Stressors: Men'                          stressor 1 women*stressor 0 PMstressor40 1 women*PMstressor40 0 / CL;
     ESTIMATE 'BP Stressors: Women'                        stressor 1 women*stressor 1 PMstressor40 1 women*PMstressor40 1 / CL;
     ESTIMATE 'BP Stressors: Sex Diff'                     women*stressor 1 women*PMstressor40 1 / CL;
     ESTIMATE 'Contextual Stressors: Men'                  PMstressor40 1 women*PMstressor40 0 / CL;
     ESTIMATE 'Contextual Stressors: Women'                PMstressor40 1 women*PMstressor40 1 / CL;
     ESTIMATE 'Contextual Stressors: Sex Diff'             women*PMstressor40 1 / CL;
     ESTIMATE 'WP Stressors: Men'                          stressor 1 women*stressor 0 / CL;
     ESTIMATE 'WP Stressors: Women'                        stressor 1 women*stressor 1 / CL;
     ESTIMATE 'WP Stressors: Sex Diff'                     women*stressor 1 / CL;
     ESTIMATE 'Contextual Stressors per Day'               PMstressor40 .2 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitBPWPStressor, FitMore=FitSex4);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=symptoms, PredFewer=PredBPWPStressor, PredMore=PredSex4);
 
TITLE1 'Eq 8.11reduced: Adding Only 2 Sex*Stressors Interactions to the Model for the Means';
TITLE2 'Using Person-Mean-Centering for Negative Mood, Grand-Mean-Centering for Stressors';
PROC MIXED DATA=work.Chapter8 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL symptoms = women age80 women*age80
                      PMmood2 WPmood PMstressor40 stressor
                      women*PMstressor40 women*stressor
                       / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredSex2;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitSex2;
     CONTRAST 'Multivariate Test of 2 Sex Interactions' women*PMstressor40 1, women*stressor 1 / CHISQ;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitBPWPStressor, FitMore=FitSex2);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=symptoms, PredFewer=PredBPWPStressor, PredMore=PredSex2);
 
TITLE1 'Eq 8.12: Adding 4 Negative Mood*Stressors Interactions to the Model for the Means';
TITLE2 'Using Person-Mean-Centering for Negative Mood, Grand-Mean-Centering for Stressors';
PROC MIXED DATA=work.Chapter8 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL symptoms = women age80 women*age80
                      PMmood2 WPmood PMstressor40 stressor
                      women*PMstressor40 women*stressor
                      PMmood2*stressor PMmood2*PMstressor40 WPmood*stressor WPmood*PMstressor40
                       / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredMoodStressor4;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitMoodStressor4;
     CONTRAST 'Multivariate Test of 4 Negative Mood*Stressors Interactions' PMmood2*stressor 1, PMmood2*PMstressor40 1, WPmood*stressor 1, WPmood*PMstressor40 1 / CHISQ;
     ESTIMATE 'Intercept: Men'                                       intercept 1 women 0 / CL;
     ESTIMATE 'Intercept: Women'                                     intercept 1 women 1 / CL;
     ESTIMATE 'Intercept: Sex Diff'                                  women 1 / CL;
     ESTIMATE 'Age Slope: Men'                                       age80 1 women*age80 0 / CL;
     ESTIMATE 'Age Slope: Women'                                     age80 1 women*age80 1 / CL;
     ESTIMATE 'Age Slope: Sex Diff'                                  women*age80 1 / CL;
     ESTIMATE 'BP Negative Mood'                                     PMmood2 1 / CL;
     ESTIMATE 'Contextual Negative Mood'                             WPmood -1 PMmood2 1 / CL;
     ESTIMATE 'WP Negative Mood'                                     WPmood 1 / CL;
     ESTIMATE 'BP Stressors: Men'                                    stressor 1 women*stressor 0 PMstressor40 1 women*PMstressor40 0 / CL;
     ESTIMATE 'BP Stressors: Women'                                  stressor 1 women*stressor 1 PMstressor40 1 women*PMstressor40 1 / CL;
     ESTIMATE 'BP Stressors: Sex Diff'                               women*stressor 1 women*PMstressor40 1 / CL;
     ESTIMATE 'Contextual Stressors: Men'                            PMstressor40 1 women*PMstressor40 0 / CL;
     ESTIMATE 'Contextual Stressors: Women'                          PMstressor40 1 women*PMstressor40 1 / CL;
     ESTIMATE 'Contextual Stressors: Sex Diff'                       women*PMstressor40 1 / CL;
     ESTIMATE 'WP Stressors: Men'                                    stressor 1 women*stressor 0 / CL;
     ESTIMATE 'WP Stressors: Women'                                  stressor 1 women*stressor 1 / CL;
     ESTIMATE 'WP Stressors: Sex Diff'                               women*stressor 1 / CL;
     ESTIMATE 'Contextual Stressors per Day'                         PMstressor40 .2 / CL;
     ESTIMATE 'BP Negative Mood by BP Stressors'                     PMmood2*stressor 1 PMmood2*PMstressor40 1 / CL;
     ESTIMATE 'BP Negative Mood by Contextual Stressors'             PMmood2*PMstressor40 1 / CL;
     ESTIMATE 'BP Negative Mood by WP Stressors'                     PMmood2*stressor 1 / CL;
     ESTIMATE 'Contextual Negative Mood by BP Stressors'             WPmood*stressor -1 PMmood2*stressor 1 WPmood*PMstressor40 -1 PMmood2*PMstressor40 1 / CL;
     ESTIMATE 'Contextual Negative Mood by Contextual Stressors'     WPmood*PMstressor40 -1 PMmood2*PMstressor40 1 / CL;
     ESTIMATE 'Contextual Negative Mood by WP Stressors'             WPmood*stressor -1 PMmood2*stressor 1 / CL;
     ESTIMATE 'WP Negative Mood by BP Stressors'                     WPmood*stressor 1 WPmood*PMstressor40 1 / CL;
     ESTIMATE 'WP Negative Mood by Contextual Stressors'             WPmood*PMstressor40 1 / CL;
     ESTIMATE 'WP Negative Mood by WP Stressors'                     WPmood*stressor 1 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitSex2, FitMore=FitMoodStressor4);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=symptoms, PredFewer=PredSex2, PredMore=PredMoodStressor4);
 
TITLE1 'Ch 8: Checking Inter-Variable Interactions via PMC Negative Mood by PMC Stressors';
PROC MIXED DATA=work.Chapter8 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL symptoms = women age80 women*age80
                      PMmood2 WPmood PMstressor40 WPstressor
                      women*PMstressor40 women*WPstressor
                      PMmood2*WPstressor PMmood2*PMstressor40 WPmood*WPstressor WPmood*PMstressor40
                       / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     ESTIMATE 'BP Negative Mood by BP Stressors'                     PMmood2*PMstressor40 1 / CL;
     ESTIMATE 'BP Negative Mood by Contextual Stressors'             PMmood2*WPstressor -1 PMmood2*PMstressor40 1 / CL;
     ESTIMATE 'BP Negative Mood by WP Stressors'                     PMmood2*WPstressor 1 / CL;
     ESTIMATE 'Contextual Negative Mood by BP Stressors'             WPmood*PMstressor40 -1 PMmood2*PMstressor40 1 / CL;
     ESTIMATE 'Contextual Negative Mood by Contextual Stressors'     WPmood*PMstressor40 -1 PMmood2*PMstressor40 1 PMmood2*WPstressor -1 WPmood*WPstressor 1 / CL;
     ESTIMATE 'Contextual Negative Mood by WP Stressors'             WPmood*WPstressor -1 PMmood2*WPstressor 1 / CL;
     ESTIMATE 'WP Negative Mood by BP Stressors'                     WPmood*PMstressor40 1 / CL;
     ESTIMATE 'WP Negative Mood by Contextual Stressors'             WPmood*WPstressor -1 WPmood*PMstressor40 1 / CL;
     ESTIMATE 'WP Negative Mood by WP Stressors'                     WPmood*WPstressor 1 / CL;
RUN; TITLE1;
 
TITLE1 'Ch 8: Checking Inter-Variable Interactions via GMC Negative Mood by GMC Stressors';
PROC MIXED DATA=work.Chapter8 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL symptoms = women age80 women*age80
                      PMmood2 mood2 PMstressor40 stressor
                      women*PMstressor40 women*stressor
                      PMmood2*stressor PMmood2*PMstressor40 mood2*stressor mood2*PMstressor40
                       / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     ESTIMATE 'BP Negative Mood by BP Stressors'                     mood2*PMstressor40 1 PMmood2*PMstressor40 1 stressor*PMmood2 1 mood2*stressor 1 / CL;
     ESTIMATE 'BP Negative Mood by Contextual Stressors'             mood2*PMstressor40 1 PMmood2*PMstressor40 1 / CL;
     ESTIMATE 'BP Negative Mood by WP Stressors'                     PMmood2*stressor 1 mood2*stressor 1 / CL;
     ESTIMATE 'Contextual Negative Mood by BP Stressors'             PMmood2*stressor 1 PMmood2*PMstressor40 1 / CL;
     ESTIMATE 'Contextual Negative Mood by Contextual Stressors'     PMmood2*PMstressor40 1 / CL;
     ESTIMATE 'Contextual Negative Mood by WP Stressors'             PMmood2*stressor 1 / CL;
     ESTIMATE 'WP Negative Mood by BP Stressors'                     mood2*PMstressor40 1 mood2*stressor 1 / CL;
     ESTIMATE 'WP Negative Mood by Contextual Stressors'             mood2*PMstressor40 1 / CL;
     ESTIMATE 'WP Negative Mood by WP Stressors'                     mood2*stressor 1 / CL;
RUN; TITLE1;
 
TITLE1 'Ch 8: Checking Inter-Variable Interactions via GMC Negative Mood by PMC Stressors';
PROC MIXED DATA=work.Chapter8 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL symptoms = women age80 women*age80
                      PMmood2 mood2 PMstressor40 WPstressor
                      women*PMstressor40 women*WPstressor
                      PMmood2*WPstressor PMmood2*PMstressor40 mood2*WPstressor mood2*PMstressor40
                       / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     ESTIMATE 'BP Negative Mood by BP Stressors'                     mood2*PMstressor40 1 PMmood2*PMstressor40 1 / CL;
     ESTIMATE 'BP Negative Mood by Contextual Stressors'             PMmood2*WPstressor -1 mood2*WPstressor -1 mood2*PMstressor40 1 PMmood2*PMstressor40 1 / CL;
     ESTIMATE 'BP Negative Mood by WP Stressors'                     PMmood2*WPstressor 1 mood2*WPstressor 1 / CL;
     ESTIMATE 'Contextual Negative Mood by BP Stressors'             PMmood2*PMstressor40 1 / CL;
     ESTIMATE 'Contextual Negative Mood by Contextual Stressors'     PMmood2*WPstressor -1 PMmood2*PMstressor40 1 / CL;
     ESTIMATE 'Contextual Negative Mood by WP Stressors'             PMmood2*WPstressor 1 / CL;
     ESTIMATE 'WP Negative Mood by BP Stressors'                     mood2*PMstressor40 1 / CL;
     ESTIMATE 'WP Negative Mood by Contextual Stressors'             mood2*WPstressor -1 mood2*PMstressor40 1 / CL;
     ESTIMATE 'WP Negative Mood by WP Stressors'                     mood2*WPstressor 1 / CL;
RUN; TITLE1;
 
TITLE1 'Eq 8.13: Adding 5 Intra-Variable Interactions to the Model for the Means';
TITLE2 'Using Person-Mean-Centering for Negative Mood, Grand-Mean-Centering for Stressors';
PROC MIXED DATA=work.Chapter8 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL symptoms = women age80 women*age80
                      PMmood2 WPmood PMstressor40 stressor
                      women*PMstressor40 women*stressor
                      PMmood2*PMmood2 WPmood*WPmood PMmood2*WPmood PMstressor40*PMstressor40 PMstressor40*stressor
                       / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredIntra5;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitIntra5;
     CONTRAST 'Multivariate Test of 5 Intra-Variable Interactions' PMmood2*PMmood2 1, WPmood*WPmood 1, PMmood2*WPmood 1, PMstressor40*PMstressor40 1, PMstressor40*stressor 1 / CHISQ;
     ESTIMATE 'BP Negative Mood: Linear'                   PMmood2 1 / CL;
     ESTIMATE 'BP Negative Mood: Quadratic'                PMmood2*PMmood2 1 / CL;
     ESTIMATE 'Contextual Negative Mood: Linear'           WPmood -1 PMmood2 1 / CL;
     ESTIMATE 'Contextual Negative Mood: Quadratic'        PMmood2*WPmood -1 PMmood2*PMmood2 1 WPmood*WPmood 1 / CL;
     ESTIMATE 'WP Negative Mood: Linear'                   WPmood 1 / CL;
     ESTIMATE 'WP Negative Mood: Quadratic'                WPmood*WPmood 1 / CL;
     ESTIMATE 'BP by WP Negative Mood'                     PMmood2*WPmood 1 / CL;
     ESTIMATE 'Contextual by WP Negative Mood'             PMmood2*WPmood 1 WPmood*WPmood -2 / CL;
     ESTIMATE 'BP Stressor: Linear'                        PMstressor40 1 stressor 1 / CL;
     ESTIMATE 'BP Stressor: Quadratic'                     PMstressor40*PMstressor40 1 PMstressor40*stressor 1 / CL;
     ESTIMATE 'Contextual Stressors: Linear'               PMstressor40 1 / CL;
     ESTIMATE 'Contextual Stressors: Quadratic'            PMstressor40*PMstressor40 1 / CL;
     ESTIMATE 'WP Stressors: Linear'                       stressor 1 / CL;
     ESTIMATE 'BP/Contextual by WP Stressors'              PMstressor40*stressor 1 / CL;
     ESTIMATE 'BP Negative Mood: Linear at 1'              PMmood2 1 PMmood2*PMmood2 -2 / CL;
     ESTIMATE 'BP Negative Mood: Linear at 3'              PMmood2 1 PMmood2*PMmood2 2 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitSex2, FitMore=FitIntra5);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=symptoms, PredFewer=PredSex2, PredMore=PredIntra5);
 
TITLE1 'Eq 8.13reduced: Baseline Homogeneous Variance Model with Significant Fixed Effects Only';
TITLE2 'Using Person-Mean-Centering for Negative Mood, Grand-Mean-Centering for Stressors';
PROC MIXED DATA=work.Chapter8 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL symptoms = women age80 women*age80
                      PMmood2 WPmood PMstressor40 stressor
                      women*PMstressor40 women*stressor
                      PMmood2*PMmood2 PMmood2*WPmood
                       / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 8.13reduced: Baseline Homogeneous Variance Model with Significant Fixed Effects Only';  
PROC NLMIXED DATA=work.Chapter8 METHOD=GAUSS TECH=NEWRAP GCONV=1e-12;  
* Must define all parameters to be estimated and provide start values;
* First three lines are fixed effects, fourth line is variances;
  PARMS fint=2.45 fsex=-.33 fage=.07 fsexage=-.09 
        fbpmood=-.34 fwpmood=.26 fcstress=1.93 fwpstress=.22
        f2sexstress=-1.33 f1sexstress=-.18 fbpmood2=-1.78 fbpwpmood=.26
        varEint=-.50 vU0int=-.47;
* Setting up level-2 equations;
  b0i = fint + fsex*women + fage*age80 + fsexage*women*age80 + fbpmood*PMmood2 + fcstress*PMstressor40 
         + f2sexstress*PMstressor40*women + fbpmood2*PMmood2*PMmood2 + U0i;
  b1i = fwpmood + fbpwpmood*PMmood2;
  b2i = fwpstress + f1sexstress*women;
* Setting up level-1 equation WITHOUT level-1 residual;
  y = (b0i) + (b1i*WPmood) + (b2i*stressor);
* Model for heterogeneous variance;
  varE = EXP(varEint);
  vU0  = EXP(vU0int);
* Symptoms is normally distributed with a mean of 'y' and a variance of 'VarE';
  MODEL symptoms ~ normal(y,varE);  
* Defining random effects: normally distributed with means and variances;
  RANDOM U0i ~ normal([0],[vU0]) SUBJECT=PersonID;  
* Requesting additional model-implied estimates;
  ESTIMATE 'Level-1 Residual Variance'         EXP(varEint);
  ESTIMATE 'Level-2 Random Intercept Variance' EXP(vU0int);
RUN; TITLE1; 

TITLE1 'Eq 8.14: Add Random Intercept Variance Heterogeneity by Sex, Age, L2 Negative Mood, and L2 Stressors';  
PROC NLMIXED DATA=work.Chapter8 METHOD=GAUSS TECH=NEWRAP GCONV=1e-12;  
* Must define all parameters to be estimated and provide start values;
* First three lines are fixed effects, fourth line is variances;
  PARMS fint=2.45 fsex=-.33 fage=.07 fsexage=-.09 
        fbpmood=-.34 fwpmood=.26 fcstress=1.93 fwpstress=.22
        f2sexstress=-1.33 f1sexstress=-.18 fbpmood2=-1.78 fbpwpmood=.26
        varEint=-.50 vU0int=-.47 vU0sex=0 vU0age=0 vU0bpmood=0 vU0bpstress=0;
* Setting up level-2 equations;
  b0i = fint + fsex*women + fage*age80 + fsexage*women*age80 + fbpmood*PMmood2 + fcstress*PMstressor40 
         + f2sexstress*PMstressor40*women + fbpmood2*PMmood2*PMmood2 + U0i;
  b1i = fwpmood + fbpwpmood*PMmood2;
  b2i = fwpstress + f1sexstress*women;
* Setting up level-1 equation WITHOUT level-1 residual;
  y = (b0i) + (b1i*WPmood) + (b2i*stressor);
* Model for heterogeneous variance;
  varE = EXP(varEint);
  vU0  = EXP(vU0int + vU0sex*women + vU0age*age80 + vU0bpmood*PMmood2 + vU0bpstress*PMstressor40);
* Symptoms is normally distributed with a mean of 'y' and a variance of 'VarE';
  MODEL symptoms ~ normal(y,varE);  
* Defining random effects: normally distributed with means and variances;
  RANDOM U0i ~ normal([0],[vU0]) SUBJECT=PersonID;  
* Requesting additional model-implied estimates or quantities;
  ESTIMATE 'Level-1 Residual Variance'         EXP(varEint);
  ESTIMATE 'Level-2 Random Intercept Variance' EXP(vU0int);
  CONTRAST 'Multivariate Test of 4 Predictors of Random Intercept Variance Heterogeneity' 
            vU0sex*1, vU0age*1, vU0bpmood*1, vU0bpstress*1;
RUN; TITLE1; 

TITLE1 'Eq 8.15: Add Residual Variance Heterogeneity by Sex, Age, Both Negative Mood, and Both Stressors';  
PROC NLMIXED DATA=work.Chapter8 METHOD=GAUSS TECH=NEWRAP GCONV=1e-12;  
* Must define all parameters to be estimated and provide start values;
* First three lines are fixed effects, fourth and fifth lines are variances;
  PARMS fint=2.45 fsex=-.33 fage=.07 fsexage=-.09 
        fbpmood=-.34 fwpmood=.26 fcstress=1.93 fwpstress=.22
        f2sexstress=-1.33 f1sexstress=-.18 fbpmood2=-1.78 fbpwpmood=.26
        varEint=-.50 varEsex=0 varEage=0 varEbpmood=0 varEbpstress=0 varEwpmood=0 varEwpstress=0
        vU0int=-.47 vU0sex=0 vU0age=0 vU0bpmood=0 vU0bpstress=0;
* Setting up level-2 equations;
  b0i = fint + fsex*women + fage*age80 + fsexage*women*age80 + fbpmood*PMmood2 + fcstress*PMstressor40 
         + f2sexstress*PMstressor40*women + fbpmood2*PMmood2*PMmood2 + U0i;
  b1i = fwpmood + fbpwpmood*PMmood2;
  b2i = fwpstress + f1sexstress*women;
* Setting up level-1 equation WITHOUT level-1 residual;
  y = (b0i) + (b1i*WPmood) + (b2i*stressor);
* Model for heterogeneous variance;
  varE = EXP(varEint + varEsex*women + varEage*age80 + varEbpmood*PMmood2 + varEbpstress*PMstressor40 
              + varEwpmood*WPmood + varEwpstress*stressor);
  vU0  = EXP(vU0int + vU0sex*women + vU0age*age80 + vU0bpmood*PMmood2 + vU0bpstress*PMstressor40);
* Symptoms is normally distributed with a mean of 'y' and a variance of 'VarE';
  MODEL symptoms ~ normal(y,varE);  
* Defining random effects: normally distributed with means and variances;
  RANDOM U0i ~ normal([0],[vU0]) SUBJECT=PersonID;  
* Requesting additional model-implied estimates or quantities;
  ESTIMATE 'Level-1 Residual Variance'         EXP(varEint);
  ESTIMATE 'Level-2 Random Intercept Variance' EXP(vU0int);
  CONTRAST 'Multivariate Test of 6 Predictors of Residual Variance Heterogeneity' 
            varEsex*1, varEage*1, varEbpmood*1, varEbpstress*1, varEwpmood*1, varEwpstress*1;
RUN; TITLE1; 

TITLE1 'Ch 8: Final Heterogeneous Variance Model';
PROC NLMIXED DATA=work.Chapter8 METHOD=GAUSS TECH=NEWRAP GCONV=1e-12;  
* Must define all parameters to be estimated and provide start values;
* First three lines are fixed effects, fourth and fifth lines are variances;
  PARMS fint=2.45 fsex=-.33 fage=.07 fsexage=-.09 
        fbpmood=1.66 fwpmood=.26 fcstress=1.93 fwpstress=.22 
        f2sexstress=-1.33 f1sexstress=-.18 
        varEint=-.50 varEbpmood=0 varEbpstress=0 
        vU0int=-.47 vU0bpstress=0;
* Setting up level-2 equations -- note that random intercept was moved to below;
  b0i = fint + fsex*women + fage*age80 + fsexage*women*age80 + fbpmood*PMmood2 + fcstress*PMstressor40 
         + f2sexstress*PMstressor40*women;
  b1i = fwpmood;
  b2i = fwpstress + f1sexstress*women;
* Setting up level-1 equation WITHOUT level-1 residual;
  yfixed = (b0i) + (b1i*WPmood) + (b2i*stressor);
  y = yfixed + U0i;
* Model for heterogeneous variance;
  varE = EXP(varEint + varEbpmood*PMmood2 + varEbpstress*PMstressor40);
  vU0  = EXP(vU0int + vU0bpstress*PMstressor40);
* Symptoms is normally distributed with a mean of 'y' and a variance of 'VarE';
  MODEL symptoms ~ normal(y,varE);  
* Defining random effects: normally distributed with means and variances;
  RANDOM U0i ~ normal([0],[vU0]) SUBJECT=PersonID;  
* Asking for predicted values;
  PREDICT yfixed OUT=PredFinal;
* Requesting additional model-implied estimates or quantities;
  ESTIMATE 'Intercept: Men'                 fint;
  ESTIMATE 'Intercept: Women'               fint + fsex;
  ESTIMATE 'Intercept: Sex Diff'            fsex;
  ESTIMATE 'Age Slope: Men'                 fage;
  ESTIMATE 'Age Slope: Women'               fage + fsexage;
  ESTIMATE 'Age Slope: Sex Diff'            fsexage;
  ESTIMATE 'BP Negative Mood'               fbpmood;
  ESTIMATE 'Contextual Negative Mood'       fbpmood - fwpmood;
  ESTIMATE 'WP Negative Mood'               fwpmood;
  ESTIMATE 'BP Stress: Men'                 fcstress + fwpstress;
  ESTIMATE 'BP Stress: Women'               fcstress + f2sexstress + fwpstress + f1sexstress;
  ESTIMATE 'BP Stress: Sex Diff'            f2sexstress + f1sexstress;
  ESTIMATE 'Contextual Stressors: Men'      fcstress;
  ESTIMATE 'Contextual Stressors: Women'    fcstress + f2sexstress;
  ESTIMATE 'Contextual Stressors: Sex Diff' f2sexstress;
  ESTIMATE 'WP Stressors: Men'              fwpstress;
  ESTIMATE 'WP Stressors: Women'            fwpstress + f1sexstress;
  ESTIMATE 'WP Stressors: Sex Diff'         f1sexstress;
  ESTIMATE 'Contextual Stressors per Day: Men'    fcstress/5;
  ESTIMATE 'Contextual Stressors per Day: Women' (fcstress + f2sexstress)/5;
 * For Figure 8.3;
  ESTIMATE 'Men, PMstressor=20%, Stressor=0' 
  fint + fsex*0 + fcstress*-.2 + f2sexstress*0 + fwpstress*0 + f1sexstress*0;
  ESTIMATE 'Men, PMstressor=20%, Stressor=1' 
  fint + fsex*0 + fcstress*-.2 + f2sexstress*0 + fwpstress*1 + f1sexstress*0;
  ESTIMATE 'Men, PMstressor=80%, Stressor=0' 
  fint + fsex*0 + fcstress*.4 + f2sexstress*0 + fwpstress*0 + f1sexstress*0;
  ESTIMATE 'Men, PMstressor=80%, Stressor=1' 
  fint + fsex*0 + fcstress*.4 + f2sexstress*0 + fwpstress*1 + f1sexstress*0;
  ESTIMATE 'Women, PMstressor=20%, Stressor=0' 
  fint + fsex*1 + fcstress*-.2 + f2sexstress*-.2 + fwpstress*0 + f1sexstress*0;
  ESTIMATE 'Women, PMstressor=20%, Stressor=1' 
  fint + fsex*1 + fcstress*-.2 + f2sexstress*-.2 + fwpstress*1 + f1sexstress*1;
  ESTIMATE 'Women, PMstressor=80%, Stressor=0' 
  fint + fsex*1 + fcstress*.4 + f2sexstress*.4 + fwpstress*0 + f1sexstress*0;
  ESTIMATE 'Women, PMstressor=80%, Stressor=1' 
  fint + fsex*1 + fcstress*.4 + f2sexstress*.4 + fwpstress*1 + f1sexstress*1; 
RUN; PROC CORR DATA=PredFinal; VAR symptoms pred; RUN; TITLE1;
****** END CHAPTER 8 MODELS ******;

* Close output directory;
ODS HTML CLOSE;

