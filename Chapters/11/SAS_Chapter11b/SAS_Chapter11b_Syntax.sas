* Output options: all can be turned on/off by adding or removing the NO;
* page number, date, centering, or page breaks, page length and width;
OPTIONS NOnumber NOdate NOcenter FormDlim=' ' PageSize=MAX LineSize=MAX;
* Eliminate SAS default titles and names of tables in output (TRACE ON to show);
TITLE; ODS TRACE OFF; ODS LISTING CLOSE;

***********************************************************************************;
*******   MACRO PROGRAMS TO AUTOMATE CALCULATIONS FOR CHAPTER 11b EXAMPLE   *******;
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
*******             BEGIN DATA MANIPULATION FOR CHAPTER 11b EXAMPLE         *******;
*******                 CHANGE "filesave" to your directory                 *******;
***********************************************************************************;

* Defining global variable for file location to be replaced in code below;
%LET filesave= C:\Dropbox\PilesOfVariance\Chapter11b\SAS;
* Location for SAS files for these models (uses macro variable filesave);
LIBNAME filesave "&filesave.";

* Import chapter 11b multivariate data in work library;
* Also make student mean variable;
DATA work.Chapter11bmultiv; SET filesave.SAS_Chapter11b;
SMaggression = MEAN(OF aggression_year0 aggression_year1 aggression_year2);
LABEL SMaggression = "SMaggression: Student Mean Aggression";
RUN;

* Get class means for student and per-year variables, merge with student data;
* Year 0;
PROC SORT DATA=work.Chapter11bmultiv; BY ClassID_year0; RUN;
PROC MEANS NOPRINT DATA=work.Chapter11bmultiv; BY ClassID_year0;
     VAR girl aggression_year0;
     OUTPUT OUT=work.ClassMeansYear0
     MEAN(girl aggression_year0) = CMgirl_year0 CMaggression_year0;
RUN;
DATA work.Chapter11bmultiv; MERGE work.Chapter11bmultiv work.ClassMeansYear0; 
     BY ClassID_year0; DROP _FREQ_ _TYPE_; 
LABEL 
CMgirl_year0 = "CMgirl_year0: % Girls in Class at Year 0"
CMaggression_year0 = "CMaggression_year0: Class Mean Aggression at Year 0";
RUN;
* Year 1;
PROC SORT DATA=work.Chapter11bmultiv; BY ClassID_year1; RUN;
PROC MEANS NOPRINT DATA=work.Chapter11bmultiv; BY ClassID_year1;
     VAR girl aggression_year1;
     OUTPUT OUT=work.ClassMeansYear1
     MEAN(girl aggression_year1) = CMgirl_year1 CMaggression_year1;
RUN;
DATA work.Chapter11bmultiv; MERGE work.Chapter11bmultiv work.ClassMeansYear1; 
     BY ClassID_year1; DROP _FREQ_ _TYPE_; 
LABEL 
CMgirl_year1 = "CMgirl_year1: % Girls in Class at Year 1"
CMaggression_year1 = "CMaggression_year1: Class Mean Aggression at Year 1";
RUN;
* Year 2;
PROC SORT DATA=work.Chapter11bmultiv; BY ClassID_year2; RUN;
PROC MEANS NOPRINT DATA=work.Chapter11bmultiv; BY ClassID_year2;
     VAR girl aggression_year2;
     OUTPUT OUT=work.ClassMeansYear2
     MEAN(girl aggression_year2) = CMgirl_year2 CMaggression_year2;
RUN;
DATA work.Chapter11bmultiv; MERGE work.Chapter11bmultiv work.ClassMeansYear2; 
     BY ClassID_year2; DROP _FREQ_ _TYPE_; 
LABEL 
CMgirl_year2 = "CMgirl_year0: % Girls in Class at Year 2"
CMaggression_year2 = "CMaggression_year0: Class Mean Aggression at Year 2";
RUN;

* Stack multivariate data by year;
* Also make a copy of Class ID variables;
PROC SORT DATA=work.Chapter11bmultiv; BY StudentID; RUN;
DATA work.Chapter11b; SET work.Chapter11bmultiv;
copyClassID_year0 = ClassID_year0;
copyClassID_year1 = ClassID_year1;
copyClassID_year2 = ClassID_year2;
year=0; ClassID=copyClassID_year0; grade=grade_year0; effort=effort_year0; aggression=aggression_year0; 
        CMgirl=CMgirl_year0; CMaggression=CMaggression_year0; OUTPUT;
year=1; ClassID=copyClassID_year1; grade=grade_year1; effort=effort_year1; aggression=aggression_year1;
        CMgirl=CMgirl_year1; CMaggression=CMaggression_year1; OUTPUT;
year=2; ClassID=copyClassID_year2; grade=grade_year2; effort=effort_year2; aggression=aggression_year2;
        CMgirl=CMgirl_year2; CMaggression=CMaggression_year2; OUTPUT;
* Drop old unnecessary multivariate variables;
DROP copyClassID_year0-copyClassID_year2 grade_year0-grade_year2 effort_year0-effort_year2
     aggression_year0-aggression_year2 CMgirl_year0-CMgirl_year2 CMaggression_year0-CMaggression_year2;
LABEL
year = "year: Year of Study (0-2)"
ClassID = "ClassID: Class ID Variable"
grade = "grade: Class Grade"
effort = "effort: Teacher-Perceived Student Effort"
aggression = "aggression: Teacher-Perceived Student Aggression"
CMgirl = "CMgirl: Class Proportion of Girls"
CMaggression = "CMaggression: Class Mean Student Aggression";
RUN; 

* Create custom intercepts for time-varying group effects;
DATA work.Chapter11b; SET work.Chapter11b; 
 * Dummy codes for acute (non-transfer) effects of classrooms across time;
     IF year=0 THEN DO; aclass0=1; aclass1=0; aclass2=0; END;
ELSE IF year=1 THEN DO; aclass0=0; aclass1=1; aclass2=0; END;
ELSE IF year=2 THEN DO; aclass0=0; aclass1=0; aclass2=1; END;
LABEL 
aclass0 = "aclass0: Acute Class Effect at Year 0"
aclass1 = "aclass1: Acute Class Effect at Year 1"
aclass2 = "aclass2: Acute Class Effect at Year 2";
* Dummy codes for cumulative (transfer) effects of classrooms across time;
     IF year=0 THEN DO; tclass0=1; tclass1=0; tclass2=0; END;
ELSE IF year=1 THEN DO; tclass0=1; tclass1=1; tclass2=0; END;
ELSE IF year=2 THEN DO; tclass0=1; tclass1=1; tclass2=1; END;
LABEL 
tclass0 = "tclass0: Transfer Class Effect at Year 0"
tclass1 = "tclass1: Transfer Class Effect at Year 1"
tclass2 = "tclass2: Transfer Class Effect at Year 2";
* Setting missing classroom values to have no effect;
* Replace missing ID variables with -99 to keep in model;
IF ClassID_year0=. THEN DO; ClassID_year0=-99; aclass0=0; tclass0=0; END;
IF ClassID_year1=. THEN DO; ClassID_year1=-99; aclass1=0; tclass1=0; END;
IF ClassID_year2=. THEN DO; ClassID_year2=-99; aclass2=0; tclass2=0; END;
IF ClassID=. THEN ClassID=-99;
* Centering model predictors for analysis;
CMgirl50 = CMgirl - .50;
CMagg2 = CMaggression - 2;
SMagg2 = SMaggression - 2;
agg2 = aggression - 2;
* Piecewise year;
IF year=0 THEN DO; year01=-1; year12=0; END;
IF year=1 THEN DO; year01= 0; year12=0; END;
IF year=2 THEN DO; year01= 0; year12=1; END;
LABEL
CMgirl50 = "CMgirl50: Class Proportion of Girls (0=.50)"
CMagg2 = "CMagg2: Class Mean Student Aggression (0=2)"
SMagg2 = "SMagg2: Student Mean Aggression (0=2)"
agg2 = "agg2: Student Aggression (0=2)"
year01 = "year01: Change from Year 1 to 0"
year12 = "year12: Change from Year 1 to 2";
* Subset sample to complete cases for all predictors;
IF NMISS(year, grade, girl, CMgirl, aggression, CMaggression, effort)>0 THEN DELETE;
RUN;

***********************************************************************************;
*****               BEGIN CHAPTER 11b TIME-VARYING GROUP MODELS               *****;
***********************************************************************************;

* Open output directory to save results to;
ODS HTML FILE="&filesave.\SAS_Chapter11b_Output.html"
         (URL="SAS_Chapter11b_Output.html") STYLE=HTMLBlue;

TITLE1 "Chapter 11b: Descriptive Statistics for Student and Year-Specific Class Variables";
PROC FREQ DATA=work.Chapter11bmultiv; 
     TABLE girl ClassID_year0*grade_year0 
           ClassID_year1*grade_year1 ClassID_year2*grade_year2; 
RUN; TITLE1;

TITLE1 "Chapter 11b: Descriptive Statistics for Level-1 Time-Varying Variables";
PROC MEANS DATA=work.Chapter11b; 
     VAR CMgirl effort aggression; 
RUN; TITLE1;

TITLE1 'Ch 11b: Empty Means, Two-Level Model of Years Within Students';
TITLE2 'Predicting Teacher-Perceived Student Effort';
PROC MIXED DATA=work.Chapter11b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS StudentID;
     MODEL effort =  / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredEmpty2E;
     RANDOM INTERCEPT / V VCORR TYPE=UN SUBJECT=StudentID;
     ODS OUTPUT InfoCrit=FitEmpty2E;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11b: Saturated Means, Unstructured Variance Model';
TITLE2 'Predicting Student Effort';
PROC MIXED DATA=work.Chapter11b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS StudentID year;
     MODEL effort = year / SOLUTION CL CHISQ DDFM=Satterthwaite;
     REPEATED year / R=3 RCORR=3 TYPE=UN SUBJECT=StudentID;
     ODS OUTPUT InfoCrit=FitSatUN2E;
     LSMEANS year / DIFF=ALL CL;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11b: Piecewise Means, Random Intercept Model';
TITLE2 'Predicting Student Effort';
PROC MIXED DATA=work.Chapter11b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS StudentID;
     MODEL effort = year01 year12 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=StudentID;
     ODS OUTPUT CovParms=CovPieceRI2E InfoCrit=FitPieceRI2E;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitPieceRI2E, FitMore=FitSatUN2E);
 
TITLE1 'Eq 11b.13: Adding Fixed Effects of Year-Specific Class';
TITLE2 'Predicting Student Effort';
PROC MIXED DATA=work.Chapter11b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS StudentID ClassID_year0 ClassID_year1 ClassID_year2;
     MODEL effort = year01 year12
                    ClassID_year0*aclass0 ClassID_year1*aclass1 ClassID_year2*aclass2
                     / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=StudentID;
     ODS OUTPUT CovParms=CovClassFixedE;
RUN; TITLE1; TITLE2;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=2, CovFewer=CovPieceRI2E, CovMore=CovClassFixedE);
 
TITLE1 'Eq 11b.14: Adding Random Acute Year-Specific Class Effects';
TITLE2 'Predicting Student Effort';
PROC MIXED DATA=work.Chapter11b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS StudentID ClassID_year0 ClassID_year1 ClassID_year2;
     MODEL effort = year01 year12 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=StudentID;
     RANDOM aclass0 / TYPE=UN SUBJECT=ClassID_year0;
     RANDOM aclass1 / TYPE=UN SUBJECT=ClassID_year1;
     RANDOM aclass2 / TYPE=UN SUBJECT=ClassID_year2;
     ODS OUTPUT CovParms=CovClassAcuteE InfoCrit=FitClassAcuteE;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitPieceRI2E, FitMore=FitClassAcuteE);
 
TITLE1 'Ch 11b: Adding Random Transfer Class Effects Instead';
TITLE2 'Predicting Student Effort';
PROC MIXED DATA=work.Chapter11b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS StudentID ClassID_year0 ClassID_year1 ClassID_year2;
     MODEL effort = year01 year12 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=StudentID;
     RANDOM tclass0 / TYPE=UN SUBJECT=ClassID_year0;
     RANDOM tclass1 / TYPE=UN SUBJECT=ClassID_year1;
     RANDOM tclass2 / TYPE=UN SUBJECT=ClassID_year2;
     ODS OUTPUT InfoCrit=FitClassTransE;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitPieceRI2E, FitMore=FitClassTransE);
 
TITLE1 'Eq 11b.15: Adding Year-Specific Effects of Class Grade';
TITLE2 'Predicting Student Effort';
PROC MIXED DATA=work.Chapter11b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS StudentID ClassID_year0 ClassID_year1 ClassID_year2 grade;
     MODEL effort = year01 year12
                    grade*aclass0 grade*aclass1 grade*aclass2
                     / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=StudentID;
     RANDOM aclass0 / TYPE=UN SUBJECT=ClassID_year0;
     RANDOM aclass1 / TYPE=UN SUBJECT=ClassID_year1;
     RANDOM aclass2 / TYPE=UN SUBJECT=ClassID_year2;
     ODS OUTPUT CovParms=CovGradeE;
     ESTIMATE 'Grade 3 vs 4 at Year 0'                grade*aclass0 -1  1  0  0  0 / CL;
     ESTIMATE 'Grade 3 vs 5 at Year 0'                grade*aclass0 -1  0  1  0  0 / CL;
     ESTIMATE 'Grade 4 vs 5 at Year 0'                grade*aclass0  0 -1  1  0  0 / CL;
     ESTIMATE 'Grade 4 vs 5 at Year 1'                grade*aclass1  0 -1  1  0  0 / CL;
     ESTIMATE 'Grade 4 vs 6 at Year 1'                grade*aclass1  0 -1  0  1  0 / CL;
     ESTIMATE 'Grade 5 vs 6 at Year 1'                grade*aclass1  0  0 -1  1  0 / CL;
     ESTIMATE 'Grade 5 vs 6 at Year 2'                grade*aclass2  0  0 -1  1  0 / CL;
     ESTIMATE 'Grade 5 vs 7 at Year 2'                grade*aclass2  0  0 -1  0  1 / CL;
     ESTIMATE 'Grade 6 vs 7 at Year 2'                grade*aclass2  0  0  0 -1  1 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=5, CovFewer=CovClassAcuteE, CovMore=CovGradeE);
 
TITLE1 'Eq 11b.16: Adding Student Gender and Year-Specific Class Contextual Gender Effects';
TITLE2 'Predicting Student Effort';
PROC MIXED DATA=work.Chapter11b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS StudentID ClassID_year0 ClassID_year1 ClassID_year2 grade;
     MODEL effort = year01 year12
                    grade*aclass0 grade*aclass1 grade*aclass2
                    girl CMgirl50*aclass0 CMgirl50*aclass1 CMgirl50*aclass2
                     / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=StudentID;
     RANDOM aclass0 / TYPE=UN SUBJECT=ClassID_year0;
     RANDOM aclass1 / TYPE=UN SUBJECT=ClassID_year1;
     RANDOM aclass2 / TYPE=UN SUBJECT=ClassID_year2;
     ODS OUTPUT CovParms=CovGirlE;
     CONTRAST 'Multivariate Test of Year-Specific Class Contextual Gender Effects' CMgirl50*aclass0 1, CMgirl50*aclass1 1, CMgirl50*aclass2 1 / CHISQ;
     ESTIMATE 'Grade 3 vs 4 at Year 0'                     grade*aclass0 -1  1  0  0  0 / CL;
     ESTIMATE 'Grade 3 vs 5 at Year 0'                     grade*aclass0 -1  0  1  0  0 / CL;
     ESTIMATE 'Grade 4 vs 5 at Year 0'                     grade*aclass0  0 -1  1  0  0 / CL;
     ESTIMATE 'Grade 4 vs 5 at Year 1'                     grade*aclass1  0 -1  1  0  0 / CL;
     ESTIMATE 'Grade 4 vs 6 at Year 1'                     grade*aclass1  0 -1  0  1  0 / CL;
     ESTIMATE 'Grade 5 vs 6 at Year 1'                     grade*aclass1  0  0 -1  1  0 / CL;
     ESTIMATE 'Grade 5 vs 6 at Year 2'                     grade*aclass2  0  0 -1  1  0 / CL;
     ESTIMATE 'Grade 5 vs 7 at Year 2'                     grade*aclass2  0  0 -1  0  1 / CL;
     ESTIMATE 'Grade 6 vs 7 at Year 2'                     grade*aclass2  0  0  0 -1  1 / CL;
     ESTIMATE 'Between-Class Gender Effect at Year 0'      girl 1 CMgirl50*aclass0 1 / CL;
     ESTIMATE 'Between-Class Gender Effect at Year 1'      girl 1 CMgirl50*aclass1 1 / CL;
     ESTIMATE 'Between-Class Gender Effect at Year 2'      girl 1 CMgirl50*aclass2 1 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=5, CovFewer=CovGradeE, CovMore=CovGirlE);
 
TITLE1 'Ch 11b: Empty Means, Two-Level Model of Years Within Students';
TITLE2 'Predicting Teacher-Perceived Student Aggression';
PROC MIXED DATA=work.Chapter11b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS StudentID;
     MODEL aggression =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / V VCORR TYPE=UN SUBJECT=StudentID;
     ODS OUTPUT InfoCrit=FitEmpty2A;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11b: Saturated Means, Unstructured Variance Model';
TITLE2 'Predicting Student Aggression';
PROC MIXED DATA=work.Chapter11b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS StudentID year;
     MODEL aggression = year / SOLUTION CL CHISQ DDFM=Satterthwaite;
     REPEATED year / R=3 RCORR=3 TYPE=UN SUBJECT=StudentID;
     ODS OUTPUT InfoCrit=FitSatUN2A;
     LSMEANS year / DIFF=ALL CL;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11b: Piecewise Means, Random Intercept Model';
TITLE2 'Predicting Student Aggression';
PROC MIXED DATA=work.Chapter11b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS StudentID;
     MODEL aggression = year01 year12 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=StudentID;
     ODS OUTPUT CovParms=CovPieceRI2A InfoCrit=FitPieceRI2A;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitPieceRI2A, FitMore=FitSatUN2A);
 
TITLE1 'Ch 11b: Adding Random Acute Year-Specific Class Effects';
TITLE2 'Predicting Student Aggression';
PROC MIXED DATA=work.Chapter11b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS StudentID ClassID_year0 ClassID_year1 ClassID_year2;
     MODEL aggression = year01 year12 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=StudentID;
     RANDOM aclass0 / TYPE=UN SUBJECT=ClassID_year0;
     RANDOM aclass1 / TYPE=UN SUBJECT=ClassID_year1;
     RANDOM aclass2 / TYPE=UN SUBJECT=ClassID_year2;
     ODS OUTPUT CovParms=CovClassAcuteA InfoCrit=FitClassAcuteA;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitPieceRI2A, FitMore=FitClassAcuteA);
 
TITLE1 'Ch 11b: Adding Random Transfer Class Effects Instead';
TITLE2 'Predicting Student Aggression';
PROC MIXED DATA=work.Chapter11b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS StudentID ClassID_year0 ClassID_year1 ClassID_year2;
     MODEL aggression = year01 year12 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=StudentID;
     RANDOM tclass0 / TYPE=UN SUBJECT=ClassID_year0;
     RANDOM tclass1 / TYPE=UN SUBJECT=ClassID_year1;
     RANDOM tclass2 / TYPE=UN SUBJECT=ClassID_year2;
     ODS OUTPUT InfoCrit=FitClassTransA;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitPieceRI2A, FitMore=FitClassTransA);
 
TITLE1 'Ch 11b: Adding Time-Varying, Student Mean, and Year-Specific Class Contextual Effects of Student Aggression';
TITLE2 'Predicting Student Effort';
PROC MIXED DATA=work.Chapter11b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS StudentID ClassID_year0 ClassID_year1 ClassID_year2 grade;
     MODEL effort = year01 year12
                    grade*aclass0 grade*aclass1 grade*aclass2
                    girl CMgirl50*aclass0 CMgirl50*aclass1 CMgirl50*aclass2
                    agg2 SMagg2 CMagg2*aclass0 CMagg2*aclass1 CMagg2*aclass2
                     / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredFinalE;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=StudentID;
     RANDOM aclass0 / TYPE=UN SUBJECT=ClassID_year0;
     RANDOM aclass1 / TYPE=UN SUBJECT=ClassID_year1;
     RANDOM aclass2 / TYPE=UN SUBJECT=ClassID_year2;
     ODS OUTPUT CovParms=CovAggE;
     CONTRAST 'Multivariate Test of Year-Specific Class Contextual Aggression Effects' CMagg2*aclass0 1, CMagg2*aclass1 1, CMagg2*aclass2 1 / CHISQ;
     ESTIMATE 'Grade 3 vs 4 at Year 0'                          grade*aclass0 -1  1  0  0  0 / CL;
     ESTIMATE 'Grade 3 vs 5 at Year 0'                          grade*aclass0 -1  0  1  0  0 / CL;
     ESTIMATE 'Grade 4 vs 5 at Year 0'                          grade*aclass0  0 -1  1  0  0 / CL;
     ESTIMATE 'Grade 4 vs 5 at Year 1'                          grade*aclass1  0 -1  1  0  0 / CL;
     ESTIMATE 'Grade 4 vs 6 at Year 1'                          grade*aclass1  0 -1  0  1  0 / CL;
     ESTIMATE 'Grade 5 vs 6 at Year 1'                          grade*aclass1  0  0 -1  1  0 / CL;
     ESTIMATE 'Grade 5 vs 6 at Year 2'                          grade*aclass2  0  0 -1  1  0 / CL;
     ESTIMATE 'Grade 5 vs 7 at Year 2'                          grade*aclass2  0  0 -1  0  1 / CL;
     ESTIMATE 'Grade 6 vs 7 at Year 2'                          grade*aclass2  0  0  0 -1  1 / CL;
     ESTIMATE 'Between-Class Gender Effect at Year 0'           girl 1 CMgirl50*aclass0 1 / CL;
     ESTIMATE 'Between-Class Gender Effect at Year 1'           girl 1 CMgirl50*aclass1 1 / CL;
     ESTIMATE 'Between-Class Gender Effect at Year 2'           girl 1 CMgirl50*aclass2 1 / CL;
     ESTIMATE 'Between-Class Aggression Effect at Year 0'       agg2 1 SMagg2 1 CMagg2*aclass0 1 / CL;
     ESTIMATE 'Between-Class Aggression Effect at Year 1'       agg2 1 SMagg2 1 CMagg2*aclass1 1 / CL;
     ESTIMATE 'Between-Class Aggression Effect at Year 2'       agg2 1 SMagg2 1 CMagg2*aclass2 1 / CL;
     ESTIMATE 'Between-Student Aggression Effect'               agg2 1 SMagg2 1 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=5, CovFewer=CovGirlE, CovMore=CovAggE);
     %PseudoR2(NCov=5, CovFewer=CovClassAcuteE, CovMore=CovAggE);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=effort, PredFewer=PredEmpty2E, PredMore=PredFinalE);
 
****** END CHAPTER 11b MODELS ******;

* Close output directory;
ODS HTML CLOSE;

