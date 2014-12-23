* Output options: all can be turned on/off by adding or removing the NO;
* page number, date, centering, or page breaks, page length and width;
OPTIONS NOnumber NOdate NOcenter FormDlim=' ' PageSize=MAX LineSize=MAX;
* Eliminate SAS default titles and names of tables in output (TRACE ON to show);
TITLE; ODS TRACE OFF; ODS LISTING CLOSE;

***********************************************************************************;
*******   MACRO PROGRAMS TO AUTOMATE CALCULATIONS FOR CHAPTER 11a EXAMPLE   *******;
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
*******    BEGIN DATA MANIPULATION FOR CHAPTER 11a THREE-LEVEL EXAMPLE      *******;
*******                 CHANGE "filesave" to your directory                 *******;
***********************************************************************************;

* Defining global variable for file location to be replaced in code below;
%LET filesave= C:\Dropbox\PilesOfVariance\Chapter11a\SAS;
* Location for SAS files for these models (uses macro variable filesave);
LIBNAME filesave "&filesave.";

* Import chapter 11a multivariate data in work library;
* Also make student mean variables;
DATA work.Chapter11amultiv; SET filesave.SAS_Chapter11a;
SMclose  = MEAN(OF  close_wave1  close_wave2  close_wave3);
SMvictim = MEAN(OF victim_wave1 victim_wave2 victim_wave3);
LABEL 
SMclose =  "SMclose: Student Mean Closeness"
SMvictim = "SMvictim: Student Mean Victimization";
RUN;

* Get class means for student and per-wave variables (keeping level-3 variables);
PROC SORT DATA=work.Chapter11amultiv; BY ClassID StudentID; RUN;
PROC MEANS NOPRINT DATA=work.Chapter11amultiv; BY ClassID classsize grade;
     VAR SMclose SMvictim girl emosup_wave1 emosup_wave2 emosup_wave3;
     OUTPUT OUT=work.ClassMeans11a
     MEAN(SMclose SMvictim girl emosup_wave1 emosup_wave2 emosup_wave3) 
        = CMclose CMvictim CMgirl CMemosup_wave1 CMemosup_wave2 CMemosup_wave3;
RUN;

* Make class mean emosup and add labels;
DATA work.ClassMeans11a; SET work.ClassMeans11a;
CMemosup = MEAN(OF CMemosup_wave1 CMemosup_wave2 CMemosup_wave3);
LABEL
CMemosup = "CMemosup: Class Mean Emotional Support"
CMclose = "CMclose: Class Mean Student-Teacher Closeness"
CMvictim = "CMvictim3: Class Mean Student-Perceived Victimization"
CMgirl = "CMgirl: Percentage Girls in Class";
* Create level-3 variables for analysis;
size23 = classsize - 23;
IF grade=3 THEN grade35=0;
IF grade=5 THEN grade35=1;
CMgirl50 = CMgirl - .50;
CMemosup5 = CMemosup - 5;
CMvictim3 = CMvictim - 3;
LABEL
size23 = "size23: # Students in Class (0=23)"
grade35 = "grade35: Grade 3=0, Grade5=1"
CMgirl50 = "CMgirl50: Percentage Girls in Class (0=.50)"
CMemosup5 = "CMemosup5: Class Mean Emotional Support (0=5)"
CMvictim3 = "CMvictim3: Class Mean Student-Perceived Victimization (0=3)";
RUN;

* Merge class means into multivariate data;
DATA work.Chapter11amultiv; MERGE work.Chapter11amultiv work.ClassMeans11a;
BY ClassID classsize grade; 
DROP _TYPE_ _FREQ_ emosup_wave1-emosup_wave3;
* Create level-2 variables for analysis;
WCclose = SMclose - CMclose;
SMvictim3 = SMvictim - 3;
WCvictim = SMvictim - CMvictim;
LABEL
WCclose = "WCclose: Within-Class Student-Teacher Closeness (0=CM)"
SMvictim3 = "SMvictim3: Student Mean Student-Perceived Victimization (0=3)"
WCvictim = "WCvictim: Within-Class Student-Perceived Victimization (0=CM)";
RUN;

* Stack multivariate data by wave;
DATA work.Chapter11a; SET work.Chapter11amultiv;
wave=1; emosup=CMemosup_wave1; victim=victim_wave1; close=close_wave1; OUTPUT;
wave=2; emosup=CMemosup_wave2; victim=victim_wave2; close=close_wave2; OUTPUT;
wave=3; emosup=CMemosup_wave3; victim=victim_wave3; close=close_wave3; OUTPUT;
* Drop old unnecessary multivariate variables;
DROP CMemosup_wave1-CMemosup_wave3 victim_wave1-victim_wave3 close_wave1-close_wave3;
LABEL
wave = "wave: Wave of Study (1-3)"
emosup = "emosup: Time-Varying Class Emotional Support"
victim = "victim: Time-Varying Student-Perceived Victimization"
close = "close: Time-Varying Student-Teacher Closeness";
RUN;

* Sort data by level and create level-1 variables for analysis;
PROC SORT DATA=work.Chapter11a; BY ClassID StudentID wave; RUN;
DATA work.Chapter11a; SET work.Chapter11a;
time1 = wave - 1;
time2 = wave - 2;
IF wave<3 THEN w3=0; ELSE IF wave=3 THEN w3=1;
WSclose = close - SMclose;
victim3 = victim - 3;
WSvictim = victim - SMvictim;
emosup5 = emosup - 5;
WCemosup = emosup - CMemosup;
LABEL
time1 = "time1: Time in Study (0=Wave1)"
time2 = "time2: Time in Study (0=Wave2)"
w3 = "w3: Is Wave3 (0=no, 1=yes)"
WSclose = "WSclose: Within-Student Student-Teacher Closeness (0=SM)"
victim3 = "victim3: Time-Varying Student-Perceived Victimization (0=3)"
WSvictim = "WSvictim: Within-Student Student-Perceived Victimization (0=SM)"
emosup5 = "emosup5: Time-Varying Class Emotional Support (0=5)"
WCemosup = "WCemosup: Within-Class Emotional Support (0=CM)";
* Subset sample to complete cases for all predictors;
IF NMISS(wave, victim, emosup, girl, classsize, close)>0 THEN DELETE;
RUN;

* Get class by wave dataset for later;
PROC SORT DATA=work.Chapter11a; BY ClassID wave; RUN;
PROC MEANS NOPRINT DATA=work.Chapter11a; BY ClassID wave;
     VAR emosup WCemosup; 
     OUTPUT OUT=work.ClassPerWave11a
     MEAN(emosup WCemosup) = emosup WCemosup;
RUN;
* Add level-1 time;
DATA work.ClassPerWave11a; SET work.ClassPerWave11a;
DROP _TYPE_ _FREQ_; 
time1 = wave - 1;
LABEL time1 = "time1: Time in Study (0=Wave1)";
RUN;

***********************************************************************************;
*****       BEGIN CHAPTER 11a TIME-INVARIANT GROUP THREE-LEVEL MODELS         *****;
***********************************************************************************;

* Open output directory to save results to;
ODS HTML FILE="&filesave.\SAS_Chapter11a_Output.html"
         (URL="SAS_Chapter11a_Output.html") STYLE=HTMLBlue;

TITLE1 "Chapter 11a: Descriptive Statistics for Level-1 Time-Varying Student Variables";
PROC MEANS N MEAN STDDEV VAR MIN MAX DATA=work.Chapter11a; 
     VAR close WSclose victim WSvictim; 
RUN; TITLE1;

TITLE1 "Chapter 11a: Descriptive Statistics for Level-1 Time-Varying Class Variables";
PROC MEANS N MEAN STDDEV VAR MIN MAX DATA=work.ClassPerWave11a; 
     VAR emosup WCemosup; 
RUN; TITLE1;

TITLE1 "Chapter 11a: Descriptive Statistics for Level-2 Student Variables";
PROC MEANS N MEAN STDDEV VAR MIN MAX DATA=work.Chapter11amultiv; 
     VAR SMclose WCclose SMvictim WCvictim girl; 
RUN; TITLE1;

TITLE1 "Chapter 11a: Descriptive Statistics for Level-3 Class Variables";
PROC MEANS N MEAN STDDEV VAR MIN MAX DATA=work.ClassMeans11a; 
     VAR CMclose CMvictim CMemosup CMgirl classsize grade35; 
RUN; 

TITLE1 'Ch 11a: Empty Means, Two-Level Model Predicting Student-Teacher Closeness';
TITLE2 'Occasions Within Students*Classes';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT InfoCrit=FitEmpty2C;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 11a.1: Empty Means, Three-Level Model Predicting Student-Teacher Closeness';
TITLE2 'Level-1 Occasions Within Level-2 Students Within Level-3 Classes';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close =  / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredEmpty3C;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT InfoCrit=FitEmpty3C;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitEmpty2C, FitMore=FitEmpty3C);
 
TITLE1 'Ch 11a: Empty Means, Two-Level Model Predicting Student-Perceived Victimization';
TITLE2 'Occasions Within Students*Classes';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL victim =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT InfoCrit=FitEmpty2V;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 11a.1: Empty Means, Three-Level Model Predicting Student-Perceived Victimization';
TITLE2 'Level-1 Occasions Within Level-2 Students Within Level-3 Classes';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL victim =  / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredEmpty3V;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT InfoCrit=FitEmpty3V;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitEmpty2V, FitMore=FitEmpty3V);
 
TITLE1 'Eq 11a.1: Empty Means, Levels 1 and 3 Model Predicting Class Emotional Support';
TITLE2 'Level-1 Ocasions Within Level-3 Classes';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL emosup =  / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredEmpty3E;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ClassID;
     ODS OUTPUT InfoCrit=FitEmpty3E;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 11a.1: Empty Means, Levels 1 and 3 Model Predicting Class Emotional Support';
TITLE2 'Level-1 Ocasions Within Level-3 Classes';
PROC MIXED DATA=work.ClassPerWave11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID;
     MODEL emosup =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ClassID;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 11a.3: Saturated Means, Unstructured Level-3 and Level-2 Variances';
TITLE2 'Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID wave;
     MODEL close = wave / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM wave / G GCORR TYPE=UN SUBJECT=ClassID;
     REPEATED wave / R RCORR TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT InfoCrit=FitSatUNC;
     LSMEANS wave / DIFF=ALL CL;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11a: Piecewise Means, Level-3 and Level-2 Random Intercepts';
TITLE2 'Three-Level Model Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 w3 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT InfoCrit=FitPieceRI2RI3C;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11a: Piecewise Means, Add Level-2 Random Time Slope';
TITLE2 'Three-Level Model Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 w3 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT InfoCrit=FitPieceRL2RI3C;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitPieceRI2RI3C, FitMore=FitPieceRL2RI3C);
 
TITLE1 'Ch 11a: Piecewise Means, Add Level-3 Random Time Slope';
TITLE2 'Three-Level Model Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 w3 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT InfoCrit=FitPieceRL2RL3C;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitPieceRL2RI3C, FitMore=FitPieceRL2RL3C);
     %FitTest(FitFewer=FitPieceRL2RL3C, FitMore=FitSatUNC);
 
TITLE1 'Eq 11a.4: Unconditional Growth Model Predicting Student Closeness';
TITLE2 'Random Linear Time Slopes for Level-2 Students and Level-3 Classes';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredUncC;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT CovParms=CovUncC;
RUN; TITLE1; TITLE2;
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=close, PredFewer=PredEmpty3C, PredMore=PredUncC);
 
TITLE1 'Ch 11a: Add Class Grade and Class Size';
TITLE2 'Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 grade35 time1*grade35 size23 time1*size23 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID*StudentID;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11a: Add Class Size Only';
TITLE2 'Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 size23 time1*size23 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT CovParms=CovSizeC;
RUN; TITLE1; TITLE2;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=7, CovFewer=CovUncC, CovMore=CovSizeC);
 
TITLE1 'Eq 11a.5: Add Student and Class Gender';
TITLE2 'Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 size23 time1*size23
                   girl time1*girl CMgirl50 time1*CMgirl50
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT CovParms=CovGirlC InfoCrit=FitGirlC;
     CONTRAST 'Multivariate Test of Level-2 Within-Class Gender Effects' girl 1, time1*girl 1 / CHISQ;
     CONTRAST 'Multivariate Test of Level-3 Contextual Class Gender Effects' CMgirl50 1, time1*CMgirl50 1 / CHISQ;
     ESTIMATE 'Contextual Gender Effect at Wave 1'         CMgirl50 1 CMgirl50*time1 0 / CL;
     ESTIMATE 'Contextual Gender Effect at Wave 2'         CMgirl50 1 CMgirl50*time1 1 / CL;
     ESTIMATE 'Contextual Gender Effect at Wave 3'         CMgirl50 1 CMgirl50*time1 2 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=7, CovFewer=CovSizeC, CovMore=CovGirlC);
 
TITLE1 'Ch 11a: Test if Level-3 Random Time Slope Variance is still needed';
TITLE2 'Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 size23 time1*size23
                   girl time1*girl CMgirl50 time1*CMgirl50
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT InfoCrit=NoRandTime3C;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=NoRandTime3C, FitMore=FitGirlC);
 
TITLE1 'Ch 11a: Add Random Gender Effect across Classes';
TITLE2 'Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 size23 time1*size23
                   girl time1*girl CMgirl50 time1*CMgirl50
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 girl / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT InfoCrit=FitRandGirlC;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitGirlC, FitMore=FitRandGirlC);
 
TITLE1 'Ch 11a: Saturated Means, Unstructured Model Predicting Class Emotional Support';
TITLE2 'Using Only Level-1 Ocasions Within Level-3 Classes';
PROC MIXED DATA=work.ClassPerWave11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID wave;
     MODEL emosup = wave / SOLUTION CL CHISQ DDFM=Satterthwaite;
     REPEATED wave / R RCORR TYPE=UN SUBJECT=ClassID;
     ODS OUTPUT InfoCrit=FitSatUNE;
     LSMEANS wave / DIFF=ALL CL;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11a: Saturated Means, Random Intercept Model Predicting Class Emotional Support';
TITLE2 'Using Only Level-1 Ocasions Within Level-3 Classes';
PROC MIXED DATA=work.ClassPerWave11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID wave;
     MODEL emosup = wave / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ClassID;
     ODS OUTPUT InfoCrit=FitSatRIE;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitSatRIE, FitMore=FitSatUNE);
 
TITLE1 'Ch 11a: Saturated Means, Random Time Slope Model Predicting Class Emotional Support';
TITLE2 'Using Only Level-1 Ocasions Within Level-3 Classes';
PROC MIXED DATA=work.ClassPerWave11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID wave;
     MODEL emosup = wave / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 11a.6: Add Time-Varying and Class Emotional Support';
TITLE2 'Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 size23 time1*size23
                   girl time1*girl CMgirl50 time1*CMgirl50
                   emosup5 time1*emosup5 CMemosup5 time1*CMemosup5
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT CovParms=CovEmosup13C InfoCrit=FitEmosup13C;
     CONTRAST 'Multivariate Test of Level-1 Within-Class Emotional Support Effects' emosup5 1, time1*emosup5 1 / CHISQ;
     CONTRAST 'Multivariate Test of Level-3 Contextual Class Emotional Support Effects' CMemosup5 1, time1*CMemosup5 1 / CHISQ;
RUN; TITLE1; TITLE2;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=7, CovFewer=CovGirlC, CovMore=CovEmosup13C);
 
TITLE1 'Ch 11a: Remove Class Size, Add Random Level-1 Emotional Support across Classes';
TITLE2 'Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   emosup5 time1*emosup5 CMemosup5 time1*CMemosup5
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 emosup5 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT InfoCrit=FitEmosup13RC;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11a: New Baseline for Pseudo-R2 without Class Size Effects';
TITLE2 'Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT CovParms=FitGirlNewC;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11a: Add Just Level-3 Emotional Support Effects';
TITLE2 'Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT CovParms=CovEmosup3C InfoCrit=FitEmosup3C;
     CONTRAST 'Multivariate Test of Level-3 Between-Class Emotional Support Effects' CMemosup5 1, time1*CMemosup5 1 / CHISQ;
     ESTIMATE 'Between-Class Emotional Support Effect at Wave 1'     CMemosup5 1 CMemosup5*time1 0 / CL;
     ESTIMATE 'Between-Class Emotional Support Effect at Wave 2'     CMemosup5 1 CMemosup5*time1 1 / CL;
     ESTIMATE 'Between-Class Emotional Support Effect at Wave 3'     CMemosup5 1 CMemosup5*time1 2 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=7, CovFewer=FitGirlNewC, CovMore=CovEmosup3C);
 
TITLE1 'Eq 11a.3: Saturated Means, Unstructured Level-3 and Level-2 Variances';
TITLE2 'for Student Victimization';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID wave;
     MODEL victim = wave / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM wave / G GCORR TYPE=UN SUBJECT=ClassID;
     REPEATED wave / R RCORR TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT InfoCrit=FitSatUNV;
     LSMEANS wave / DIFF=ALL CL;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11a: Piecewise Means, Level-3 and Level-2 Random Intercepts';
TITLE2 'Three-Level Model Predicting Student Victimization';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL victim = time1 w3 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT InfoCrit=FitPieceRI2RI3V;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitPieceRI2RI3V, FitMore=FitSatUNV);
 
TITLE1 'Ch 11a: Piecewise Means, Add Level-2 Random Time Slope';
TITLE2 'Three-Level Model Predicting Student Victimization';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL victim = time1 w3 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT InfoCrit=FitPieceRL2RI3V;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitPieceRI2RI3V, FitMore=FitPieceRL2RI3V);
 
TITLE1 'Ch 11a: Piecewise Means, Add Level-3 Random Time Slope';
TITLE2 'Three-Level Model Predicting Student Victimization';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL victim = time1 w3 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT InfoCrit=FitPieceRL2RL3V;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitPieceRL2RI3V, FitMore=FitPieceRL2RL3V);
     %FitTest(FitFewer=FitPieceRL2RL3V, FitMore=FitSatUNV);
 
TITLE1 'Ch 11a: Fixed Linear Time, Level-3 and Level-2 Random Intercepts';
TITLE2 'Three-Level Model Predicting Student Victimization';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL victim = time1 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=ClassID*StudentID;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11a: Add All 3 Main Effects of Student Vicitimization Predicting Student Closeness';
TITLE2 'Using Variable-Centered Level-1 and Level-2 Victim Predictors';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5
                   WSvictim WCvictim CMvictim3
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT CovParms=CovVicVBC InfoCrit=FitVicVBC;
     ESTIMATE 'Level-1 Within-Student Victim Effect'                 WSvictim 1 / CL;
     ESTIMATE 'Level-2 Within-Class   Victim Effect'                 WCvictim 1 / CL;
     ESTIMATE 'Level-3 Between-Class  Victim Effect'                 CMvictim3 1 / CL;
     ESTIMATE 'Level-2 Within-Class  Contextual Victim Effect'       WSvictim -1 WCvictim 1 / CL;
     ESTIMATE 'Level-3 Between-Class Contextual Victim Effect'       WCvictim -1 CMvictim3 1 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=7, CovFewer=CovEmosup3C, CovMore=CovVicVBC);
 
TITLE1 'Ch 11a: Add All 3 Main Effects of Student Vicitimization Predicting Student Closeness';
TITLE2 'Using Constant-Centered Level-1 and Level-2 Victim Predictors';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5
                   victim3 SMvictim3 CMvictim3
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT CovParms=CovVicCBC;
     ESTIMATE 'Level-1 Within-Student Victim Effect'                 victim3 1 / CL;
     ESTIMATE 'Level-2 Within-Class   Victim Effect'                 victim3 1 SMvictim3 1 / CL;
     ESTIMATE 'Level-3 Between-Class  Victim Effect'                 victim3 1 SMvictim3 1 CMvictim3 1 / CL;
     ESTIMATE 'Level-2 Within-Class  Contextual Victim Effect'       SMvictim3 1 / CL;
     ESTIMATE 'Level-3 Between-Class Contextual Victim Effect'       CMvictim3 1 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=7, CovFewer=CovEmosup3C, CovMore=CovVicCBC);
 
TITLE1 'Ch 11a: Constant-Centered Student Vicitimization Predicting Student Closeness';
TITLE2 'For Table 11.3: Level-2 and Level-3 Effects, Omitting Level-1 Effect';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5 SMvictim3 CMvictim3
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID*StudentID;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11a: Constant-Centered Student Vicitimization Predicting Student Closeness';
TITLE2 'For Table 11.3: Level-1 and Level-3 Effects Only, Omitting Level-2 Effect';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5 victim3 CMvictim3
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID*StudentID;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11a: Constant-Centered Student Vicitimization Predicting Student Closeness';
TITLE2 'For Table 11.3: Level-1 and Level-2 Effects, Omitting Level-3 Effect';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5 victim3 SMvictim3
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID*StudentID;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11a: Constant-Centered Student Vicitimization Predicting Student Closeness';
TITLE2 'For Table 11.3: Level-1 Effect Only, Omitting Level-2 and Level-3 Effects';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5 victim3
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID*StudentID;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11a: Add All 3 Victim*Time Interactions Predicting Student Closeness';
TITLE2 'Using Variable-Centered Level-1 and Level-2 Victim Predictors';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5
                   WSvictim WCvictim CMvictim3
                   time1*WSvictim time1*WCvictim time1*CMvictim3
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID*StudentID;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11a: Add All 3 Victim*Time Interactions Predicting Student Closeness';
TITLE2 'Using Constant-Centered Level-1 and Level-2 Victim Predictors';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5
                   victim3 SMvictim3 CMvictim3
                   time1*victim3 time1*SMvictim3 time1*CMvictim3
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID*StudentID;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11a: Add Random Within-Class Vicitimization Effect across Classes';
TITLE2 'Using Variable-Centered Level-1 and Level-2 Victim Predictors';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5
                   WSvictim WCvictim CMvictim3
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 WCvictim / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT InfoCrit=FitVicRWC3C;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitVicVBC, FitMore=FitVicRWC3C);
 
TITLE1 'Ch 11a: Add Random Within-Student Vicitimization Effect across Students';
TITLE2 'Using Variable-Centered Level-1 and Level-2 Victim Predictors';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5
                   WSvictim WCvictim CMvictim3
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 WSvictim / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT CovParms=CovRandVic InfoCrit=FitVicRWS2C;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitVicVBC, FitMore=FitVicRWS2C);
 
TITLE1 'Ch 11a: Add Random Within-Student Vicitimization Effect across Classes';
TITLE2 'Using Variable-Centered Level-1 and Level-2 Victim Predictors';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5
                   WSvictim WCvictim CMvictim3
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 WCvictim / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 WSvictim / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT InfoCrit=FitVicRWS23C;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitVicRWS2C, FitMore=FitVicRWS23C);
 
TITLE1 'Ch 11a: Add Quadratic Level-3 Effects of Gender';
TITLE2 'Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5 WSvictim WCvictim CMvictim3
                   CMgirl50*CMgirl50 time1*CMgirl50*CMgirl50
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 WSvictim / TYPE=UN SUBJECT=ClassID*StudentID;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11a: Add Quadratic Level-3 Effects of Emotional Support';
TITLE2 'Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5 WSvictim WCvictim CMvictim3
                   CMemosup5*CMemosup5 time1*CMemosup5*CMemosup5
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 WSvictim / TYPE=UN SUBJECT=ClassID*StudentID;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11a: Add Quadratic Effect of Victimization at Each Level';
TITLE2 'Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5 WSvictim WCvictim CMvictim3
                   CMvictim3*CMvictim3 WCvictim*WCvictim WSvictim*WSvictim
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 WSvictim / TYPE=UN SUBJECT=ClassID*StudentID;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11a: Add Two-Way and Three-Way Interactions Among Level-3 Effects';
TITLE2 'Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5 WSvictim WCvictim CMvictim3
                   CMgirl50*CMemosup5 CMgirl50*CMvictim3 CMemosup5*CMvictim3
                   time1*CMgirl50*CMemosup5 time1*CMgirl50*CMvictim3 time1*CMemosup5*CMvictim3
                   CMgirl50*CMemosup5*CMvictim3 time1*CMgirl50*CMemosup5*CMvictim3
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 WSvictim / TYPE=UN SUBJECT=ClassID*StudentID;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11a: Keep Gender*Emotional Support and Emotional Support*Victimization Level-3 Interactions';
TITLE2 'Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5 WSvictim WCvictim CMvictim3
                   CMgirl50*CMemosup5 CMemosup5*CMvictim3
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 WSvictim / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT CovParms=CovL3x;
RUN; TITLE1; TITLE2;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=10, CovFewer=CovRandVic, CovMore=CovL3x);
 
TITLE1 'Eq 11a.8: Add Level-2 Interactions (and Contextual Level-3 Interactions)';
TITLE2 'Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5 WSvictim WCvictim CMvictim3
                   CMgirl50*CMemosup5 CMemosup5*CMvictim3
                   girl*WCvictim CMgirl50*WCvictim
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 WSvictim / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT CovParms=CovL32x;
RUN; TITLE1; TITLE2;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=10, CovFewer=CovL3x, CovMore=CovL32x);
 
TITLE1 'Ch 11a: Add Time by Level-2 Interactions (and Contextual Level-3 Interactions)';
TITLE2 'Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5 WSvictim WCvictim CMvictim3
                   CMgirl50*CMemosup5 CMemosup5*CMvictim3
                   girl*WCvictim CMgirl50*WCvictim
                   time1*WCvictim time1*girl*WCvictim time1*CMvictim3 time1*CMgirl50*WCvictim
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 WSvictim / TYPE=UN SUBJECT=ClassID*StudentID;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11a: Add Level-3 by Student Gender Cross-Level Interactions';
TITLE2 'Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5 WSvictim WCvictim CMvictim3
                   CMgirl50*CMemosup5 CMemosup5*CMvictim3
                   girl*WCvictim CMgirl50*WCvictim
                   girl*CMgirl50 girl*CMemosup5 girl*CMvictim3
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 WSvictim / TYPE=UN SUBJECT=ClassID*StudentID;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11a: Add Level-3 by Within-Class Victim Cross-Level Interactions';
TITLE2 'Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5 WSvictim WCvictim CMvictim3
                   CMgirl50*CMemosup5 CMemosup5*CMvictim3
                   girl*WCvictim CMgirl50*WCvictim
                   WCvictim*CMemosup5 WCvictim*CMvictim3
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 WSvictim / TYPE=UN SUBJECT=ClassID*StudentID;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 11a: Add Level-3 by Within-Student Victim Cross-Level Interactions';
TITLE2 'Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5 WSvictim WCvictim CMvictim3
                   CMgirl50*CMemosup5 CMemosup5*CMvictim3
                   girl*WCvictim CMgirl50*WCvictim
                   WSvictim*CMgirl50 WSvictim*CMemosup5 WSvictim*CMvictim3
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 WSvictim / TYPE=UN SUBJECT=ClassID*StudentID;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 11a.9: Add Level-2 and Level-3 Victim by Within-Student Victim Cross-Level Interactions';
TITLE2 'Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5 WSvictim WCvictim CMvictim3
                   CMgirl50*CMemosup5 CMemosup5*CMvictim3
                   girl*WCvictim CMgirl50*WCvictim
                   WSvictim*WCvictim WSvictim*CMvictim3
                    / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=FinalC;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 WSvictim / TYPE=UN SUBJECT=ClassID*StudentID;
     ODS OUTPUT CovParms=CovL32xVicx;
     ESTIMATE 'Contextual Gender Effect at Wave 1'                   CMgirl50 1 CMgirl50*time1 0 / CL;
     ESTIMATE 'Contextual Gender Effect at Wave 2'                   CMgirl50 1 CMgirl50*time1 1 / CL;
     ESTIMATE 'Contextual Gender Effect at Wave 3'                   CMgirl50 1 CMgirl50*time1 2 / CL;
     ESTIMATE 'Between-Class Emotional Support Effect at Wave 1'     CMemosup5 1 CMemosup5*time1 0 / CL;
     ESTIMATE 'Between-Class Emotional Support Effect at Wave 2'     CMemosup5 1 CMemosup5*time1 1 / CL;
     ESTIMATE 'Between-Class Emotional Support Effect at Wave 3'     CMemosup5 1 CMemosup5*time1 2 / CL;
     ESTIMATE 'Level-2 Victim Effect in Girls'                       WCvictim 1 WCvictim*girl 1 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=10, CovFewer=CovL32x, CovMore=CovL32xVicx);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=close, PredFewer=PredEmpty, PredMore=FinalC);
 
TITLE1 'Ch 11a: Add Level-2 and Level-3 Gender by Within-Student Victim Cross-Level Interactions';
TITLE2 'Predicting Student Closeness';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5 WSvictim WCvictim CMvictim3
                   CMgirl50*CMemosup5 CMemosup5*CMvictim3
                   girl*WCvictim CMgirl50*WCvictim WSvictim*WCvictim WSvictim*CMvictim3
                   WSvictim*girl WSvictim*CMgirl50
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 WSvictim / TYPE=UN SUBJECT=ClassID*StudentID;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 11a.9: Add Level-2 and Level-3 Victim by Within-Student Victim Cross-Level Interactions';
TITLE2 'Predicting Student Closeness using ML instead of REML';
PROC MIXED DATA=work.Chapter11a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS ClassID StudentID;
     MODEL close = time1 girl time1*girl CMgirl50 time1*CMgirl50
                   CMemosup5 time1*CMemosup5 WSvictim WCvictim CMvictim3
                   CMgirl50*CMemosup5 CMemosup5*CMvictim3
                   girl*WCvictim CMgirl50*WCvictim
                   WSvictim*WCvictim WSvictim*CMvictim3
                    / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT time1 / TYPE=UN SUBJECT=ClassID;
     RANDOM INTERCEPT time1 WSvictim / TYPE=UN SUBJECT=ClassID*StudentID;
RUN; TITLE1; TITLE2;
 
****** END CHAPTER 11a MODELS ******;

* Close output directory;
ODS HTML CLOSE;

