* Output options: all can be turned on/off by adding or removing the NO;
* page number, date, centering, or page breaks, page length and width;
OPTIONS NOnumber NOdate NOcenter FormDlim=' ' PageSize=MAX LineSize=MAX;
* Eliminate SAS default titles and names of tables in output (TRACE ON to show);
TITLE; ODS TRACE OFF;

***********************************************************************************;
*******   MACRO PROGRAMS TO AUTOMATE CALCULATIONS FOR CHAPTER 2 EXAMPLE     *******;
*******               NOTHING IN HERE NEEDS TO BE CHANGED                   *******;
***********************************************************************************;

* To use ModelR2 macro for chapter 2 cross-sectional models;
* CovFewer = name of covparms table for baseline empty model;
* CovFewer = name of covparms table for nested model;
* CovMore  = name of covparms table for comparison model;
%MACRO ModelR2(CovBase=,CovFewer=,CovMore=);
DATA &CovBase.;  LENGTH Name $30.; SET &CovBase.;  Name="&CovBase." ;  
     CALL SYMPUT('EmptyEst',estimate);  RUN;
DATA &CovFewer.; LENGTH Name $30.; SET &CovFewer.; Name="&CovFewer."; RUN;
DATA &CovMore.;  LENGTH Name $30.; SET &CovMore.;  Name="&CovMore.";  RUN;
DATA CovCompare; LENGTH Name $30.; SET &CovBase. &CovFewer. &CovMore.; RUN;
DATA CovCompare; SET CovCompare;  
     R2_from_Base=(&EmptyEst.-Estimate)/&EmptyEst.;
     R2_Increment=R2_from_Base-LAG1(R2_from_Base); 
     IF _N_=2 THEN R2_Increment=.; RUN;
DATA CovCompare; SET CovCompare; 
     IF CovParm IN("UN(2,1)","UN(3,1)","UN(4,1)","UN(3,2)","UN(4,2)","UN(4,3)") 
     THEN DELETE; RUN;
TITLE9 "R2 (% Reduction) Overall and for &CovFewer. vs. &CovMore.";
PROC PRINT NOOBS DATA=CovCompare; RUN; TITLE9;
%MEND ModelR2;

* To use Regions Macro, enter;
*   FixData =   Name of dataset that stores fixed effects for model (SolutionF on ODS);
*   CovBData =  Name of dataset that stores XTX inv matrix for model (CovB on ODS);
*   Pred =      Case-sensitive name of predictor effect regions are for; 
*   Mod =       Case-sensitive name of moderator effect (for region values);
*   ModCenter = Centering point of moderator predictor;
*   Interact =  Case-sensitive name of interaction effect; 
*   Order =     Order of entry of interaction in MODEL statement;
%MACRO Regions(FixData=,CovBData=, Pred=,Mod=,ModCenter=,Interact=,Order=);
DATA _NULL_; SET &FixData.; WHERE Effect="&Pred.";
     CALL SYMPUT('Bpred', Estimate); 
     CALL SYMPUT('SEpred', StdErr); RUN; 
DATA _NULL_; SET &FixData.; WHERE Effect="&Interact.";
     CALL SYMPUT('Binter', Estimate); 
     CALL SYMPUT('SEinter', StdErr); RUN; 
%LET order=%EVAL(&order.+1);
DATA _NULL_; SET &CovBData.; 
     WHERE INDEX(Effect,"&Pred.")>0 AND INDEX(Effect,"*")=0;
     CALL SYMPUT('CovPredInt', ROUND(Col&order.,.0001)); RUN;    
%PUT Bpred=&Bpred. SEpred=&SEpred. Binter=&Binter. 
     SEinter=&SEinter. CovPredInt=&CovPredInt.;
DATA Regions;
     A=(1.96*1.96)*(&SEinter.*&SEinter.)-(&Binter.*&Binter.);
     B=2*((1.96*1.96)*&CovPredInt.-(&Bpred.*&Binter.));
     C=(1.96*1.96*&SEpred.*&SEpred.)-(&Bpred.*&Bpred.);
     CenteredLower=((-1*B)+SQRT((B*B)-4*A*C))/(2*A); 
        CALL SYMPUT('cenlower',ROUND(CenteredLower,.001));
     CenteredUpper=((-1*B)-SQRT((B*B)-4*A*C))/(2*A); 
        CALL SYMPUT('cenupper',ROUND(CenteredUpper,.001));
     UncenteredLower=CenteredLower+&ModCenter.; 
        CALL SYMPUT('uncenlower',ROUND(UncenteredLower,.001));
     UncenteredUpper=CenteredUpper+&ModCenter.; 
        CALL SYMPUT('uncenupper',ROUND(UncenteredUpper,.001));
RUN;
TITLE7 "Regions of significance for &interact. interaction:";
TITLE8 "The effect of &pred. will be significant at centered values of &mod. BELOW the lower bound"; 
TITLE9 "and ABOVE the upper bound, which translate to these uncentered lower and upper bounds."; 
PROC PRINT DATA=Regions NOOBS; VAR CenteredLower--UncenteredUpper; RUN; TITLE7; TITLE8; TITLE9;
%MEND Regions;

***********************************************************************************;
*******             BEGIN DATA MANIPULATION FOR CHAPTER 2 EXAMPLE           *******;
*******                  CHANGE "filesave" to your directory                *******;
***********************************************************************************;

* Defining global variable for file location to be replaced in code below;
%LET filesave= C:\Dropbox\PilesOfVariance\Chapter2\SAS;
* Location for SAS files for these models (uses macro variable filesave);
LIBNAME filesave "&filesave.";

* Create format for demgroup;
PROC FORMAT; VALUE FDemGroup 1="1None" 2="2Future" 3="3Current"; RUN;

* Import chapter 2 example data into work library and center predictors;
DATA work.Chapter2; SET filesave.SAS_Chapter2;
* Centering age at different points;
age80 = age - 80;
age85 = age - 85;
age90 = age - 90;
* Centering grip at different points;
grip6 = grip - 6;
grip9 = grip - 9;
grip12 = grip - 12;
* Re-coding sex so women are reference;
     IF sexMW=0 THEN sexWM=1; 
ELSE IF sexMW=1 THEN sexWM=0; 
* Creating all possible contrasts for dementia groups;
     IF demgroup=1 THEN DO; demNF=0; demNC=0; demFN=1; demFC=0; demCN=1; demCF=0; END;
ELSE IF demgroup=2 THEN DO; demNF=1; demNC=0; demFN=0; demFC=0; demCN=0; demCF=1; END;
ELSE IF demgroup=3 THEN DO; demNF=0; demNC=1; demFN=0; demFC=1; demCN=0; demCF=0; END;
* Labeling new variables;
LABEL   
age80=  "age80: Age in Years (0=80)"
age85=  "age85: Age in Years (0=85)"
age90=  "age85: Age in Years (0=90)"
grip6=  "grip6: Grip Strength in Pounds (0=6)"
grip9=  "grip9: Grip Strength in Pounds (0=9)"
grip12= "grip12: Grip Strength in Pounds (0=12)"
sexWM=  "sexwm: Sex (0=Women, 1=Men)"
demNF=  "demNF: Dementia Contrast for None=0 vs Future=1"
demNC=  "demNC: Dementia Contrast for None=0 vs Current=1"
demFN=  "demFN: Dementia Contrast for Future=0 vs None=1"
demFC=  "demFC: Dementia Contrast for Future=0 vs Current=1"
demCN=  "demCN: Dementia Contrast for Current=0 vs None=1"
demCF=  "demCF: Dementia Contrast for Current=0 vs Future=1";
* Add format to demgroup to display in later output;
FORMAT demgroup Fdemgroup.;
RUN;

* Creating 'fake people' to show age*grip interaction;
* Each row is a fake person for which to create a predicted outcome;
DATA work.FakeAgeGrip; INPUT PersonID grip9 age85 sexMW demNF demNC; 
DATALINES;
-99  3 -5  0  0  0 
-99  3  0  0  0  0 
-99  3  5  0  0  0 
-99  0 -5  0  0  0 
-99  0  0  0  0  0 
-99  0  5  0  0  0 
-99 -3 -5  0  0  0 
-99 -3  0  0  0  0 
-99 -3  5  0  0  0 
; RUN;
* Merge with real data;
DATA work.PlotAgeGrip; MERGE work.Chapter2 work.FakeAgeGrip; BY PersonID; RUN;

* Creating 'fake people' to show age*grip*sex interaction;
* Each row is a fake person for which to create a predicted outcome;
DATA work.FakeAgeGripSex; INPUT PersonID grip9 age85 sexMW demNF demNC; 
DATALINES;
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
; RUN;
* Merge with real data;
DATA work.PlotAgeGripSex; MERGE work.Chapter2 work.FakeAgeGripSex; BY PersonID; RUN;

***********************************************************************************;
*******                       BEGIN CHAPTER 2 MODELS                        *******;
***********************************************************************************;

* Open output directory to save results to;
ODS HTML FILE="&filesave.\SAS_Chapter2_Output.html"
         (URL="SAS_Chapter2_Output.html") STYLE=HTMLBlue;

TITLE1 "Chapter 2: Descriptive Statistics for Example Variables";
PROC MEANS DATA=work.Chapter2; VAR age grip cognition; RUN;
PROC FREQ  DATA=work.Chapter2; TABLE sexMW*demgroup; RUN;
PROC CORR  DATA=work.Chapter2; VAR age grip sexMW cognition; RUN;
TITLE1;

TITLE1 'Eq 2.3: Empty Means Model';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     MODEL cognition =  / SOLUTION CL CHISQ DDFM=BW;
     ODS OUTPUT CovParms=CovEmpty;
RUN; TITLE1;
 
TITLE1 'Eq 2.4: Adding Age (0=85)';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     MODEL cognition = age85 / SOLUTION CL CHISQ DDFM=BW;
     ODS OUTPUT CovParms=CovAge;
RUN; TITLE1;
     * Call macro to calculate R2 for overall model;
     %ModelR2(CovBase=CovEmpty, CovFewer=CovEmpty, CovMore=CovAge);
 
TITLE1 'Eq 2.4: Adding Original Age Instead';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     MODEL cognition = age / SOLUTION CL CHISQ DDFM=BW;
RUN; TITLE1;
 
TITLE1 'Eq 2.6: Adding Grip (0=9)';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     MODEL cognition = age85 grip9 / SOLUTION CL CHISQ DDFM=BW;
     ODS OUTPUT CovParms=CovGrip;
     CONTRAST 'Model R2 Test' age85 1, grip9 1 / CHISQ;
RUN; TITLE1;
     * Call macro to calculate R2 for overall model;
     %ModelR2(CovBase=CovEmpty, CovFewer=CovAge, CovMore=CovGrip);
 
TITLE1 'Eq 2.7: Adding Sex (0=M, 1=W)';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     MODEL cognition = age85 grip9 sexMW / SOLUTION CL CHISQ DDFM=BW;
     ODS OUTPUT CovParms=CovSex;
     CONTRAST 'Model R2 Test' age85 1, grip9 1, sexMW 1 / CHISQ;
RUN; TITLE1;
     * Call macro to calculate R2 for overall model;
     %ModelR2(CovBase=CovEmpty, CovFewer=CovGrip, CovMore=CovSex);
 
TITLE1 'Eq 2.7: Adding Sex (1=M 0=W)';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     MODEL cognition = age85 grip9 sexWM / SOLUTION CL CHISQ DDFM=BW;
RUN; TITLE1;
 
TITLE1 'Eq 2.8: Adding Dementia Group';
TITLE2 'Using Manual Group Contrasts so Reference=None';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     MODEL cognition = age85 grip9 sexMW demNF demNC / SOLUTION CL CHISQ DDFM=BW;
     ODS OUTPUT CovParms=CovDem;
     CONTRAST 'Model R2 Test' age85 1, grip9 1, sexMW 1, demNF 1, demNC 1 / CHISQ;
     CONTRAST 'Omnibus Dementia Group Test' demNF 1, demNC 1 / CHISQ;
     ESTIMATE 'Future vs Current' demNF -1 demNC 1 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate R2 for overall model;
     %ModelR2(CovBase=CovEmpty, CovFewer=CovSex, CovMore=CovDem);
 
TITLE1 'Eq 2.8: Adding Dementia Group';
TITLE2 'Categorical Predictor for Dementia Group';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS demgroup;
     MODEL cognition = age85 grip9 sexMW demgroup / SOLUTION CL CHISQ DDFM=BW;
     LSMEANS demgroup / DIFF=ALL AT(age85 grip9 sexMW) = (0 0 0) CL;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 2.9: Adding Age by Grip Interaction';
TITLE2 'Age (0=85), Grip (0=9)';
* Estimate model on data with fake people to make predictions;
PROC MIXED DATA=work.PlotAgeGrip COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     MODEL cognition = age85 grip9 sexMW demNF demNC
                       age85*grip9
                        / SOLUTION CL CHISQ DDFM=BW COVB OUTPM=PredAgeGrip;
     ODS OUTPUT CovParms=CovAgeGrip SolutionF=FixAgeGrip COVB=CovBAgeGrip;
     CONTRAST 'Model R2 Test' age85 1, grip9 1, sexMW 1, demNF 1, demNC 1, age85*grip9 1 / CHISQ;
     ESTIMATE 'Future vs Current'                     demNF -1 demNC 1 / CL;
     ESTIMATE 'Age Slope at Grip Strength =  6'       age85 1 age85*grip9 -3 / CL;
     ESTIMATE 'Age Slope at Grip Strength =  9'       age85 1 age85*grip9 0 / CL;
     ESTIMATE 'Age Slope at Grip Strength = 12'       age85 1 age85*grip9 3 / CL;
     ESTIMATE 'Grip Strength Slope at Age = 80'       grip9 1 age85*grip9 -5 / CL;
     ESTIMATE 'Grip Strength Slope at Age = 85'       grip9 1 age85*grip9 0 / CL;
     ESTIMATE 'Grip Strength Slope at Age = 90'       grip9 1 age85*grip9 5 / CL;
     ESTIMATE 'Cognition at Grip = 12 Age = 80'       intercept 1 age85 -5 grip9 3  age85*grip9 -15 / CL;
     ESTIMATE 'Cognition at Grip = 12 Age = 85'       intercept 1 age85 0  grip9 3  age85*grip9 0 / CL;
     ESTIMATE 'Cognition at Grip = 12 Age = 90'       intercept 1 age85 5  grip9 3  age85*grip9 15 / CL;
     ESTIMATE 'Cognition at Grip =  9 Age = 80'       intercept 1 age85 -5 grip9 0  age85*grip9 0 / CL;
     ESTIMATE 'Cognition at Grip =  9 Age = 85'       intercept 1 age85 0  grip9 0  age85*grip9 0 / CL;
     ESTIMATE 'Cognition at Grip =  9 Age = 90'       intercept 1 age85 5  grip9 0  age85*grip9 0 / CL;
     ESTIMATE 'Cognition at Grip =  6 Age = 80'       intercept 1 age85 -5 grip9 -3 age85*grip9 15 / CL;
     ESTIMATE 'Cognition at Grip =  6 Age = 85'       intercept 1 age85 0  grip9 -3 age85*grip9 0 / CL;
     ESTIMATE 'Cognition at Grip =  6 Age = 90'       intercept 1 age85 5  grip9 -3 age85*grip9 -15 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate R2 for overall model;
     %ModelR2(CovBase=CovEmpty, CovFewer=CovDem, CovMore=CovAgeGrip);
     * Call macro for regions of significance for main effects of interaction;
     %Regions(FixData=FixAgeGrip, CovBData=CovBAgeGrip, Pred=grip9, Mod=age85,
              ModCenter=85, Interact=age85*grip9, Order=6);
     %Regions(FixData=FixAgeGrip, CovBData=CovBAgeGrip, Pred=age85, Mod=grip9,
              ModCenter=9, Interact=age85*grip9, Order=6);
     * Subset predicted outcomes data to fake people;
     DATA PredAgeGrip; SET PredAgeGrip; WHERE PersonID=-99; RUN;
     TITLE9 'Predicted Outcomes for Fake People';
     PROC PRINT NOOBS DATA=PredAgeGrip; VAR age85 grip9 sexMW demNF demNC pred; RUN;
 
TITLE1 'Eq 2.9: Adding Age by Grip Interaction';
TITLE2 'Age (0=80), Grip (0=12)';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     MODEL cognition = age80 grip12 sexMW demNF demNC
                       age80*grip12
                        / SOLUTION CL CHISQ DDFM=BW;
     ESTIMATE 'Future vs Current' demNF -1 demNC 1 / CL;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 2.9: Adding Age by Grip Interaction';
TITLE2 'Age (0=90), Grip (0=6)';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     MODEL cognition = age90 grip6 sexMW demNF demNC
                       age90*grip6
                        / SOLUTION CL CHISQ DDFM=BW;
     ESTIMATE 'Future vs Current' demNF -1 demNC 1 / CL;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 2.13: Adding Sex by Dementia Interaction';
TITLE2 'Sex (0=Men), Dementia (0=None)';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     MODEL cognition = age85 grip9 sexMW demNF demNC
                       age85*grip9 sexMW*demNF sexMW*demNC
                        / SOLUTION CL CHISQ DDFM=BW COVB;
     ODS OUTPUT CovParms=CovSexDem SolutionF=FixAgeGrip COVB=CovBAgeGrip;
     CONTRAST 'Model R2 Test' age85 1, grip9 1, sexMW 1, demNF 1, demNC 1, age85*grip9 1, sexMW*demNF 1, sexMW*demNC 1 / CHISQ;
     CONTRAST 'Omnibus Dementia*Sex Interaction Test' sexMW*demNF 1, sexMW*demNC 1 / CHISQ;
     ESTIMATE 'Cognition for Men   None'                   intercept 1 sexMW 0 demNF 0 demNC 0 sexMW*demNF 0 sexMW*demNC 0 / CL;
     ESTIMATE 'Cognition for Women None'                   intercept 1 sexMW 1 demNF 0 demNC 0 sexMW*demNF 0 sexMW*demNC 0 / CL;
     ESTIMATE 'Cognition for Men   Future'                 intercept 1 sexMW 0 demNF 1 demNC 0 sexMW*demNF 0 sexMW*demNC 0 / CL;
     ESTIMATE 'Cognition for Women Future'                 intercept 1 sexMW 1 demNF 1 demNC 0 sexMW*demNF 1 sexMW*demNC 0 / CL;
     ESTIMATE 'Cognition for Men   Current'                intercept 1 sexMW 0 demNF 0 demNC 1 sexMW*demNF 0 sexMW*demNC 0 / CL;
     ESTIMATE 'Cognition for Women Current'                intercept 1 sexMW 1 demNF 0 demNC 1 sexMW*demNF 0 sexMW*demNC 1 / CL;
     ESTIMATE 'Sex Difference for No Dementia'             sexMW 1 sexMW*demNF 0 sexMW*demNC 0 / CL;
     ESTIMATE 'Sex Difference for Future Dementia'         sexMW 1 sexMW*demNF 1 sexMW*demNC 0 / CL;
     ESTIMATE 'Sex Difference for Current Dementia'        sexMW 1 sexMW*demNF 0 sexMW*demNC 1 / CL;
     ESTIMATE 'None-Future Difference for Men'             demNF 1 sexMW*demNF 0 / CL;
     ESTIMATE 'None-Future Difference for Women'           demNF 1 sexMW*demNF 1 / CL;
     ESTIMATE 'None-Current Difference for Men'            demNC 1 sexMW*demNC 0 / CL;
     ESTIMATE 'None-Current Difference for Women'          demNC 1 sexMW*demNC 1 / CL;
     ESTIMATE 'Future-Current Difference for Men'          demNF -1 demNC 1 sexMW*demNF 0  sexMW*demNC 0 / CL;
     ESTIMATE 'Future-Current Difference for Women'        demNF -1 demNC 1 sexMW*demNF -1 sexMW*demNC 1 / CL;
     ESTIMATE 'None-Future Sex Difference'                 sexMW*demNF 1 / CL;
     ESTIMATE 'None-Current Sex Difference'                sexMW*demNC 1 / CL;
     ESTIMATE 'Future-Current Sex Difference'              sexMW*demNF -1 sexMW*demNC 1 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate R2 for overall model;
     %ModelR2(CovBase=CovEmpty, CovFewer=CovAgeGrip, CovMore=CovSexDem);
     * Call macro for regions of significance for main effects of interaction;
     %Regions(FixData=FixAgeGrip, CovBData=CovBAgeGrip, Pred=grip9, Mod=age85,
              ModCenter=85, Interact=age85*grip9, Order=6);
     %Regions(FixData=FixAgeGrip, CovBData=CovBAgeGrip, Pred=age85, Mod=grip9,
              ModCenter=9, Interact=age85*grip9, Order=6);
 
TITLE1 'Eq 2.13: Adding Sex by Dementia Interaction';
TITLE2 'Sex (0=Women), Dementia (0=None)';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     MODEL cognition = age85 grip9 sexwm demNF demNC
                       age85*grip9 sexwm*demNF sexwm*demNC
                        / SOLUTION CL CHISQ DDFM=BW;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 2.15: Adding Sex by Dementia Interaction';
TITLE2 'Sex (0=Men), Dementia (0=Future)';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     MODEL cognition = age85 grip9 sexMW demFN demFC
                       age85*grip9 sexMW*demFN sexMW*demFC
                        / SOLUTION CL CHISQ DDFM=BW;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 2.15: Adding Sex by Dementia Interaction';
TITLE2 'Sex (0=Women), Dementia (0=Future)';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     MODEL cognition = age85 grip9 sexwm demFN demFC
                       age85*grip9 sexwm*demFN sexwm*demFC
                        / SOLUTION CL CHISQ DDFM=BW;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 2.16: Adding Sex by Dementia Interaction';
TITLE2 'Sex (0=Men), Dementia (0=Current)';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     MODEL cognition = age85 grip9 sexMW demCN demCF
                       age85*grip9 sexMW*demCN sexMW*demCF
                        / SOLUTION CL CHISQ DDFM=BW;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 2.16: Adding Sex by Dementia Interaction';
TITLE2 'Sex (0=Women), Dementia (0=Current)';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     MODEL cognition = age85 grip9 sexwm demCN demCF
                       age85*grip9 sexwm*demCN sexwm*demCF
                        / SOLUTION CL CHISQ DDFM=BW;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 2.16: Adding Sex by Dementia Interaction';
TITLE2 'Categorical Sex and Dementia';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS sexMW demgroup;
     MODEL cognition = age85 grip9 sexMW demgroup
                       age85*grip9 sexMW*demgroup
                        / SOLUTION CL CHISQ DDFM=BW;
     LSMEANS sexMW*demgroup / SLICE=demgroup SLICE=sexMW DIFF=ALL AT(age85 grip9) = (0 0) CL;
     CONTRAST 'Sex by None-Future    Interaction'     sexMW*demgroup -1  1  0  1 -1  0;
     CONTRAST 'Sex by None-Current   Interaction'     sexMW*demgroup -1  0  1  1  0 -1;
     CONTRAST 'Sex by Future-Current Interaction'     sexMW*demgroup  0 -1  1  0  1 -1;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 2.17: Adding Sex by Age and Sex by Grip Interactions';
TITLE2 'Age (0=85), Grip (0=9), Sex (0=Men), Dementia (0=None)';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     MODEL cognition = age85 grip9 sexMW demNF demNC
                       age85*grip9 sexMW*demNF sexMW*demNC
                       age85*sexMW grip9*sexMW
                        / SOLUTION CL CHISQ DDFM=BW;
     ODS OUTPUT CovParms=Cov2AgeSexGrip;
     ESTIMATE 'Age for Men'                 age85 1 age85*sexMW 0 / CL;
     ESTIMATE 'Age for Women'               age85 1 age85*sexMW 1 / CL;
     ESTIMATE 'Grip for Men'                grip9 1 grip9*sexMW 0 / CL;
     ESTIMATE 'Grip for Women'              grip9 1 grip9*sexMW 1 / CL;
     ESTIMATE 'Sex for None'                sexMW 1 sexMW*demNF 0 sexMW*demNC 0 / CL;
     ESTIMATE 'Sex for Future'              sexMW 1 sexMW*demNF 1 sexMW*demNC 0 / CL;
     ESTIMATE 'Sex for Current'             sexMW 1 sexMW*demNF 0 sexMW*demNC 1 / CL;
     ESTIMATE 'Men: None vs Future'         demNF 1  demNC 0 sexMW*demNF 0  sexMW*demNC 0 / CL;
     ESTIMATE 'Men: None vs Current'        demNF 0  demNC 1 sexMW*demNF 0  sexMW*demNC 0 / CL;
     ESTIMATE 'Men: Future vs Current'      demNF -1 demNC 1 sexMW*demNF 0  sexMW*demNC 0 / CL;
     ESTIMATE 'Women: None vs Future'       demNF 1  demNC 0 sexMW*demNF 1  sexMW*demNC 0 / CL;
     ESTIMATE 'Women: None vs Current'      demNF 0  demNC 1 sexMW*demNF 0  sexMW*demNC 1 / CL;
     ESTIMATE 'Women: Future vs Current'    demNF -1 demNC 1 sexMW*demNF -1 sexMW*demNC 1 / CL;
     ESTIMATE 'Sex by None vs Future'       demNF*sexMW 1  demNC*sexMW 0 / CL;
     ESTIMATE 'Sex by None vs Current'      demNF*sexMW 0  demNC*sexMW 1 / CL;
     ESTIMATE 'Sex by Future vs Current'    demNF*sexMW -1 demNC*sexMW 1 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate R2 for overall model;
     %ModelR2(CovBase=CovEmpty, CovFewer=CovSexDem, CovMore=Cov2AgeSexGrip);
 
TITLE1 'Eq 2.18: Adding Sex by Age by Grip Three-Way Interaction';
TITLE2 'Age (0=85), Grip (0=9), Sex (0=Men), Dementia (0=None)';
* Estimate model on data with fake people to make predictions;
PROC MIXED DATA=work.PlotAgeGripSex COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     MODEL cognition = age85 grip9 sexMW demNF demNC
                       age85*grip9 sexMW*demNF sexMW*demNC
                       age85*sexMW grip9*sexMW age85*grip9*sexMW
                        / SOLUTION CL CHISQ DDFM=BW OUTPM=Pred3AgeSexGrip;
     ODS OUTPUT CovParms=Cov3AgeSexGrip;
     ESTIMATE 'Age for Men'                 age85 1  age85*sexMW 0 / CL;
     ESTIMATE 'Age for Women'               age85 1  age85*sexMW 1 / CL;
     ESTIMATE 'Grip for Men'                grip9 1  grip9*sexMW 0 / CL;
     ESTIMATE 'Grip for Women'              grip9 1  grip9*sexMW 1 / CL;
     ESTIMATE 'Sex for None'                sexMW 1  sexMW*demNF 0 sexMW*demNC 0 / CL;
     ESTIMATE 'Sex for Future'              sexMW 1  sexMW*demNF 1 sexMW*demNC 0 / CL;
     ESTIMATE 'Sex for Current'             sexMW 1  sexMW*demNF 0 sexMW*demNC 1 / CL;
     ESTIMATE 'Men: None vs Future'         demNF 1  demNC 0 sexMW*demNF 0  sexMW*demNC 0 / CL;
     ESTIMATE 'Men: None vs Current'        demNF 0  demNC 1 sexMW*demNF 0  sexMW*demNC 0 / CL;
     ESTIMATE 'Men: Future vs Current'      demNF -1 demNC 1 sexMW*demNF 0  sexMW*demNC 0 / CL;
     ESTIMATE 'Women: None vs Future'       demNF 1  demNC 0 sexMW*demNF 1  sexMW*demNC 0 / CL;
     ESTIMATE 'Women: None vs Current'      demNF 0  demNC 1 sexMW*demNF 0  sexMW*demNC 1 / CL;
     ESTIMATE 'Women: Future vs Current'    demNF -1 demNC 1 sexMW*demNF -1 sexMW*demNC 1 / CL;
     ESTIMATE 'Sex by None vs Future'       demNF*sexMW 1  demNC*sexMW 0 / CL;
     ESTIMATE 'Sex by None vs Current'      demNF*sexMW 0  demNC*sexMW 1 / CL;
     ESTIMATE 'Sex by Future vs Current'    demNF*sexMW -1 demNC*sexMW 1 / CL;
     ESTIMATE 'Age by Grip for Men'         age85*grip9 1  age85*grip9*sexMW 0 / CL;
     ESTIMATE 'Age by Grip for Women'       age85*grip9 1  age85*grip9*sexMW 1 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate R2 for overall model;
     %ModelR2(CovBase=CovEmpty, CovFewer=Cov2AgeSexGrip, CovMore=Cov3AgeSexGrip);
     * Subset predicted outcomes data to fake people;
     DATA Pred3AgeSexGrip; SET Pred3AgeSexGrip; WHERE PersonID=-99; RUN;
     TITLE9 'Predicted Outcomes for Fake People';
     PROC PRINT NOOBS DATA=Pred3AgeSexGrip; VAR age85 grip9 sexMW demNF demNC pred; RUN;
 
TITLE1 'Eq 2.13: Final Reported Model';
TITLE2 'Age (0=85), Grip (0=9), Sex (0=Men), Dementia (0=None)';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     MODEL cognition = age85 grip9 sexMW demNF demNC
                       age85*grip9 sexMW*demNF sexMW*demNC
                        / SOLUTION CL CHISQ DDFM=BW;
     ODS OUTPUT CovParms=CovSexDem;
     CONTRAST 'Model R2 Test' age85 1, grip9 1, sexMW 1, demNF 1, demNC 1, age85*grip9 1, sexMW*demNF 1, sexMW*demNC 1 / CHISQ;
     CONTRAST 'Omnibus Dementia*Sex Interaction Test' sexMW*demNF 1, sexMW*demNC 1 / CHISQ;
     ESTIMATE 'Sex Difference for No Dementia'             sexMW 1  sexMW*demNF 0 sexMW*demNC 0 / CL;
     ESTIMATE 'Sex Difference for Future Dementia'         sexMW 1  sexMW*demNF 1 sexMW*demNC 0 / CL;
     ESTIMATE 'Sex Difference for Current Dementia'        sexMW 1  sexMW*demNF 0 sexMW*demNC 1 / CL;
     ESTIMATE 'None-Future Difference for Men'             demNF 1  sexMW*demNF 0 / CL;
     ESTIMATE 'None-Future Difference for Women'           demNF 1  sexMW*demNF 1 / CL;
     ESTIMATE 'None-Current Difference for Men'            demNC 1  sexMW*demNC 0 / CL;
     ESTIMATE 'None-Current Difference for Women'          demNC 1  sexMW*demNC 1 / CL;
     ESTIMATE 'Future-Current Difference for Men'          demNF -1 demNC 1 sexMW*demNF 0  sexMW*demNC 0 / CL;
     ESTIMATE 'Future-Current Difference for Women'        demNF -1 demNC 1 sexMW*demNF -1 sexMW*demNC 1 / CL;
     ESTIMATE 'None-Future Sex Difference'                 sexMW*demNF 1 / CL;
     ESTIMATE 'None-Current Sex Difference'                sexMW*demNC 1 / CL;
     ESTIMATE 'Future-Current Sex Difference'              sexMW*demNF -1 sexMW*demNC 1 / CL;
RUN; TITLE1; TITLE2;
     * Call macro to calculate R2 for overall model;
     %ModelR2(CovBase=CovEmpty, CovFewer=CovEmpty, CovMore=CovSexDem);
 
TITLE1 'Eq 2.13: Final Reported Model';
TITLE2 'Categorical Variables for Sex and Dementia';
PROC MIXED DATA=work.Chapter2 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS sexMW demgroup;
     MODEL cognition = age85 grip9 sexMW demgroup
                       age85*grip9 sexMW*demgroup
                        / SOLUTION CL CHISQ DDFM=BW;
     LSMEANS sexMW*demgroup / SLICE=demgroup SLICE=sexMW DIFF=ALL AT(age85 grip9) = (0 0) CL;
RUN; TITLE1; TITLE2;
 
****** END CHAPTER 2 MODELS ******;

* Close output directory;
ODS HTML CLOSE;

