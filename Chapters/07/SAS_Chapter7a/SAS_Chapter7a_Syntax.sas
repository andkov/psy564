* Output options: all can be turned on/off by adding or removing the NO;
* page number, date, centering, or page breaks, page length and width;
OPTIONS NOnumber NOdate NOcenter FormDlim=' ' PageSize=MAX LineSize=MAX;
* Eliminate SAS default titles and names of tables in output (TRACE ON to show);
TITLE; ODS TRACE OFF; ODS LISTING CLOSE;

***********************************************************************************;
*******  MACRO PROGRAMS TO AUTOMATE CALCULATIONS FOR CHAPTER 7a EXAMPLE     *******;
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

* To use Regions Macro, enter;
*   FixData =   Name of dataset that stores fixed effects for model (SolutionF on ODS);
*   CovBData =  Name of dataset that stores XTX inv matrix for model (CovB on ODS);
*   Pred =      Case-sensitive name of predictor effect regions are for; 
*   Mod =       Case-sensitive name of moderator effect (for region values);
*   ModCenter = Centering point of moderator predictor;
*   Interact =  Case-sensitive name of interaction effect; 
*   Order =     Order of entry of interaction in MODEL statement;
%MACRO Regions(FixData=,CovBData=,Pred=,Mod=,ModCenter=,Interact=,Order=);
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
*******     BEGIN DATA MANIPULATION FOR CHAPTER 7a FLUCTUATION EXAMPLE      *******;
*******                 CHANGE "filesave" to your directory                 *******;
***********************************************************************************;

* Defining global variable for file location to be replaced in code below;
%LET filesave= C:\Dropbox\PilesOfVariance\Chapter7a\SAS;
* Location for SAS files for these models (uses macro variable filesave);
LIBNAME filesave "&filesave.";

* Import chapter 7a stacked data into work library; 
DATA work.Chapter7a; SET filesave.SAS_Chapter7a; RUN;

* Create person means across time for women and age for descriptives;
PROC SORT DATA=work.Chapter7a; BY PersonID session; RUN;
PROC MEANS NOPRINT DATA=work.Chapter7a; BY PersonID;
     VAR women baseage;
     OUTPUT OUT=work.PersonMeans7a 
     MEAN(women baseage) = PMwomen PMbaseage;
RUN;

* Create centered predictors for analysis;
DATA work.Chapter7a; SET work.Chapter7a;
studyday1 = studyday - 1;
dayofweek1 = dayofweek - 1;
age80 = baseage - 80;
LABEL
studyday1 = "studyday1: Day of Study (0=1)"
dayofweek1 = "dayofweek1: Day of Week (0=1)"
age80 = "age80: Baseline Age (0=80)";
* Subset sample to complete cases for all eventual predictors in chapter 8;
IF NMISS(women, baseage, symptoms, mood, stressor, mood)>0 THEN DELETE;
* Subset sample to study days within a two-week period;
IF studyday>14 THEN DELETE;
RUN; 

***********************************************************************************;
*******                 BEGIN CHAPTER 7a FLUCTUATION MODELS                 *******;
***********************************************************************************;

* Open output directory to save results to;
ODS HTML FILE="&filesave.\SAS_Chapter7a_Output.html"
         (URL="SAS_Chapter7a_Output.html") STYLE=HTMLBlue;

TITLE1 "Chapter 7a: Descriptive Statistics for Time-Invariant Variables";
PROC MEANS DATA=work.PersonMeans7a; 
     VAR PMwomen PMbaseage; 
RUN; TITLE1;

TITLE1 "Chapter 7a: Descriptive Statistics for Time-Varying Variables";
PROC MEANS DATA=work.Chapter7a; 
     VAR symptoms; 
RUN; TITLE1;

TITLE1 'Eq 7a.3: Empty Means, Random Intercept Model';
PROC MIXED DATA=work.Chapter7a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL symptoms =  / SOLUTION CL CHISQ DDFM=Satterthwaite OUTPM=PredEmpty;
     RANDOM INTERCEPT / V VCORR TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovEmpty InfoCrit=FitEmpty;
RUN; TITLE1;
 
TITLE1 'Ch 7a: Testing Saturated Means by Day of Study';
PROC MIXED DATA=work.Chapter7a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID studyday;
     MODEL symptoms = studyday / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     LSMEANS studyday / DIFF=ALL CL;
RUN; TITLE1;
 
TITLE1 'Ch 7a: Testing Fixed Linear Effect of Day of Study';
PROC MIXED DATA=work.Chapter7a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL symptoms = studyday1 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitFixDayofStudy;
RUN; TITLE1;
 
TITLE1 'Ch 7a: Testing Random Linear Effect of Day of Study';
PROC MIXED DATA=work.Chapter7a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL symptoms = studyday1 / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT studyday1 / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitRandDayofStudy;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitFixDayofStudy, FitMore=FitRandDayofStudy);
 
TITLE1 'Ch 7a: Testing Saturated Means by Day of Week';
PROC MIXED DATA=work.Chapter7a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID dayofweek;
     MODEL symptoms = dayofweek / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     LSMEANS dayofweek / DIFF=ALL CL;
RUN; TITLE1;
 
TITLE1 'Ch 7a: Testing Fixed Effect of Weekend';
PROC MIXED DATA=work.Chapter7a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL symptoms = weekend / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitFixWeekend;
RUN; TITLE1;
 
TITLE1 'Ch 7a: Testing Random Effect of Weekend';
PROC MIXED DATA=work.Chapter7a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL symptoms = weekend / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT weekend / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitRandWeekend;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitFixWeekend, FitMore=FitRandWeekend);
 
TITLE1 'Eq 7a.4: Adding Sex and Age to the Model for the Means';
PROC MIXED DATA=work.Chapter7a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=ML;
     CLASS PersonID;
     MODEL symptoms = women age80 women*age80 / SOLUTION CL CHISQ DDFM=Satterthwaite COVB OUTPM=PredSexAge;
     RANDOM INTERCEPT / TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT CovParms=CovSexAge InfoCrit=FitSexAge SolutionF=FixSexAge COVB=CovBSexAge;
     CONTRAST 'Multivariate Test of Fixed Effects' women 1, age80 1, women*age80 1 / CHISQ;
     ESTIMATE 'Age Slope for Men'      age80 1 women*age80 0 / CL;
     ESTIMATE 'Age Slope for Women'    age80 1 women*age80 1 / CL;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=FitEmpty, FitMore=FitSexAge);
     * Call macro to calculate pseudo R2;
     %PseudoR2(NCov=2, CovFewer=CovEmpty, CovMore=CovSexAge);
     * Call macro to calculate Total R2 for overall model;
     %TotalR2(DV=symptoms, PredFewer=PredEmpty, PredMore=PredSexAge);
     * Call macro for regions of significance for main effects of interaction;
     %Regions(FixData=FixSexAge, CovBData=CovBSexAge, Pred=women, Mod=age80,
              ModCenter=80, Interact=women*age80, Order=3);
 
TITLE1 'Eq 7.4: Adding Sex and Age to the Model for the Means via NLMIXED';  
PROC NLMIXED DATA=work.Chapter7a METHOD=GAUSS TECH=NEWRAP GCONV=1e-12;  
* Must define all parameters to be estimated and provide start values;
* First line is fixed effects, second line is variances;
  PARMS fint=1.7 fsex=-.53 fage=.10 fsexage=-.11   
        varEint=1 vU0int=1;
* Setting up level-2 equations;
  b0i = fint + fsex*women + fage*age80 + fsexage*women*age80 + U0i; 
* Setting up level-1 equation WITHOUT level-1 residual;
  y = (b0i);
* Model for heterogeneous variances;
  varE = EXP(varEint);
  vU0  = EXP(vU0int);
* Symptoms is normally distributed with a mean of 'y' and a variance of 'VarE';
  MODEL symptoms ~ NORMAL(y,varE);  
* Defining random effects: normally distributed with means and variances;
  RANDOM U0i ~ normal([0],[vU0]) SUBJECT=PersonID;
* Requesting model-implied effects or quantities; 
  ESTIMATE 'Age Slope for Men'                          fage;
  ESTIMATE 'Age Slope for Women'                        fage+fsexage;
  ESTIMATE 'Level-1 Residual Variance'                  EXP(varEint);
  ESTIMATE 'Level-2 Random Intercept Variance'          EXP(vU0int);
  CONTRAST 'Multivariate Test of Fixed Effects in Model for Means' fsex*1, fage*1, fsex*age*1;
RUN; TITLE1; 

TITLE1 'Eq 7.5: Adding Sex and Age to Predict Heterogeneity of Level-2 Variance';  
PROC NLMIXED DATA=work.Chapter7a METHOD=GAUSS TECH=NEWRAP GCONV=1e-12;  
* Must define all parameters to be estimated and provide start values;
* First line is fixed effects, second line is variances;
  PARMS fint=1.7 fsex=-.53 fage=.10 fsexage=-.11   
        varEint=1 vU0int=1 vU0sex=0 vU0age=0 vU0sexage=0;
* Setting up level-2 equations;
  b0i = fint + fsex*women + fage*age80 + fsexage*women*age80 + U0i; 
* Setting up level-1 equation WITHOUT level-1 residual;
  y = (b0i);
* Model for heterogeneous variances;
  varE = EXP(varEint);
  vU0  = EXP(vU0int + vU0sex*women + vU0age*age80 + vU0sexage*women*age80);
* Symptoms is normally distributed with a mean of 'y' and a variance of 'VarE';
  MODEL symptoms ~ NORMAL(y,varE);  
* Defining random effects: normally distributed with means and variances;
  RANDOM U0i ~ normal([0],[vU0]) SUBJECT=PersonID;
* Requesting model-implied effects or quantities; 
  ESTIMATE 'Age Slope for Men'                                      fage;
  ESTIMATE 'Age Slope for Women'                                    fage+fsexage;
  ESTIMATE 'Level-1 Residual Variance'                              EXP(varEint);
  ESTIMATE 'Level-2 Random Intercept Variance Intercept'            EXP(vU0int);
  ESTIMATE 'Woman Effect on Random Intercept Variance for Age=80'   vU0sex;
  ESTIMATE 'Age Effect on Random Intercept Variance for Men'        vU0age;
  ESTIMATE 'Age Effect on Random Intercept Variance for Women'      vU0age+vU0sexage;
  ESTIMATE 'Woman*Age Interaction on Random Intercept Variance'     vU0sexage;
  CONTRAST 'Multivariate Test of Predictors of Random Intercept Variance'  vU0sex*1, vU0age*1, vU0sexage*1;
RUN; TITLE1; 

TITLE1 'Eq 7.6: Add Random Scale Factor - blows up'; 
PROC NLMIXED DATA=work.Chapter7a METHOD=GAUSS TECH=NEWRAP GCONV=1e-12;  
* Must define all parameters to be estimated and provide start values;
* First line is fixed effects, second line is variances;
  PARMS fint=1.7 fsex=-.53 fage=.10 fsexage=-.11   
        varEint=-.48 vU0int=.06 cU0scale=.006 vscale=.002;
* Setting up level-2 equations;
  b0i = fint + fsex*women + fage*age80 + fsexage*women*age80 + U0i; 
* Setting up level-1 equation WITHOUT level-1 residual;
  y = (b0i);
* Model for heterogeneous variances;
  varE = EXP(varEint + Scalei);
  vU0  = EXP(vU0int);
* Symptoms is normally distributed with a mean of 'y' and a variance of 'VarE';
  MODEL symptoms ~ NORMAL(y,varE);  
* Defining random effects: normally distributed with means and variances;
  RANDOM U0i Scalei ~ normal([0,0],[vU0,cU0scale,vscale]) SUBJECT=PersonID; 
RUN; TITLE1;

TITLE1 'Eq 7.7: Adding Sex and Age to Predict Heterogeneity of Level-1 Variance'; 
PROC NLMIXED DATA=work.Chapter7a METHOD=GAUSS TECH=NEWRAP GCONV=1e-12;  
* Must define all parameters to be estimated and provide start values;
* First line is fixed effects, second line is variances;
  PARMS fint=1.7 fsex=-.53 fage=.10 fsexage=-.11   
        varEint=1 varEsex=0 varEage=0 varEsexage=0 vU0int=1;
* Setting up level-2 equations;
  b0i = fint + fsex*women + fage*age80 + fsexage*women*age80 + U0i; 
* Setting up level-1 equation WITHOUT level-1 residual;
  y = (b0i);
* Model for heterogeneous variances;
  varE = EXP(varEint + varEsex*women + varEage*age80 + varEsexage*women*age80);
  vU0  = EXP(vU0int);
* Symptoms is normally distributed with a mean of 'y' and a variance of 'VarE';
  MODEL symptoms ~ NORMAL(y,varE);  
* Defining random effects: normally distributed with means and variances;
  RANDOM U0i ~ normal([0],[vU0]) SUBJECT=PersonID;
* Requesting model-implied effects or quantities; 
  ESTIMATE 'Age Slope for Men'                              fage;
  ESTIMATE 'Age Slope for Women'                            fage+fsexage;
  ESTIMATE 'Level-2 Random Intercept Variance'              EXP(vU0int);
  ESTIMATE 'Level-1 Residual Variance Intercept'            EXP(varEint);
  ESTIMATE 'Woman Effect on Residual Variance for Age=80'   varEsex;
  ESTIMATE 'Age Effect on Residual Variance for Men'        varEage;
  ESTIMATE 'Age Effect on Residual Variance for Women'      varEage+varEsexage;
  ESTIMATE 'Woman*Age Interaction on Residual Variance'     varEsexage;
  CONTRAST 'Multivariate Test of Predictors of Residual Variance'  varEsex*1, varEage*1, varEsexage*1;
RUN; TITLE1; 

****** END CHAPTER 7a MODELS ******;

* Close output directory;
ODS HTML CLOSE;

