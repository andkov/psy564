* Output options: all can be turned on/off by adding or removing the NO;
* page number, date, centering, or page breaks, page length and width;
OPTIONS NOnumber NOdate NOcenter FormDlim=' ' PageSize=MAX LineSize=MAX;
* Eliminate SAS default titles and names of tables in output (TRACE ON to show);
TITLE; ODS TRACE OFF; ODS LISTING CLOSE;

***********************************************************************************;
*******   MACRO PROGRAMS TO AUTOMATE CALCULATIONS FOR CHAPTER 4 EXAMPLE     *******;
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

***********************************************************************************;
*******             BEGIN DATA MANIPULATION OF CHAPTER 4 EXAMPLE            *******;
*******               CHANGE "filesave" to your directory                   *******;
***********************************************************************************;

* Defining global variable for file location to be replaced in code below;
%LET filesave= C:\Dropbox\PilesOfVariance\Chapter4\SAS;
* Location for SAS files for these models (uses macro variable filesave);
LIBNAME filesave "&filesave.";
    
* Import chapter 4 stacked data;
DATA work.Chapter4; SET filesave.SAS_Chapter4; RUN;

***********************************************************************************;
*******                       BEGIN CHAPTER 4 MODELS                        *******;
***********************************************************************************;

* Open output directory to save results to;
ODS HTML FILE="&filesave.\SAS_Chapter4_Output.html"
         (URL="SAS_Chapter4_Output.html") STYLE=HTMLBlue;

TITLE1 "Chapter 4 Example: Means by StudyDay for Positive Mood outcome";
* CLASS= means per studyday, WAYS= means overall=0 and per studyday=1;
PROC MEANS MEAN STDERR MIN MAX DATA=work.Chapter4; 
     CLASS studyday;
     WAYS 0 1;
     VAR posmood;
RUN; TITLE1;

TITLE1 'Ch 4: Saturated Means, Unstructured R-Only Model';
TITLE2 'Test for mean differences across days';
PROC MIXED DATA=work.Chapter4 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID studyday;
     MODEL posmood = studyday / SOLUTION CL CHISQ DDFM=Satterthwaite;
     REPEATED studyday / R RCORR TYPE=UN SUBJECT=PersonID;
     LSMEANS studyday / DIFF=ALL CL;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 4: Empty Means, Unstructured R-Only Model';
TITLE2 'Best-fitting and least parsimonious baseline';
PROC MIXED DATA=work.Chapter4 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID studyday;
     MODEL posmood =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     REPEATED studyday / R RCORR TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=UN;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 4: Empty Means, Compound Symmetry R-Only Model';
TITLE2 'Worst-fitting and most parsimonious baseline';
PROC MIXED DATA=work.Chapter4 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID studyday;
     MODEL posmood =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     REPEATED studyday / R RCORR TYPE=CS SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=CS;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=CS, FitMore=UN);
 
TITLE1 'Ch 4: Empty Means, Compound Symmetry Heterogeneous R-Only Model';
PROC MIXED DATA=work.Chapter4 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID studyday;
     MODEL posmood =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     REPEATED studyday / R RCORR TYPE=CSH SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=CSH;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=CSH, FitMore=UN);
     %FitTest(FitFewer=CS, FitMore=CSH);
 
TITLE1 'Ch 4: Empty Means, First-Order Auto-Regressive R-Only Model';
PROC MIXED DATA=work.Chapter4 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID studyday;
     MODEL posmood =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     REPEATED studyday / R RCORR TYPE=AR(1) SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=AR1;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=AR1, FitMore=UN);
 
TITLE1 'Ch 4: Empty Means, First-Order Auto-Regressive Heterogeneous R-Only Model';
PROC MIXED DATA=work.Chapter4 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID studyday;
     MODEL posmood =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     REPEATED studyday / R RCORR TYPE=ARH(1) SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=ARH1;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=ARH1, FitMore=UN);
     %FitTest(FitFewer=AR1, FitMore=ARH1);
 
TITLE1 'Ch 4: Empty Means, n-1 Lag Toeplitz R-Only Model';
PROC MIXED DATA=work.Chapter4 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID studyday;
     MODEL posmood =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     REPEATED studyday / R RCORR TYPE=TOEP(7) SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=TOEP7;
RUN; TITLE1;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=TOEP7, FitMore=UN);
     %FitTest(FitFewer=CS, FitMore=TOEP7);
     %FitTest(FitFewer=AR1, FitMore=TOEP7);
 
TITLE1 'Ch 4: Empty Means, n-1 Lag Toeplitz Heterogeneous R-Only Model';
TITLE2 'Worst-fitting and most parsimonious baseline';
PROC MIXED DATA=work.Chapter4 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID studyday;
     MODEL posmood =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     REPEATED studyday / R RCORR TYPE=TOEPH(7) SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=TOEPH7;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=TOEPH7, FitMore=UN);
     %FitTest(FitFewer=TOEP7, FitMore=TOEPH7);
     %FitTest(FitFewer=CSH, FitMore=TOEPH7);
     %FitTest(FitFewer=ARH1, FitMore=TOEPH7);
 
TITLE1 'Ch 4: Empty Means, Random Intercept in G';
TITLE2 'n-1 Order Unstructured R';
PROC MIXED DATA=work.Chapter4 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID studyday;
     MODEL posmood =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / G GCORR V VCORR TYPE=UN SUBJECT=PersonID;
     REPEATED studyday / R RCORR TYPE=UN(6) SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=RIUN6;
RUN; TITLE1; TITLE2;
 
TITLE1 'Ch 4: Empty Means, Random Intercept in G';
TITLE2 'Diagonal R';
PROC MIXED DATA=work.Chapter4 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID studyday;
     MODEL posmood =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / G GCORR V VCORR TYPE=UN SUBJECT=PersonID;
     REPEATED studyday / R RCORR TYPE=VC SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=RIDIAG;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=RIDIAG, FitMore=UN);
 
TITLE1 'Ch 4: Empty Means, Random Intercept in G';
TITLE2 'Diagonal Heterogeneous R';
PROC MIXED DATA=work.Chapter4 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID studyday;
     MODEL posmood =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / G GCORR V VCORR TYPE=UN SUBJECT=PersonID;
     REPEATED studyday / R RCORR TYPE=UN(1) SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=RIDIAGH;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=RIDIAGH, FitMore=UN);
     %FitTest(FitFewer=RIDIAG, FitMore=RIDIAGH);
 
TITLE1 'Ch 4: Empty Means, Random Intercept in G';
TITLE2 'First-Order Autoregressive R';
PROC MIXED DATA=work.Chapter4 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID studyday;
     MODEL posmood =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / G GCORR V VCORR TYPE=UN SUBJECT=PersonID;
     REPEATED studyday / R RCORR TYPE=AR(1) SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=RIAR1;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=RIAR1, FitMore=UN);
     %FitTest(FitFewer=AR1, FitMore=RIAR1);
     %FitTest(FitFewer=RIDIAG, FitMore=RIAR1);
 
TITLE1 'Ch 4: Empty Means, Random Intercept in G';
TITLE2 'First-Order Autoregressive Heterogeneous R';
PROC MIXED DATA=work.Chapter4 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID studyday;
     MODEL posmood =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / G GCORR V VCORR TYPE=UN SUBJECT=PersonID;
     REPEATED studyday / R RCORR TYPE=ARH(1) SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=RIARH1;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=RIARH1, FitMore=UN);
     %FitTest(FitFewer=RIAR1, FitMore=RIARH1);
 
TITLE1 'Ch 4: Empty Means, Random Intercept in G';
TITLE2 '1-Lag Toeplitz R';
PROC MIXED DATA=work.Chapter4 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID studyday;
     MODEL posmood =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / G GCORR V VCORR TYPE=UN SUBJECT=PersonID;
     REPEATED studyday / R RCORR TYPE=TOEP(2) SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=RITOEP2;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=RITOEP2, FitMore=UN);
     %FitTest(FitFewer=RIDIAG, FitMore=RITOEP2);
 
TITLE1 'Ch 4: Empty Means, Random Intercept in G';
TITLE2 '1-Lag Toeplitz Heterogeneous R';
PROC MIXED DATA=work.Chapter4 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID studyday;
     MODEL posmood =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / G GCORR V VCORR TYPE=UN SUBJECT=PersonID;
     REPEATED studyday / R RCORR TYPE=TOEPH(2) SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=RITOEPH2;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=RITOEPH2, FitMore=UN);
     %FitTest(FitFewer=RITOEP2, FitMore=RITOEPH2);
     %FitTest(FitFewer=RIDIAGH, FitMore=RITOEPH2);
 
TITLE1 'Ch 4: Empty Means, Random Intercept in G';
TITLE2 '2-Lag Toeplitz R';
PROC MIXED DATA=work.Chapter4 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID studyday;
     MODEL posmood =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / G GCORR V VCORR TYPE=UN SUBJECT=PersonID;
     REPEATED studyday / R RCORR TYPE=TOEP(3) SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=RITOEP3;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=RITOEP3, FitMore=UN);
     %FitTest(FitFewer=RITOEP2, FitMore=RITOEP3);
 
TITLE1 'Ch 4: Empty Means, Random Intercept in G';
TITLE2 '2-Lag Toeplitz Heterogeneous R';
PROC MIXED DATA=work.Chapter4 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID studyday;
     MODEL posmood =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / G GCORR V VCORR TYPE=UN SUBJECT=PersonID;
     REPEATED studyday / R RCORR TYPE=TOEPH(3) SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=RITOEPH3;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=RITOEPH3, FitMore=UN);
     %FitTest(FitFewer=RITOEP3, FitMore=RITOEPH3);
     %FitTest(FitFewer=RITOEPH2, FitMore=RITOEPH3);
 
TITLE1 'Ch 4: Empty Means, Random Intercept in G';
TITLE2 '3-Lag Toeplitz R';
PROC MIXED DATA=work.Chapter4 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID studyday;
     MODEL posmood =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / G GCORR V VCORR TYPE=UN SUBJECT=PersonID;
     REPEATED studyday / R RCORR TYPE=TOEP(4) SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=RITOEP4;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=RITOEP4, FitMore=UN);
     %FitTest(FitFewer=RITOEP3, FitMore=RITOEP4);
 
TITLE1 'Ch 4: Empty Means, Random Intercept in G';
TITLE2 '3-Lag Toeplitz Heterogeneous R';
PROC MIXED DATA=work.Chapter4 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID studyday;
     MODEL posmood =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / G GCORR V VCORR TYPE=UN SUBJECT=PersonID;
     REPEATED studyday / R RCORR TYPE=TOEPH(4) SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=RITOEPH4;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=RITOEPH4, FitMore=UN);
     %FitTest(FitFewer=RITOEP4, FitMore=RITOEPH4);
     %FitTest(FitFewer=RITOEPH3, FitMore=RITOEPH4);
 
TITLE1 'Ch 4: Empty Means, Random Intercept in G';
TITLE2 '4-Lag Toeplitz R';
PROC MIXED DATA=work.Chapter4 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID studyday;
     MODEL posmood =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / G GCORR V VCORR TYPE=UN SUBJECT=PersonID;
     REPEATED studyday / R RCORR TYPE=TOEP(5) SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=RITOEP5;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=RITOEP5, FitMore=UN);
     %FitTest(FitFewer=RITOEP4, FitMore=RITOEP5);
 
TITLE1 'Ch 4: Empty Means, Random Intercept in G';
TITLE2 '4-Lag Toeplitz Heterogeneous R';
PROC MIXED DATA=work.Chapter4 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID studyday;
     MODEL posmood =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / G GCORR V VCORR TYPE=UN SUBJECT=PersonID;
     REPEATED studyday / R RCORR TYPE=TOEPH(5) SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=RITOEPH5;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=RITOEPH5, FitMore=UN);
     %FitTest(FitFewer=RITOEP5, FitMore=RITOEPH5);
     %FitTest(FitFewer=RITOEPH4, FitMore=RITOEPH5);
 
TITLE1 'Ch 4: Empty Means, Random Intercept in G';
TITLE2 '5-Lag Toeplitz R';
PROC MIXED DATA=work.Chapter4 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID studyday;
     MODEL posmood =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / G GCORR V VCORR TYPE=UN SUBJECT=PersonID;
     REPEATED studyday / R RCORR TYPE=TOEP(6) SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=RITOEP6;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=RITOEP6, FitMore=UN);
     %FitTest(FitFewer=RITOEP5, FitMore=RITOEP6);
 
TITLE1 'Ch 4: Empty Means, Random Intercept in G';
TITLE2 '5-Lag Toeplitz Heterogeneous R';
PROC MIXED DATA=work.Chapter4 COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID studyday;
     MODEL posmood =  / SOLUTION CL CHISQ DDFM=Satterthwaite;
     RANDOM INTERCEPT / G GCORR V VCORR TYPE=UN SUBJECT=PersonID;
     REPEATED studyday / R RCORR TYPE=TOEPH(6) SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=RITOEPH6;
RUN; TITLE1; TITLE2;
     * Call macro to calculate LRT for nested models;
     %FitTest(FitFewer=RITOEPH6, FitMore=UN);
     %FitTest(FitFewer=RITOEP6, FitMore=RITOEPH6);
     %FitTest(FitFewer=RITOEPH5, FitMore=RITOEPH6);
 
****** END CHAPTER 4 MODELS ******;

* Close output directory;
ODS HTML CLOSE;

