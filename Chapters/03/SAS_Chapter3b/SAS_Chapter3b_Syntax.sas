* Output options: all can be turned on/off by adding or removing the NO;
* page number, date, centering, or page breaks, page length and width;
OPTIONS NOnumber NOdate NOcenter FormDlim=' ' PageSize=MAX LineSize=MAX;
* Eliminate SAS default titles and names of tables in output (TRACE ON to show);
TITLE; ODS TRACE OFF; ODS LISTING CLOSE;

***********************************************************************************;
*******  MACRO PROGRAMS TO AUTOMATE CALCULATIONS FOR CHAPTER 3b EXAMPLE     *******;
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
*******     BEGIN DATA MANIPULATION OF CHAPTER 3b SIX-OCCASION EXAMPLE      *******;
*******               CHANGE "filesave" to your directory                   *******;
***********************************************************************************;

* Defining global variable for file location to be replaced in code below;
%LET filesave= C:\Dropbox\PilesOfVariance\Chapter3b\SAS;
* Location for SAS files for these models (uses macro variable filesave);
LIBNAME filesave "&filesave.";
    
* Import chapter 3 six-occasion stacked data;
DATA work.Chapter3b; SET filesave.SAS_Chapter3b; RUN;

***********************************************************************************;
*******                BEGIN CHAPTER 3b SIX-OCCASION MODELS                 *******;
***********************************************************************************;

* Open output directory to save results to;
ODS HTML FILE="&filesave.\SAS_Chapter3b_Output.html"
         (URL="SAS_Chapter3b_Output.html") STYLE=HTMLBlue;

TITLE1 "Chapter 3b Example: Means by session for RT outcome";
* CLASS= means per session, WAYS= means overall=0 and per session=1;
PROC MEANS MEAN STDERR MIN MAX DATA=work.Chapter3b; 
     CLASS session;
     WAYS 0 1;
     VAR rt;
RUN; TITLE1;

TITLE1 'Eq 3b.10: Between-Person Independent ANOVA';
PROC MIXED DATA=work.Chapter3b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID session;
     MODEL rt = session / SOLUTION CL CHISQ DDFM=Satterthwaite;
     REPEATED session / R RCORR TYPE=VC SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitBPANOVA;
     LSMEANS session / DIFF=ALL CL;
RUN; TITLE1;
 
TITLE1 'Eq 3b.10: Univariate Repeated Measures ANOVA';
PROC MIXED DATA=work.Chapter3b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID session;
     MODEL rt = session / SOLUTION CL CHISQ DDFM=Satterthwaite;
     REPEATED session / R RCORR TYPE=CS SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitUnivANOVA;
     LSMEANS session / DIFF=ALL CL;
RUN; TITLE1;
     %FitTest(FitFewer=FitBPANOVA, FitMore=FitUnivANOVA);
 
TITLE1 'Eq 3b.10: Multivariate Repeated Measures ANOVA';
PROC MIXED DATA=work.Chapter3b COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID session;
     MODEL rt = session / SOLUTION CL CHISQ DDFM=Satterthwaite;
     REPEATED session / R RCORR TYPE=UN SUBJECT=PersonID;
     ODS OUTPUT InfoCrit=FitMultivANOVA;
     LSMEANS session / DIFF=ALL CL;
RUN; TITLE1;
     %FitTest(FitFewer=FitUnivANOVA, FitMore=FitMultivANOVA);
 
****** END CHAPTER 3b MODELS ******;

* Close output directory;
ODS HTML CLOSE;

