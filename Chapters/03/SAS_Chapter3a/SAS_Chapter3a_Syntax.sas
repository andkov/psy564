* Output options: all can be turned on/off by adding or removing the NO;
* page number, date, centering, or page breaks, page length and width;
OPTIONS NOnumber NOdate NOcenter FormDlim=' ' PageSize=MAX LineSize=MAX;
* Eliminate SAS default titles and names of tables in output (TRACE ON to show);
TITLE; ODS TRACE OFF; ODS LISTING CLOSE;

***********************************************************************************;
*******     BEGIN DATA MANIPULATION OF CHAPTER 3a TWO-OCCASION EXAMPLE      *******;
*******               CHANGE "filesave" to your directory                   *******;
***********************************************************************************;

* Defining global variable for file location to be replaced in code below;
%LET filesave= C:\Dropbox\PilesOfVariance\Chapter3a\SAS;
* Location for SAS files for these models (uses macro variable filesave);
LIBNAME filesave "&filesave.";
    
* Import and stack chapter 3 two-occasion multivariate data;
* Create new variable on left from old variable on right, OUTPUT writes data;
DATA work.Chapter3a; SET filesave.SAS_Chapter3a;
time=1; outcome=outcome1; OUTPUT;
time=2; outcome=outcome2; OUTPUT;
DROP outcome1 outcome2; 
LABEL 
time = "time: Occasion (1=pre-test, 2=post-test)"
outcome = "outcome: Learning Outcome";  
RUN;

* Center predictors for analysis;
DATA work.Chapter3a; SET work.Chapter3a; 
time1 = time - 1;  
treat = group - 1; 
LABEL 
time1 = "time1: Time (0=pre-test, 1=post-test)"
treat = "treat: Treatment Group (0=control, 1=treatment)";
RUN;

***********************************************************************************;
*******               BEGIN CHAPTER 3a TWO-OCCASION MODELS                  *******;
***********************************************************************************;

* Open output directory to save results to;
ODS HTML FILE="&filesave.\SAS_Chapter3a_Output.html"
         (URL="SAS_Chapter3a_Output.html") STYLE=HTMLBlue;

* CLASS= means per group and time, WAYS= means overall=0 and per category=1;
TITLE1 "Chapter 3a Example: Means by group and time for learning outcome";
PROC MEANS MEAN STDERR MIN MAX DATA=work.Chapter3a; 
     CLASS group time;
     WAYS 0 1;
     VAR outcome;
RUN; TITLE1;

TITLE1 'Eq 3a.1: Empty Between-Person Model';
PROC MIXED DATA=work.Chapter3a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID time;
     MODEL outcome =  / SOLUTION CL CHISQ DDFM=BW;
     REPEATED time / R RCORR TYPE=VC SUBJECT=PersonID;
RUN; TITLE1;
 
TITLE1 'Eq 3a.2: Empty Within-Person Model';
PROC MIXED DATA=work.Chapter3a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID time;
     MODEL outcome =  / SOLUTION CL CHISQ DDFM=BW;
     REPEATED time / R RCORR TYPE=CS SUBJECT=PersonID;
RUN; TITLE1;
 
TITLE1 'Eq 3a.7: Conditional Between-Person Model (top of Eq. 3.7)';
TITLE2 'Manual Contrasts for Time and Group';
PROC MIXED DATA=work.Chapter3a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID time;
     MODEL outcome = time1 treat time1*treat / SOLUTION CL CHISQ DDFM=BW;
     REPEATED time / R RCORR TYPE=VC SUBJECT=PersonID;
     ESTIMATE 'Mean: Control Group at Pre-Test'       intercept 1 time1 0 treat 0 time1*treat 0 / CL;
     ESTIMATE 'Mean: Control Group at Post-Test'      intercept 1 time1 1 treat 0 time1*treat 0 / CL;
     ESTIMATE 'Mean: Treatment Group at Pre-Test'     intercept 1 time1 0 treat 1 time1*treat 0 / CL;
     ESTIMATE 'Mean: Treatment Group at Post-Test'    intercept 1 time1 1 treat 1 time1*treat 1 / CL;
     ESTIMATE 'Time  Effect for Control Group'        time1 1 time1*treat 0 / CL;
     ESTIMATE 'Time  Effect for Treatment Group'      time1 1 time1*treat 1 / CL;
     ESTIMATE 'Group Effect at Pre-Test'              treat 1 time1*treat 0 / CL;
     ESTIMATE 'Group Effect at Post-Test'             treat 1 time1*treat 1 / CL;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 3a.7: Conditional Within-Person Model (bottom of Eq. 3.7)';
TITLE2 'Manual Contrasts for Time and Group';
PROC MIXED DATA=work.Chapter3a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID time;
     MODEL outcome = time1 treat time1*treat / SOLUTION CL CHISQ DDFM=BW;
     REPEATED time / R RCORR TYPE=CS SUBJECT=PersonID;
     ESTIMATE 'Mean: Control Group at Pre-Test'       intercept 1 time1 0 treat 0 time1*treat 0 / CL;
     ESTIMATE 'Mean: Control Group at Post-Test'      intercept 1 time1 1 treat 0 time1*treat 0 / CL;
     ESTIMATE 'Mean: Treatment Group at Pre-Test'     intercept 1 time1 0 treat 1 time1*treat 0 / CL;
     ESTIMATE 'Mean: Treatment Group at Post-Test'    intercept 1 time1 1 treat 1 time1*treat 1 / CL;
     ESTIMATE 'Time  Effect for Control Group'        time1 1 time1*treat 0 / CL;
     ESTIMATE 'Time  Effect for Treatment Group'      time1 1 time1*treat 1 / CL;
     ESTIMATE 'Group Effect at Pre-Test'              treat 1 time1*treat 0 / CL;
     ESTIMATE 'Group Effect at Post-Test'             treat 1 time1*treat 1 / CL;
RUN; TITLE1; TITLE2;
 
TITLE1 'Eq 3a.7: Conditional Within-Person Model (bottom of Eq. 3.7)';
TITLE2 'Categorical Predictors for Time and Group';
PROC MIXED DATA=work.Chapter3a COVTEST NOCLPRINT NAMELEN=100 IC METHOD=REML;
     CLASS PersonID time time1 treat;
     MODEL outcome = time1 treat time1*treat / SOLUTION CL CHISQ DDFM=BW;
     REPEATED time / R RCORR TYPE=CS SUBJECT=PersonID;
     LSMEANS time1*treat / SLICE=time1 SLICE=treat DIFF=ALL CL;
RUN; TITLE1; TITLE2;
 
****** END CHAPTER 3a MODELS ******;

* Close output directory;
ODS HTML CLOSE;

