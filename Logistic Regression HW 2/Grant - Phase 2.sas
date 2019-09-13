libname logreg "C:\Users\97420\OneDrive\Documents\MSA Fall\Logistic Regression\Homework\Homework2_LR";

/***** Check missing values ******/

proc means data=logreg.insurance_t_bin nmiss;
	var MOVED LORES_Bin INAREA HMVAL_Bin HMOWN CRSCORE_Bin AGE_Bin;
run;

proc freq data=logreg.insurance_t_bin;
	table ins*BRANCH;
run;

/* Extra col for HMOWN */
data logreg.insurance_t_inv;
	set logreg.insurance_t_bin;
	IF HMOWN = . then HMOWN = -1;
RUN;

proc freq data=logreg.insurance_t_inv;
	tables HMOWN HMOWN*INS;
RUN;

proc freq data=logreg.insurance_t_inv;
	tables (RES MOVED LORES_Bin INAREA HMVAL_Bin HMOWN CRSCORE_Bin AGE_Bin BRANCH)*INS / nopercent nocol nocum norow;
run;
/* No sep concerns */

/* Check sig */
proc freq data=logreg.insurance_t_inv;
	tables (RES MOVED LORES_Bin INAREA HMVAL_Bin HMOWN CRSCORE_Bin AGE_Bin BRANCH)*INS/
	chisq measures cl relrisk;
	title "association with INS";
run;
	