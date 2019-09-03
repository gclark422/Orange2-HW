libname hw "C:\Users\grant\Desktop\IAA\Logistic Regression\HW1";

proc univariate data=hw.insurance_t;
	var LORES HMVAL AGE CRSCORE;
run;

proc means data=hw.insurance_t nmiss;
	var LORES;
run;

/* Looking at binary variables */
proc freq data=hw.insurance_t;
	table INS*HMOWN / chisq measures cl;
run;

proc freq data=hw.insurance_t;
	table INS*MOVED / chisq measures cl;
run;

proc freq data=hw.insurance_t;
	table INS*INAREA / chisq measures cl;
run;

/*********************************************************************************/

/* LORES analysis */
data loreslog;
	set hw.insurance_t;
	rlogr = lores * log(lores);
run;

proc logistic data=loreslog;
	model INS(event='1') = rlogr LORES / clodds=pl clparm=pl;
run;
quit;

/* Get wald chisq and p */
proc logistic data=hw.insurance_t plots(only)=(effect oddsratio);
	model INS(event='1') = LORES / clodds=pl clparm=pl;
run;
/*********************************************************************************/

/* HMVAL analysis */
data hmvallog;
	set hw.insurance_t;
	hlogh = HMVAL * log(HMVAL);
run;

proc logistic data=hmvallog;
	model INS(event='1') = hlogh HMVAL / clodds=pl clparm=pl;
run;
quit;

proc logistic data=hw.insurance_t plots(only) = (effect oddsratio);
	model INS(event='1') = HMVAL / clodds=pl clparm=pl;
run; quit;

/*********************************************************************************/

/* AGE analysis */
data agelog;
	set hw.insurance_t;
	aloga = AGE * log(AGE);
run;

proc logistic data=agelog;
	model INS(event='1') = aloga AGE / clodds=pl clparm=pl;
run;
quit;

proc logistic data=hw.insurance_t plots(only) = (effect oddsratio);
	model INS(event='1') = AGE / clodds=pl clparm=pl;
run; quit;

/*********************************************************************************/

/* CRSCORE analysis */
data crlog;
	set hw.insurance_t;
	clogc = CRSCORE * log(CRSCORE);
run;

proc logistic data=crlog;
	model INS(event='1') = clogc CRSCORE / clodds=pl clparm=pl;
run;
quit;

proc logistic data=hw.insurance_t plots(only) = (effect oddsratio);
	model INS(event='1') = CRSCORE / clodds=pl clparm=pl;
run; quit;

/*********************************************************************************/

/* Pearsons on BRANCH and RES (nominal variables) */
proc freq data=hw.insurance_t;
	tables (BRANCH RES)*INS / chisq;
run;

