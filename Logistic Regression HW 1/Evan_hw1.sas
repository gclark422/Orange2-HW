libname hw1 "C:\Users\97420\OneDrive\Documents\MSA Fall\Logistic Regression\Homework\Homework1_LR";

/*--------------------- Check Significance and OR for Binary Vars ---------------------------------*/
proc freq data=hw1.insurance_t;
	tables (IRA LOC INV ILS MM)*INS/
	chisq measures cl relrisk;
	title "association with INS";
run;
	
*/ 
IRA: p-value < 0.0001 (significant)   odds ratio 3.1848 (2.6419, 3.8393)
LOC: p-value = 0.4995 (insignificant) odds ratio 1.0648 (0.8873, 1.2779)
INV: p-value < 0.0001 (significant)   odds ratio 3.4720 (2.6506, 4.5480)
ILS: p-value = 0.0073 (insignificant) odds ratio 0.7452 (0.6008,0.9243)
MM:  p-value < 0.0001 (significant)   odds ratio 2.8503 (2.4949, 3.2562)
;

/* --------------------------------------------------- */
/* ------- check '.'(missing values) ----------------- */
/* --------------------------------------------------- */
proc means data=hw1.insurance_t nmiss;
	var IRA IRABAL LOC LOCBAL INV INVBAL ILS ILSBAL MM;
run;
/* Only INV and INVBAL have matching amount of missing values */



/* --------------------------------------------------- */
/* check 0 or negative values for continuous variables */
/* --------------------------------------------------- */
proc univariate data=hw1.insurance_t;
	var IRABAL LOCBAL INVBAL ILSBAL;
RUN;
/* 
IRABAL: More than 90% is 0
LOCBAL: More than 90% is 0 and 4 observations are negative
INVBAL: More than 95% is 0
ILSBAL: More than 90% is 0
*/

/* -------------------------------------------------------------*/
/* check 0 or negative values when account = "1" */
/* -------------------------------------------------------------*/
proc univariate data=hw1.insurance_t;
	var IRABAL;
	where IRA = 1;
RUN;

proc univariate data=hw1.insurance_t;
	var LOCBAL;
	where LOC = 1;
RUN;

proc univariate data=hw1.insurance_t;
	var INVBAL;
	where INV = 1;
RUN;

proc univariate data=hw1.insurance_t;
	var ILSBAL;
	where ILS = 1;
RUN;
/* 
IRABAL: More than 10% is still 0, use GAM
LOCBAL: More than 10% is still 0 and 4 observations are negative, 
	use GAM
INVBAL: More than 25% is 0, use GAM
ILSBAL: all positive now, use Box Tidwell :)
*/





/* ------------------- check  --------------------*/
proc means data=hw1.insurance_t;
	var IRA IRABAL LOC LOCBAL INV INVBAL ILS ILSBAL MM;
	where IRA = 0 and IRABAL = 0;
run;

/*--------------------- Check Assumptions for Continuous Vars ---------------------------------*/
/*------------------------------- IRABAL(GAM)--------------------------*/
proc logistic data=hw1.insurance_t alpha=0.002 plots(only)=(effect oddsratio);
	model INS(event='1') = IRABAL / clodds=pl clparm=pl;
run;
quit;
*/ 
IRABAL: p-value < 0.0001 (significant)   Wald chi-sq = 43.9315
;

Proc gam data=hw1.insurance_t plots= components(clm commonaxes);
	model INS(event = "1") = spline(IRABAL) /dist = binomial link = logit;
	where IRA = 1;
run;

/* 
DOES NOT CONVERGE;
spline(IRABAL) p-value = NA; 
*/

/*--------------------------- LOCBAL(GAM)--------------------------*/
proc logistic data=hw1.insurance_t alpha=0.002 plots(only)=(effect oddsratio);
	model INS(event='1') = LOCBAL / clodds=pl clparm=pl;
run;
quit;
*/ 
IRABAL: p-value = 0.9106 (insignificant)   Wald chi-sq = 0.0126
;

Proc gam data=hw1.insurance_t plots= components(clm commonaxes);
	model INS(event = "1") = spline(LOCBAL, df=4) /dist = binomial link = logit;
run;
/* 
DOES NOT CONVERGE!!!
spline(LOCBAL) p-value = 0.0360; 
linearity assumption met
*/

/*--------------------------- INVBAL(GAM)--------------------------*/
proc logistic data=hw1.insurance_t alpha=0.002 plots(only)=(effect oddsratio);
	model INS(event='1') = INVBAL / clodds=pl clparm=pl;
run;
quit;
*/ 
INVBAL: p-value = 0.0393(Insignificant)   Wald chi-sq = 4.2466
;

Proc gam data=hw1.insurance_t plots= components(clm commonaxes);
	model INS(event = "1") = spline(INVBAL) /dist = binomial link = logit;
run;

/* 
DOES CONVERGE;
spline(INVBAL) p-value <.0001;
Linearity Assumption NOT met 
*/


/*--------------------------- ILSBAL(GAM)--------------------------*/
proc logistic data=hw1.insurance_t alpha=0.002 plots(only)=(effect oddsratio);
	model INS(event='1') = ILSBAL / clodds=pl clparm=pl;
run;
quit;
*/ 
ILSBAL: p-value = 0.0313 (Insignificant)   Wald chi-sq = 4.6349
;

Proc gam data=hw1.insurance_t plots= components(clm commonaxes);
	model INS(event = "1") = spline(ILSBAL) /dist = binomial link = logit;
run;

/* 
DOES CONVERGE;
spline(INVBAL) p-value = 0.0011;
Linearity Assumption NOT met 
*/

/*--------------------- Count missing values for all---------------------------------*/
proc means data=hw1.insurance_t NMISS N; run;



