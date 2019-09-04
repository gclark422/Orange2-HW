/* Initialize the lrhw1 libref */

libname lrhw1 "C:\Users\Asus\Documents\Learning\Advanced Analytics\Logistic Regression\Homework\Homework 1";

proc contents data=lrhw1.insurance_t;
run;

/* ACCTAGE: Continuous - Box Tidwell */
proc univariate data=lrhw1.insurance_t;
	var acctage;
run;

proc logistic data=lrhw1.insurance_t plots(only)=(oddsratio);
	model ins(event='1') = acctage / clodds=pl clparm=pl;
run;

data acctagetemp;
	set lrhw1.insurance_t;
	logACCTAGE = acctage*log(acctage);
run;

proc logistic data=temp plots(only)=(oddsratio);
	model ins(event='1') = acctage logacctage / clodds=pl clparm=pl;
run;
	

/*DDA: Binary - Mantel Haenzel*/
proc freq data=lrhw1.insurance_t;
	table dda;
run;

proc freq data=lrhw1.insurance_t;
	Tables ins*dda / chisq expected cellchi2 nocol nopercent relrisk;
run;

/*DDABAL: Continuous - Should be GAM, but doesn't converge, so instead Box Tidwell with values >0 */
proc univariate data=lrhw1.insurance_t;
	var ddabal;
	where dda = 1;
run;

proc gam data=lrhw1.insurance_t plots=components(clm commonaxes);
	model ins(event = '1') = spline(DDABAL, df=4) / dist = binomial link = logit;
	where dda = 1;
run;

data ddabaltemp;
	set lrhw1.insurance_t;
	logDDABAL = ddabal*log(ddabal);
	where dda = 1 and ddabal>0;
run;

proc logistic data=temp plots(only)=(oddsratio);
	model ins(event='1') = acctage logacctage / clodds=pl clparm=pl;
run;

proc logistic data=lrhw1.insurance_t plots(only)=(oddsratio);
	model ins(event='1') = ddabal / clodds=pl clparm=pl;
	*units ddabal = 1000;
run;

/*DEP: Continuous - GAM*/
proc univariate data=lrhw1.insurance_t;
	var dep;
	where dda=1;
run;

proc gam data=lrhw1.insurance_t plots=components(clm commonaxes);
	model ins(event = '1') = spline(DEP, df=4) / dist = binomial link = logit;
	where dda = 1;
run;

proc logistic data=lrhw1.insurance_t plots(only)=(oddsratio);
	model ins(event='1') = DEP / clodds=pl clparm=pl;
	where dda=1;
run;

/*DEPAMT: Continuous - GAM*/
proc univariate data=lrhw1.insurance_t;
	var depamt;
	where dda = 1;
run;

proc gam data=lrhw1.insurance_t plots=components(clm commonaxes);
	model ins(event = '1') = spline(DEPAMT, df=4) / dist = binomial link = logit;
	where dda=1;
run;

proc logistic data=lrhw1.insurance_t plots(only)=(oddsratio);
	model ins(event='1') = DEPAMT / clodds=pl clparm=pl;
	where dda=1;
run;

/*CASHBK: Ordinal (3 Levels) - Mantel Haenzel*/
proc freq data=lrhw1.insurance_t;
	table cashbk;
	where dda = 1;
run;

proc freq data=lrhw1.insurance_t;
	tables ins*CASHBK / chisq expected cellchi2 nocol nopercent relrisk;
	where dda = 1;
run;

proc logistic data=lrhw1.insurance_t;
	class cashbk(ref='0') / param = reference;
	model ins(event='1') = cashbk / clodds = pl clparm=pl;
	where dda = 1;
run;

/*CHECKS: Continuous - GAM*/
proc univariate data=lrhw1.insurance_t;
	var checks;
	where dda=1;
run;

proc gam data=lrhw1.insurance_t plots=components(clm commonaxes);
	model ins(event = '1') = spline(CHECKS, df=4) / dist = binomial link = logit;
	where dda=1;
run;

proc logistic data=lrhw1.insurance_t plots(only)=(oddsratio);
	model ins(event='1') = CHECKS / clodds=pl clparm=pl;
	where dda=1;
run;

/*DIRDEP: Binary - Mantel Haenzel*/
proc freq data=lrhw1.insurance_t;
	table dirdep;
	where dda=1;
run;

proc freq data=lrhw1.insurance_t;
	tables ins*DIRDEP / chisq expected cellchi2 nocol nopercent relrisk;
	where dda=1;
run;

/*NSF: Binary - Mantel Haenzel*/
proc freq data=lrhw1.insurance_t;
	table NSF;
	where dda=1;
run;

proc freq data=lrhw1.insurance_t;
	tables ins*NSF / chisq expected cellchi2 nocol nopercent relrisk;
	where dda=1;
run;

/*NSFAMT: Continuous - GAM*/
proc univariate data=lrhw1.insurance_t;
	var NSFAMT;
run;

proc gam data=lrhw1.insurance_t plots=components(clm commonaxes);
	model ins(event = '1') = spline(NSFAMT, df=4) / dist = binomial link = logit;
run;

proc logistic data=lrhw1.insurance_t plots(only)=(oddsratio);
	model ins(event='1') = NSFAMT / clodds=pl clparm=pl;
run;




/*DDABAL Box Tidwell */
proc logistic data=temp plots(only)=(oddsratio);
	model ins(event='1') = acctage logacctage / clodds=pl clparm=pl;
	where dda = 1 and ddabal >0;
run;

/* Verifying that only those who have Direct Desposit also have a Checking Account */
proc freq data=lrhw1.insurance_t;
	tables dda*dirdep;
run;
	
/* Looking for multi-collinearity 
HMVAL DDABAL at .66 
CHECKS DEP at .63 
POSAMT POS at .85 
MTGBAL CCBAL at .95
HMVAL INCOME at .68 */
proc corr data=lrhw1.insurance_t;
	var acctage ddabal dep depamt checks nsfamt phone teller savbal atmamt pos posamt cdbal irabal locbal invbal ilsbal mmbal mtgbal ccbal income lores hmval age crscore;
	where dda=1 or sav=1 or atm=1 or pos=1 or cd=1 or ira=1 or loc=1 or inv=1 or ils=1 or mm=1 or mtg=1 or cc=1 or hmown=1;
run;
