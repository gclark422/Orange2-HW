/* 
ACCTAGE 
DDA 
DDABAL 
DEP 
DEPAMT 
CASHBK 
CHECKS 
DIRDEP 
NSF 
NSFAMT 
*/

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
	

/*DDA: Binary - Mantel Haenszel*/
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

proc logistic data=ddabaltemp plots(only)=(oddsratio);
	model ins(event='1') = ddabal logDDABAL / clodds=pl clparm=pl;
run;

proc logistic data=lrhw1.insurance_t plots(only)=(oddsratio);
	model ins(event='1') = ddabal / clodds=pl clparm=pl;
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
run;

/*CASHBK: Ordinal (3 Levels) - Fisher's Exact Test since more than 20% of expected values <5*/
proc freq data=lrhw1.insurance_t;
	table cashbk;
	where dda = 1;
run;

proc freq data=lrhw1.insurance_t;
	tables ins*CASHBK / chisq expected cellchi2 nocol nopercent relrisk;
	exact fisher;
run;

proc logistic data=lrhw1.insurance_t;
	class cashbk(ref='0') / param = reference;
	model ins(event='1') = cashbk / clodds = pl clparm=pl;
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
run;

/*DIRDEP: Binary - Mantel Haenszel*/
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

/*NSFAMT: Continuous - Box Tidwell*/
proc univariate data=lrhw1.insurance_t;
	var NSFAMT;
	where dda=1 and nsf=1;
run;

data nsfamttemp;
	set lrhw1.insurance_t;
	logNSFAMT = nsfamt*log(nsfamt);
	where dda=1 and nsf=1;
run;

proc logistic data=nsfamttemp plots(only)=(oddsratio);
	model ins(event='1') = nsfamt logNSFAMT / clodds=pl clparm=pl;
run;

proc logistic data=lrhw1.insurance_t plots(only)=(oddsratio);
	model ins(event='1') = NSFAMT / clodds=pl clparm=pl;
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

/******************************************************************************************************/

/*
PHONE 
TELLER 
SAV 
SAVBAL 
ATM 
ATMAMT 
POS 
POSAMT 
CD 
CDBAL 
*/

/*--------------exploring phone variable-----------------------*/
proc freq data = logist1.insurance_t;
tables phone;
run;

/*logistic regression*/
proc logistic data=logist1.insurance_t alpha=0.05
 plots(only)=(effect oddsratio);
 model ins(event='1')=phone / clodds=pl;
run;

/*MH chi sq*/
proc freq data=logist1.insurance_t;
 tables (phone)*ins
 / chisq expected cellchi2 nocol nopercent
 relrisk;
run;

/*GAM*/
proc gam data=logist1.insurance_t
plots = components(clm commonaxes);
model ins(event='1') = spline(phone, df=4)
/ dist = binomial link = logit;
run;

/*significance*/
proc logistic data=logist1.insurance_t plots(only)=(oddsratio);
	model ins(event='1') = phone / clodds=pl clparm=pl;
run;

/*---------------------exploring teller variable---------------------*/
proc freq data = logist1.insurance_t;
tables teller;
run;

/*logistic regression*/
proc logistic data=logist1.insurance_t alpha=0.05
 plots(only)=(effect oddsratio);
 model ins(event='1')=teller / clodds=pl;
run;

/*MH chi sq*/
proc freq data=logist1.insurance_t;
 tables (teller)*ins
 / chisq expected cellchi2 nocol nopercent
 relrisk;
run;

/*GAM*/
proc gam data=logist1.insurance_t
plots = components(clm commonaxes);
model ins(event='1') = spline(teller, df=4)
/ dist = binomial link = logit;
run;

/*significance*/
proc logistic data=logist1.insurance_t plots(only)=(oddsratio);
	model ins(event='1') = teller / clodds=pl clparm=pl;
run;

/*------------------exploring sav variable-------------------------*/
proc freq data = logist1.insurance_t;
tables sav;
run;

/*logistic regression*/
proc logistic data=logist1.insurance_t alpha=0.05
 plots(only)=(effect oddsratio);
 model ins(event='1')=sav / clodds=pl;
run;

/*MH chi sq*/
proc freq data=logist1.insurance_t;
 tables (sav)*ins
 / chisq expected cellchi2 nocol nopercent
 relrisk;
run;

/*-------------------------------exploring savbal variable---------------------------*/
proc freq data = logist1.insurance_t;
tables savbal;
where sav = 1;
run;

/*box tidwell for savbal*/
data logist1.insurance_t;
set logist1.insurance_t;
slogs = savbal*log(savbal);
where sav = 1
run;
proc logistic data=logist1.insurance_t plots(only)=(oddsratio);
model ins(event='1') = savbal slogs /
clodds=pl clparm=pl;
where sav = 1;
run;
quit;

proc logistic data=logist1.insurance_t plots(only)=(oddsratio);
	model ins(event='1') = savbal / clodds=pl clparm=pl;
	where sav = 1;
run;

/*--------------------------exploring atm variable-----------------------*/
proc freq data = logist1.insurance_t;
tables atm;
run;

/*logistic regression*/
proc logistic data=logist1.insurance_t alpha=0.05
 plots(only)=(effect oddsratio);
 model ins(event='1')=atm / clodds=pl;
 *title 'LOGISTIC MODEL (1):Bonus=Basement_Area';
run;

/*MH chi sq*/
proc freq data=logist1.insurance_t;
 tables (atm)*ins
 / chisq expected cellchi2 nocol nopercent
 relrisk;
run;

/*---------------------------------exploring atmamt variable--------------------------------*/
proc freq data = logist1.insurance_t;
tables atmamt;
where atm = 1;
run;

/*GAM*/
proc gam data=logist1.insurance_t
plots = components(clm commonaxes);
model ins(event='1') = spline(atmamt, df=4)
/ dist = binomial link = logit;
where atm = 1;
run;

/*Box tidwell for atmamt*/ 
data logist1.insurance_t;
set logist1.insurance_t;
aloga = atmamt*log(atmamt);
run;
proc logistic data=logist1.insurance_t plots(only)=(oddsratio);
model ins(event='1') = atmamt aloga /
clodds=pl clparm=pl;
run;
quit;

/*significance*/
proc logistic data=logist1.insurance_t plots(only)=(oddsratio);
	model ins(event='1') = atmamt / clodds=pl clparm=pl;
	where atm = 1;
run;

/*----------------------exploring pos variable-----------------------*/
proc freq data = logist1.insurance_t;
tables pos;
run;

proc freq data=logist1.insurance_t;
 tables (pos)*ins
 / chisq expected cellchi2 nocol nopercent
 relrisk;
run;

/*GAM*/
proc gam data=logist1.insurance_t
plots = components(clm commonaxes);
model ins(event='1') = spline(pos, df=4)
/ dist = binomial link = logit;
run;

/*logistic regression*/
proc logistic data=logist1.insurance_t alpha=0.05
 plots(only)=(effect oddsratio);
 model ins(event='1')=pos / clodds=pl;
run;

/*significance*/
proc logistic data=logist1.insurance_t plots(only)=(oddsratio);
	model ins(event='1') = pos / clodds=pl clparm=pl;
run;

/*------------------exploring posamt variable----------------*/
proc freq data = logist1.insurance_t;
tables posamt;
where pos >= 1;
run;

/*Box tidwell for posamt*/ 
data logist1.insurance_t;
set logist1.insurance_t;
plogp = posamt*log(posamt);
where pos >= 1;
run;
/*logistic regression*/
proc logistic data=logist1.insurance_t plots(only)=(oddsratio);
model ins(event='1') = posamt plogp /
clodds=pl clparm=pl;
where pos >=1;
run;
quit;

/*significance*/
proc logistic data=logist1.insurance_t plots(only)=(oddsratio);
	model ins(event='1') = teller / clodds=pl clparm=pl;
	where pos >= 1;
run;

/*-------------------------exploring cd variable------------------*/
proc freq data = logist1.insurance_t;
tables cd;
run;

proc freq data=logist1.insurance_t;
 tables (cd)*ins
 / chisq expected cellchi2 nocol nopercent
 relrisk;
run;

/*logistic regression*/
proc logistic data=logist1.insurance_t alpha=0.05
 plots(only)=(effect oddsratio);
 model ins(event='1')=cd / clodds=pl;
run;

/*--------------------exploring cdbal variable------------------*/
proc freq data = logist1.insurance_t;
tables cdbal;
where cd = 1;
run;

/*Box tidwell for cdbal*/ 
data logist1.insurance_t;
set logist1.insurance_t;
clogc = cdbal*log(cdbal);
where cd = 1;
run;
proc logistic data=logist1.insurance_t plots(only)=(oddsratio);
model ins(event='1') = cdbal clogc /
clodds=pl clparm=pl;
where cd = 1;
run;
quit;

/*significance*/
proc logistic data=logist1.insurance_t plots(only)=(oddsratio);
	model ins(event='1') = cdbal / clodds=pl clparm=pl;
	where cd = 1;
run;

/****************************************************************************/

/*
IRA 
IRABAL 
LOC 
LOCBAL 
INV 
INVBAL 
ILS 
ILSBAL 
MM 
*/
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
	var IRA IRABAL LOC LOCBAL INV INVBAL ILS ILSBAL MM INS;
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


/* --------------- check Linearity Assumption --------------------*/
proc means data=hw1.insurance_t;
	var IRA IRABAL LOC LOCBAL INV INVBAL ILS ILSBAL MM;
	where IRA = 0 and IRABAL = 0;
run;

/*------------------ Check Assumptions for Continuous Vars ---------------------------------*/
/*--------------------------- IRABAL(GAM)--------------------------*/
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
DOES NOT CONVERGE, has to run Box Tidwell; 
*/

data ira;
	set hw1.insurance_t;
	iralog = IRABAL*log(IRABAL);
run;

proc logistic data=ira plots(only)=(effect oddsratio);
	model INS(event='1') = IRABAL iralog / clodds=pl clparm=pl;
	where IRA = 1;
run;
quit;
/* 
47 people have 0 balance even though they do have account
iralog p-value = 0.4556, linearity assumption met; 
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
	where LOC = 1;
run;
/* 
DOES NOT CONVERGE, has to run Box Tidwell; 
*/


data loc;
	set hw1.insurance_t;
	loclog = LOCBAL*log(LOCBAL);
run;

proc logistic data=loc plots(only)=(effect oddsratio);
	model INS(event='1') = LOCBAL loclog / clodds=pl clparm=pl;
	where LOC = 1;
run;
quit;
/* 
56 people have 0 balance even though they do have account
iralog p-value = 0.2066, linearity assumption met; 
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
	where INV = 1;
run;

/* 
DOES CONVERGE;
spline(INVBAL) p-value = 0.3289; DF=3
Linearity Assumption met 
*/


/*--------------------------- ILSBAL(GAM)--------------------------*/
proc logistic data=hw1.insurance_t alpha=0.002 plots(only)=(effect oddsratio);
	model INS(event='1') = ILSBAL / clodds=pl clparm=pl;
run;
quit;
*/ 
ILSBAL: p-value = 0.0313 (Insignificant)   Wald chi-sq = 4.6349
;

data ils;
	set hw1.insurance_t;
	ilslog = ILSBAL*log(ILSBAL);
run;

proc logistic data=ils plots(only)=(effect oddsratio);
	model INS(event='1') = ILSBAL ilslog / clodds=pl clparm=pl;
	where ILS = 1;
run;
quit;
/* 
DOES CONVERGE;
log p-value = 0.0422;
Linearity Assumption NOT met 
*/

/*--------------------- Count missing values for all---------------------------------*/
proc means data=hw1.insurance_t NMISS N; run;

/********************************************************************************************/

/*
MMBAL 
MMCRED 
MTG 
MTGBAL 
CC
CCBAL
CCPURC
SDB
INCOME
*/

*Interesting finding: 75% of data were 0. Perhaps is multicollinear with MM;

/*Check for significance Wald Chi Sq: 208.0829 P-value: <.0001. SIGNIFICANT*/
/*H0: the beta for MMBAL = 0*/
/*Ha: the beta for MMBAL is NOT 0*/

proc logistic data=ins.insurance_t alpha=0.002
              plots(only)=(effect oddsratio);
    model Ins(event='1')=MMBAL / clodds=pl clparm=pl;
run;

*Run Box-Tidwell b/c there's no obs with 0 value after the where statement;
proc univariate data=ins.insurance_t;
    var MMBAL;
    where mm=1;
    inset mean std median min max;
run;

/* Checking Assumptions - Box Tidwell MMBAL */
data insurancemmbal;
	set ins.insurance_t;
	where mm=1;
	xlogx = mmbal*log(mmbal);
run;

/*H0: the delta of MMBAL * log(MMBAL) is 0*/
/*H0: the delta of MMBAL * log(MMBAL) is NOT 0*/
/*MMBAL * log(MMBAL) p-value: 0.3489. Linearity assumption meets*/

proc logistic data=insurancemmbal alpha=0.002
              plots(only)=(oddsratio);
    model Ins(event='1')=mmbal xlogx/ clodds=pl clparm=pl;
    *title 'Modeling Buying Insurance';
run;
quit;


/*MMCRED*/
/*WARNING: 30% of the cells have expected counts less */
/*than 5. Chi-Square may not be a valid test.*/
*Fisher Test Table Probability (P) <.0001  Pr <= P <.0001;
*MMCRED is significant in predicting whether customers bought the insurance product*/

proc freq data=ins.insurance_t;
    tables mmcred * ins;
	exact fisher ;
run;

*No reference level;
proc logistic data=ins.insurance_t alpha=0.002
              plots(only)=(effect oddsratio);
	model Ins(event='1')=mmcred / clodds=pl clparm=pl;
run;


/*MTG Mantel Haenszel INSIGNIFICANT*/
/*Mantel-Haenszel Chi-Square 0.3980 p-value 0.5281 */
/*OR 1.0693 (0.8683 1.3169 )*/
proc freq data=ins.insurance_t;
    tables mtg * ins / chisq measures cl;
    format ins insfmt.;
run;

/*MTGBAL - insignificant, linearity assumption met*/
/*Interesting finding: 95% of data contains 0*/

/*Check for significance. Wald Chi Sq: 3.5516 P-value: 0.0595. INSIGNIFICANT*/
/*H0: the beta of MTGBAL is 0 (not useful)*/
/*HA: the beta of MTGBAL is NOT 0 (useful)*/
proc logistic data=ins.insurance_t alpha=0.002
              plots(only)=(effect oddsratio);
    model Ins(event='1')=MTGBAL / clodds=pl clparm=pl;
run;

*Run GAM b/c there's still 0 values after filtering out the 0s w/ the where statement;
proc univariate data=ins.insurance_t;
    var mtgbal;
    where mtg=1;
    inset mean std median min max / format=5.2 position=ne;
run;

/*H0: MTGBAL is linearly related to whether customers buy insurance products*/
/*HA: MTGBAL is NOT linearly related to whether customers buy insurance products*/

/*Model DID NOT converged*/
proc gam data=ins.insurance_t plots = components(clm commonaxes);
	where mtg=1;
	model ins(event='1') = spline(mtgbal, df=4)/ dist = binomial link = logit;
run;
quit;

/*The model did NOT converged. Ran Box Tidwell*/
/*95% of data are 0s*/
data logmtbal;
	set ins.insurance_t;
	where mtg=1;
	log = MTGBAL*log(MTGBAL);
run;

* WARNING Message: Invalid argument to function LOG(0) at line 734 column 18;

/*H0: the delta of MTGBAL * log(MTGBAL) is 0*/
/*H0: the delta of MTGBAL * log(MTGBAL) is NOT 0*/
/*MTGBAL p-value: 0.1089 MTGBAL*log(MTGBAL) p-value: 0.1309 . Linearity assumption HOLDS*/
proc logistic data=logmtbal alpha=0.002
              plots(only)=(oddsratio);
    model Ins(event='1')=MTGBAL log/ clodds=pl clparm=pl;
run;
quit;


/*CC(sample size 7420 Missing 1075)*/
/*Mantel-Haenszel's p-value: <0.001. CC is significant in predicting whether customers bought the insurance product*/
/*Spearman correlation 0.1375*/
/*Odds ratio: 1.7813*/
/*Customers who don't have credit cards have 1.78 times the odds of not purchasing insurance products. 
Focus on customers who have credit cards*/
proc freq data=ins.insurance_t;
    tables cc * ins / chisq measures cl;
    format ins insfmt.;
    *title 'Ordinal Association between having a Credit Card Account and Buying Insurance Products';
run;

/*CCBAL - insignificant, linearity assumption MET*/
/*Check for significance. Wald Chi Sq: 8.6804 P-value: 0.0032. INSIGNIFICANT*/
proc logistic data=ins.insurance_t alpha=0.002
              plots(only)=(effect oddsratio);
    model Ins(event='1')=ccbal / clodds=pl clparm=pl;
run;

/*50% of the data has 0 values. Also have negative values. Used GAM to check for linearity assumption*/
/*1075 observations were deleted due to missing values for the response or explanatory variables*/
proc univariate data=ins.insurance_t;
    var CCBAL;
	where cc=1;
    inset mean std median min max;
run;

/* Checking Assumptions - GAM - CCBAL. Model DID NOT converged */
proc gam data=ins.insurance_t plots = components(clm commonaxes);
	where cc=1;
	model ins(event='1') = spline(ccbal, df=4)/ dist = binomial link = logit;
run;
quit;

/*Used Box-Tidwell*/
data insuranceccbal;
	set ins.insurance_t;
	where cc=1;
	CCBALlog = CCBAL*log(CCBAL);
run;

/*H0: the delta of CCBAL * log(CCBAL) is 0*/
/*H0: the delta of CCBAL * log(CCBAL) is NOT 0*/
/*CCBAL p-value: 0.0888 CCBAL*log(CCBAL) p-value: 0.0790 . Linearity assumption holds*/
proc logistic data=insuranceccbal alpha=0.002
              plots(only)=(oddsratio);
    model Ins(event='1')=CCBAL CCBALlog/ clodds=pl clparm=pl;
run;
quit;

/*CCPURC*/

/*WARNING: 13% of the data are missing.*/
/*Mantel-Haenszel's p-value: <0.001. CCPURC is significant in predicting whether customers bought the insurance product*/
/*Spearman correlation 0.0859*/
/*Odds ratio: 1.402 */

proc freq data=ins.insurance_t;
    tables ccpurc * ins / chisq measures cl;
run;

*have to run proc logistic to get the confidence limits;
proc logistic data=ins.insurance_t alpha=0.002
              plots(only)=(effect oddsratio);
    model Ins(event='1')=ccpurc / clodds=pl clparm=pl;
run;

/*SDB*/
/*Mantel-Haenszel's p-value: <0.001. SDB is significant in predicting whether customers bought the insurance product*/
/*Spearman correlation 0.0677*/
/*Odds ratio: 1.5497 (1.3495, 1.7795) */
/*Customers who don't have safety deposit box have 1.55 times the odds of not purchasing insurance products. 
Focus on customers who have safety deposit box*/

proc freq data=ins.insurance_t;
    tables sdb * ins / chisq measures cl;
    *format ins insfmt.;
    *title 'Ordinal Association between having a Safety Deposit Box and Buying Insurance Products';
run;


/*INCOME*/
*1537 missing obs, contain 0 value in 0% min;
/*Check for significance. Wald Chi Sq: 1.2867 P-value: 0.2567. INSIGNIFICANT*/
proc logistic data=ins.insurance_t alpha=0.002
              plots(only)=(effect oddsratio);
    model Ins(event='1')=income / clodds=pl clparm=pl;
run;

*Run GAM bc there are 0 values;
proc univariate data=ins.insurance_t;
    var income;
    inset mean std median min max;
run;

/*p-value 0.3614 Linearity assumption met Model converged DF=3*/
proc gam data=ins.insurance_t plots = components(clm commonaxes);
	model ins(event='1') = spline(income, df=4)/ dist = binomial link = logit;
run;
quit;


/*Data Exploration*/

*Examining Missing values;
proc freq data=ins.insurance_t ;
    tables mmbal mmcred mtg mtgbal cc ccbal ccpurc sdb income;
run; 

/* Examining Distributions - Categorical Predictors */
proc freq data=ins.insurance_t ;
    tables Ins MMCRED MTG cc ccpurc sdb 
           mmcred*ins mtg*ins cc*ins ccpurc*ins sdb*ins/
           plots(only)=freqplot(scale=percent);
    format ins insfmt.;
run;

/*MMBAL*/
/*Wald Chi-Square 0.4706 MMBAL p-value is 0.4927. INSIGNIFICANT maybe bc of multi-collinearity*/

proc logistic data=insurancemmbal alpha=0.002
              plots(only)=(effect oddsratio);
    model Ins(event='1')=MMBAL / clodds=pl clparm=pl;
run;

/*Wald Chi Sq: 3.5903 	MTGBAL p-value is 0.0581 INSIGNIFICANT*/
proc logistic data=logmtbal alpha=0.002
              plots(only)=(effect oddsratio);
    model Ins(event='1')=MTGBAL / clodds=pl clparm=pl;
run;

/*CCBAL p-value is 0.0032 . Insignificant*/
proc logistic data=insuranceccbal alpha=0.002
              plots(only)=(effect oddsratio);
    model Ins(event='1')=ccbal / clodds=pl clparm=pl;
run;
proc freq data=ins.insurance_t;
	tables ins*ccbal;
	format Ins insfmt. ccbal ccbalfmt.;
run;

proc freq data=ins.insurance_t;
	tables ins*mtgbal;
	format Ins insfmt. mtgbal mtgbalfmt.;
run;

/************************/
/* Tests of Association */
/************************/

/* Chi-Square Test */
proc freq data=ins.insurance_t;
    tables (MMBAL MTGBAL CCBAL INCOME)*Ins
          / chisq expected cellchi2 nocol nopercent 
            relrisk;
    format ins insfmt.;
/*    title 'Associations with Buying Insurance';*/
run;

/* Detecting Ordinal Associations (MTG CC SDB MMCRED CCPURC)*/

/*Mantel-Haenszel's p-value: 0.5281. MTG is insignificant in predicting whether customers bought the insurance product*/
proc freq data=ins.insurance_t;
    tables mtg * ins / chisq measures cl;
    format ins insfmt.;
    *title 'Ordinal Association between having a Mortgage Account and Buying Insurance Products';
run;

/************************************************************************************************/


/* Looking at missing variables */
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

/* Get wald chisq and p value */
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

/* Get wald chisq and p value */
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

/* Get wald chisq and p value */
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

/* Get wald chisq and p value */
proc logistic data=hw.insurance_t plots(only) = (effect oddsratio);
	model INS(event='1') = CRSCORE / clodds=pl clparm=pl;
run; quit;

/*********************************************************************************/

/* Pearsons on BRANCH and RES (nominal variables) */
proc freq data=hw.insurance_t;
	tables (BRANCH RES)*INS / chisq;
run;
