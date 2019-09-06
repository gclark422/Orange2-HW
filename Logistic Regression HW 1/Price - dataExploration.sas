/*Library creation*/
libname logist1 "C:\Users\Price Burnett\Downloads\Homework1_LR (1)";run;

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
