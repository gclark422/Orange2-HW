libname ins "\\vmware-host\Shared Folders\Documents\Logistic Regression\Homework1_LR";

/*MMBAL - continuous var, SIGNIFICANT, linearity assumption met*/
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
