libname hw3 "C:\Users\97420\OneDrive\Documents\MSA Fall\Logistic Regression\Homework\Homework2_LR";
run;

/* Imputing validation dataset */

data hw3.insurance_v_imputed;
	set hw3.insurance_v_bin;
	if hmown = . then hmown = -1;
	if inv = . then inv = -1;
	if ccpurc = . then ccpurc = -1;
	if cc = . then cc = -1;
	if mmcred = 5 then mmcred = 3;
	if cashbk = 2 then cashbk = 1;
run;


/****************************** Concordance, AUC, ROC curve *********************************/
proc logistic data=hw3.insurance_t_imputed plots(only) = ROC;
	class nsf(ref='0') dda(ref='0') ddabal_bin(ref='1') checks_bin(ref='1') teller_bin(ref='1') savbal_bin(ref='1') 
	cdbal_bin(ref='1') atmamt_bin(ref='1')	branch(ref='B1') mm(ref='0') ira(ref='0') inv(ref='-1') ils cc(ref='0') 
	/ param=ref; 
	model ins(event='1') = nsf dda ddabal_bin checks_bin teller_bin savbal_bin cdbal_bin
		atmamt_bin branch mm ira inv ils cc ddabal_bin*savbal_bin mm*ddabal_bin dda*ira / clodds=pl clparm=pl;
run;
quit;
/* Concordance = 80.8 and AUC = 80.85, ROC curve at bottom of output 
17 vars significant from the forward selection w/ interactions from Phase II*/



/********************************* Discrimination Slope ****************************************/
ods trace on;
ods graphics on;
proc logistic data=HW3.INSURANCE_T_IMPUTED noprint;
	class nsf(ref='0') dda(ref='0') ddabal_bin(ref='1') checks_bin(ref='1') teller_bin(ref='1') savbal_bin(ref='1') 
	cdbal_bin(ref='1') atmamt_bin(ref='1')	branch(ref='B1') mm(ref='0') ira(ref='0') inv(ref='-1') ils cc(ref='0') 
	/ param=ref; 
	model ins(event='1') = nsf dda ddabal_bin checks_bin teller_bin savbal_bin cdbal_bin
		atmamt_bin branch mm ira inv ils cc ddabal_bin*savbal_bin mm*ddabal_bin dda*ira / clodds=pl clparm=pl;
	output out=predprobs p=phat;
run;
 
proc sort data=predprobs;
	by descending ins;
run;

proc ttest data=predprobs order=data;
	ods select statistics summarypanel;
	class ins;
	var phat;
	title 'Coefficient of Discrimination and Plots';
run;
ods trace off;
/* coefficient of discrimination is 0.2648*/


/************************************* K-S Statistic ******************************************/
proc npar1way data=predprobs d plot=edfplot;
	class ins;
	var phat;
run;
/* Cut-off is at 0.299877; K-S stats D: 0.4822 */


/* Confusion Matrix Table on Validation Data*/
/*Percent Concordant 82.2 */
proc logistic data=hw3.insurance_v_imputed plots(only)=(oddsratio);
	class nsf dda ddabal_bin(ref='1') checks_bin(ref='1') teller_bin(ref='1') savbal_bin(ref='1') 
	cdbal_bin(ref='1') atmamt_bin(ref='1')	branch(ref='B1') mm ira	inv(ref='-1') ils cc(ref='0') 
	/ param=ref; 
	model ins(event='1') = nsf dda ddabal_bin checks_bin teller_bin savbal_bin cdbal_bin
		atmamt_bin branch mm ira inv ils cc ddabal_bin*savbal_bin mm*ddabal_bin dda*ira / ctable pprob =0.299877;
	ods output classification=classtable;
	title 'Modeling Insurance Products';
run;
quit;

proc print data=classtable;
run;


/********************************** Lift Chart Validation Data *******************************/
proc freq data=hw3.insurance_v_imputed;
	tables ins;
run;
/*Total 0's & 1's in the validation data 2124*/
/*Population proportion of 1 in training data set .3493*/



/* should we use validation dataset or training here? */
proc logistic data=hw3.insurance_v_imputed plots(only)=(oddsratio);
	class nsf dda ddabal_bin(ref='1') checks_bin(ref='1') teller_bin(ref='1') savbal_bin(ref='1') 
	cdbal_bin(ref='1') atmamt_bin(ref='1')	branch(ref='B1') mm ira	inv(ref='-1') ils cc(ref='-1') 
	/ param=ref; 
	model ins(event='1') = nsf dda ddabal_bin checks_bin teller_bin savbal_bin cdbal_bin
		atmamt_bin branch mm ira inv ils cc ddabal_bin*savbal_bin mm*ddabal_bin dda*ira / clodds=pl clparm=pl;
	score data=hw3.insurance_v_imputed fitstat outroc=roc;
run;
quit;

data work.roc; 
	set work.roc; 
	cutoff = _PROB_; 
	specif = 1-_1MSPEC_; 
	depth=(_POS_+_FALPOS_)/2124*100; 
	precision=_POS_/(_POS_+_FALPOS_); 
	acc=_POS_+_NEG_;
	lift=precision/0.3493; 
run;



/*Lift Plot*/
proc sgplot data=work.roc; 
	series y=lift x=depth; 
	refline 1.0 / axis=y; 
	title1 "Lift Chart for validation Data"; 
	xaxis label="Depth (%)";
	yaxis label="Lift";
run; 
quit;