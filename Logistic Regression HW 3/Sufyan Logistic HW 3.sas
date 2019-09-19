/* Initialize Library */
libname lrhw3 "C:\Users\Asus\Documents\Learning\Advanced Analytics\Logistic Regression\Homework\Homework 2";

/* Data Cleaning */
data temp;
	set lrhw3.insurance_t_bin;
	if hmown = . then hmown = -1;
	if inv = . then inv = -1;
	if ccpurc = . then ccpurc = -1;
	if cc = . then cc = -1;
	if mmcred = 5 then mmcred = 3;
	if cashbk = 2 then cashbk = 1;
run;

data temp2;
	set lrhw3.insurance_v_bin;
	if hmown = . then hmown = -1;
	if inv = . then inv = -1;
	if ccpurc = . then ccpurc = -1;
	if cc = . then cc = -1;
	if mmcred = 5 then mmcred = 3;
	if cashbk = 2 then cashbk = 1;
run;

/* Concordance = 80.8 and AUC = 80.8, ROC curve at bottom of output */
proc logistic data=temp plots(only) = ROC;
	class nsf dda ddabal_bin(ref='1') checks_bin(ref='1') teller_bin(ref='1') savbal_bin(ref='1') 
	cdbal_bin(ref='1') atmamt_bin(ref='1')	branch(ref='B1') mm ira	inv(ref='-1') ils cc(ref='-1') 
	/ param=ref; 
	model ins(event='1') = nsf dda ddabal_bin checks_bin teller_bin savbal_bin cdbal_bin
		atmamt_bin branch mm ira inv ils cc ddabal_bin*savbal_bin mm*ddabal_bin dda*ira / clodds=pl clparm=pl;
run;
quit;


/* Discrimination Slope = 0.2648 */
proc logistic data=temp noprint;
	class nsf dda ddabal_bin(ref='1') checks_bin(ref='1') teller_bin(ref='1') savbal_bin(ref='1') 
	cdbal_bin(ref='1') atmamt_bin(ref='1')	branch(ref='B1') mm ira	inv(ref='-1') ils cc(ref='-1') 
	/ param=ref;
	model ins(event='1') = nsf dda ddabal_bin checks_bin teller_bin savbal_bin cdbal_bin
		atmamt_bin branch mm ira inv ils cc ddabal_bin*savbal_bin mm*ddabal_bin dda*ira / clodds=pl clparm=pl;
	output out=predprobs p=phat predprobs=individual;
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

/*K-S Statistic = 0.299877 */
proc npar1way data=predprobs d plot=edfplot;
	class ins;
	var phat;
run;

/* Getting information for Confusion Matrix based on K-S Statistic */
proc logistic data=temp2 plots(only)=(oddsratio);
	class nsf dda ddabal_bin(ref='1') checks_bin(ref='1') teller_bin(ref='1') savbal_bin(ref='1') 
	cdbal_bin(ref='1') atmamt_bin(ref='1')	branch(ref='B1') mm ira	inv(ref='-1') ils cc(ref='-1') 
	/ param=ref; 
	model ins(event='1') = nsf dda ddabal_bin checks_bin teller_bin savbal_bin cdbal_bin
		atmamt_bin branch mm ira inv ils cc ddabal_bin*savbal_bin mm*ddabal_bin dda*ira / ctable pprob =0.299877;
	ods output classification=classtable;
run;
quit;

proc print data=classtable;
run;

/* Confusion Matrix - Accuracy = 0.6992 */
/*********************/
/*      Predicted    */
/*       0       1
A
c   0   909     473
t
u   1   166     576
a
l
*/

/* Lift */
proc logistic data=temp plots(only)=(oddsratio);
	class nsf dda ddabal_bin(ref='1') checks_bin(ref='1') teller_bin(ref='1') savbal_bin(ref='1') 
	cdbal_bin(ref='1') atmamt_bin(ref='1')	branch(ref='B1') mm ira	inv(ref='-1') ils cc(ref='-1') 
	/ param=ref; 
		model ins(event='1') = nsf dda ddabal_bin checks_bin teller_bin savbal_bin cdbal_bin
		atmamt_bin branch mm ira inv ils cc ddabal_bin*savbal_bin mm*ddabal_bin dda*ira / clodds=pl clparm=pl;
	score data=temp2 fitstat outroc=roc;
run;
quit;

data work.roc; 
	set work.roc; 
	cutoff = _PROB_; 
	specif = 1-_1MSPEC_; 
	depth=(_POS_+_FALPOS_)/1049*100; 
	precision=_POS_/(_POS_+_FALPOS_); 
	acc=_POS_+_NEG_;
	lift=precision/0.3435; 
run;

/* Lift plot
Interpretation? Targeting the top 20% of customers, based on predictive probability,
 gets over 2 times as many responses (insurance purchases?) compared to targeting a random sample 
 of 20% of customers. */

proc sgplot data=work.roc;
	series y=lift x=depth; 
	refline 1.0 / axis=y; 
	title1 "Lift Chart for Validation Data"; 
	xaxis label="Depth (%)";
	yaxis label="Lift";
run; 
quit;

