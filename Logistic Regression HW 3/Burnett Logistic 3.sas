/*Library creation*/
libname logist1 "C:\Users\Price Burnett\Downloads\Homework2_LR";run;

/*Data Transformation*/
data temp_t;
	set logist1.insurance_t_bin;
	if hmown = . then hmown = -1;
	if inv = . then inv = -1;
	if ccpurc = . then ccpurc = -1;
	if cc = . then cc = -1;
	if mmcred = 5 then mmcred = 3;
	if cashbk = 2 then cashbk = 1;
run;

data temp_v;
	set logist1.insurance_v_bin;
	if hmown = . then hmown = -1;
	if inv = . then inv = -1;
	if ccpurc = . then ccpurc = -1;
	if cc = . then cc = -1;
	if mmcred = 5 then mmcred = 3;
	if cashbk = 2 then cashbk = 1;
run;

/*concordance area under the ROC curve*/
proc logistic data=temp_t plots(only) = ROC;
	class nsf dda ddabal_bin(ref='1') checks_bin(ref='1') teller_bin(ref='1') savbal_bin(ref='1') 
	cdbal_bin(ref='1') atmamt_bin(ref='1')	branch(ref='B1') mm ira	inv(ref='-1') ils cc(ref='-1') 
	/ param=ref; 
	model ins(event='1') = nsf dda ddabal_bin checks_bin teller_bin savbal_bin cdbal_bin
		atmamt_bin branch mm ira inv ils cc ddabal_bin*savbal_bin mm*ddabal_bin dda*ira / clodds=pl clparm=pl;
run;
quit;

/*discrimination slope*/
proc logistic data=temp_t;
	class nsf dda ddabal_bin(ref='1') checks_bin(ref='1') teller_bin(ref='1') savbal_bin(ref='1') 
	cdbal_bin(ref='1') atmamt_bin(ref='1')	branch(ref='B1') mm ira	inv(ref='-1') ils cc(ref='-1') 
	/ param=ref;
	model ins(event='1') = nsf dda ddabal_bin checks_bin teller_bin savbal_bin cdbal_bin
		atmamt_bin branch mm ira inv ils cc ddabal_bin*savbal_bin mm*ddabal_bin dda*ira / clodds=pl clparm=pl;
	output out=model1 p=phat predprobs=individual;
run;

/*sort the data*/
proc sort data=model1;
	by descending ins;
run;

/*T-test procedure*/
proc ttest data=model1 order=data;
	ods select statistics summarypanel;
	class ins;
	var phat;
	title 'Discrimination Slope';
run;


/*KS statistic*/
proc npar1way data=model1 d plot=edfplot;
	class ins;
	var phat;
run;

/*confusion matrix info*/
proc logistic data=temp_v plots(only)=(oddsratio);
	class nsf dda ddabal_bin(ref='1') checks_bin(ref='1') teller_bin(ref='1') savbal_bin(ref='1') 
	cdbal_bin(ref='1') atmamt_bin(ref='1')	branch(ref='B1') mm ira	inv(ref='-1') ils cc(ref='-1') 
	/ param=ref;
	model ins(event='1') = nsf dda ddabal_bin checks_bin teller_bin savbal_bin cdbal_bin
		atmamt_bin branch mm ira inv ils cc ddabal_bin*savbal_bin mm*ddabal_bin dda*ira / ctable pprob =0.299877;
	ods output classification=model2;
run;

/*Confusion matrix*/
proc print data=model2;
run;

/*lift*/
proc logistic data=temp_t plots(only)=(oddsratio) PLOTS(MAXPOINTS=NONE);
	class nsf dda ddabal_bin(ref='1') checks_bin(ref='1') teller_bin(ref='1') savbal_bin(ref='1') 
	cdbal_bin(ref='1') atmamt_bin(ref='1')	branch(ref='B1') mm ira	inv(ref='-1') ils cc(ref='-1') / param=ref; 
	model ins(event='1') = nsf dda ddabal_bin checks_bin teller_bin savbal_bin cdbal_bin
	atmamt_bin branch mm ira inv ils cc ddabal_bin*savbal_bin mm*ddabal_bin dda*ira / clodds=pl clparm=pl;
	score data=temp_v fitstat outroc=temp_roc;
run;
quit;

data temp_roc; 
	set temp_roc; 
	cutoff = _PROB_; 
	specif = 1-_1MSPEC_; 
	depth=(_POS_+_FALPOS_)/2124*100; 
	precision=_POS_/(_POS_+_FALPOS_); 
	acc=_POS_+_NEG_;
	lift=precision/0.3435; 
run;

