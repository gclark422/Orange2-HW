/*Library creation*/
libname logist1 "C:\Users\Price Burnett\Downloads\Homework2_LR";run;

/****** Transforming the data ******/

data temp;
	set logist1.insurance_t_bin;
	if hmown = . then hmown = -1;
	if inv = . then inv = -1;
	if ccpurc = . then ccpurc = -1;
	if cc = . then cc = -1;
	if mmcred = 5 then mmcred = 3;
	if cashbk = 2 then cashbk = 1;
run;

/* Create Macro */
%let all = nsfamt_bin nsf dirdep depamt_bin ddabal_bin dda checks_bin cashbk acctage_bin teller_bin 
	savbal_bin sav posamt_bin pos_bin phone_bin cdbal_bin cd atmamt_bin atm res moved lores_bin 
	inarea hmval_bin hmown crscore_bin branch age_bin mm locbal_bin loc irabal_bin ira invbal_bin inv 
	ilsbal_bin ils sdb mtgbal_bin mtg mmcred mmbal_bin income_bin ccpurc ccbal_bin cc;
	
/*Backwards Selection with all variables */
proc logistic data=temp plots(only)=(oddsratio);
class nsfamt_bin nsf dirdep depamt_bin(ref = '1') ddabal_bin(ref='1') dda checks_bin(ref='1') 
		cashbk acctage_bin(ref='1') teller_bin(ref='1') savbal_bin(ref='1') sav posamt_bin(ref='1') 
		pos_bin(ref='1') phone_bin(ref='1') cdbal_bin(ref='1') cd atmamt_bin(ref='1') atm res(ref='R')
		moved lores_bin(ref='1') inarea hmval_bin(ref='1') hmown(ref='-1') crscore_bin(ref='1') 
		branch(ref='B1') age_bin(ref='1') mm locbal_bin(ref='1') loc irabal_bin ira invbal_bin(ref='1') 
		inv(ref='-1') ilsbal_bin ils sdb mtgbal_bin(ref='1') mtg mmcred(ref='1') mmbal_bin income_bin(ref='1')
		ccpurc(ref='-1') ccbal_bin(ref='1') cc(ref='-1')/ param=ref; 
model ins(event='1') = &all /
selection=backward slstay=0.002
clodds=pl clparm=pl;
title 'Backward Selection';
run;
quit;

/*Forward Selection including significant variables*/
proc logistic data=temp plots(only)=(oddsratio);
class   ddabal_bin(ref='1') dda checks_bin(ref='1') 
		teller_bin(ref='1') savbal_bin(ref='1') 
		 cdbal_bin(ref='1') atmamt_bin(ref='1')  
		branch(ref='B1')  mm locbal_bin(ref='1')  ira 
		inv(ref='-1')  ils cc(ref='-1')/ param=ref; 
model ins(event='1') = ddabal_bin | dda | checks_bin | teller_bin | savbal_bin | cdbal_bin | atmamt_bin | branch | mm | ira | inv | ils | cc @2
selection=forward slentry=.002
clodds=pl clparm =pl;
title 'Forward Selection';
run;
quit;