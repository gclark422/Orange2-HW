/* Initialize the libref */
libname hw2 "C:\Users\Asus\Documents\Learning\Advanced Analytics\Logistic Regression\Homework\Homework 2";


/****** Checking all of the individual variables ******/
/* NSFAMT_Bin, None missing, No Separation Concerns, MH = 42.4893, OR = 0.4892 */
proc freq data=hw2.insurance_t_bin;
	table ins*nsfamt_bin / chisq expected cellchi2 nocol nopercent oddsratio;
run;

proc freq data=hw2.insurance_t_bin;
	table branch;
run;

/* NSF, None missing, No Separation Concerns, MH = 43.2028, OR = 0.5555 */
proc freq data=hw2.insurance_t_bin;
	table ins*nsf / chisq expected cellchi2 nocol nopercent oddsratio;
run;

/* DIRDEP, None missing, No Separation Concerns, MH = 43.7840, OR = 0.7119 */
proc freq data=hw2.insurance_t_bin;
	table ins*dirdep / chisq expected cellchi2 nocol nopercent oddsratio;
run;

/* DEPAMT_Bin, None missing, No Separation Concerns, MH = 24.7370 */
proc freq data=hw2.insurance_t_bin;
	table ins*depamt_bin / chisq expected cellchi2 nocol nopercent oddsratio;
run;

/* DDABAL_Bin, None missing, No Separation Concerns, MH = 24.3957 */
proc freq data=hw2.insurance_t_bin;
	table ins*ddabal_bin / chisq expected cellchi2 nocol nopercent oddsratio;
run;

/* DDA, None missing, No Separation Concerns, MH = 312.7474, OR = 0.3751 */
proc freq data=hw2.insurance_t_bin;
	table ins*dda / chisq expected cellchi2 nocol nopercent oddsratio;
run;

/* CHECKS_Bin, None missing, No Separation Concerns, MH = 72.7037 */
proc freq data=hw2.insurance_t_bin;
	table ins*checks_bin / chisq expected cellchi2 nocol nopercent oddsratio;
run;

/* CASHBK, None missing, Quasi-separation Concerns - Absorb "2" into "1" */
proc freq data=hw2.insurance_t_bin;
	table ins*cashbk / chisq expected cellchi2 nocol nopercent oddsratio;
run;

/* CASHBK reduced, None Missing, No Separation, MH = 11.1357, OR = 0.4915 */
data temp;
	set hw2.insurance_t_bin;
	if cashbk = 2 then cashbk = 1;
run;

proc freq data=temp;
	table ins*cashbk / chisq expected cellchi2 nocol nopercent oddsratio;
run;

/* ACCTAGE_Bin, None missing, No Separation Concerns, MH = 5.4306 */
proc freq data=hw2.insurance_t_bin;
	table ins*acctage_bin / chisq expected cellchi2 nocol nopercent oddsratio;
run;


/****** Transforming the data ******/

data temp;
	set hw2.insurance_t_bin;
	if hmown = . then hmown = -1;
	if inv = . then inv = -1;
	if ccpurc = . then ccpurc = -1;
	if cc = . then cc = -1;
	if mmcred = 5 then mmcred = 3;
	if cashbk = 2 then cashbk = 1;
run;


/****** Backward selection on all variables ******/

/* Macro for all variables */
%let all = nsfamt_bin nsf dirdep depamt_bin ddabal_bin dda checks_bin cashbk acctage_bin teller_bin 
	savbal_bin sav posamt_bin pos_bin phone_bin cdbal_bin cd atmamt_bin atm res moved lores_bin 
	inarea hmval_bin hmown crscore_bin branch age_bin mm locbal_bin loc irabal_bin ira invbal_bin inv 
	ilsbal_bin ils sdb mtgbal_bin mtg mmcred mmbal_bin income_bin ccpurc ccbal_bin cc;

/* Backward selection on all variables
Remaining Variables:
NSF DDABAL_Bin DDA CHECKS_Bin TELLER_Bin SAVBAL_Bin CDBAL_Bin ATMAMT_Bin BRANCH MM IRA INV ILS CC*/
proc logistic data=temp plots(only)=(oddsratio);
	class nsfamt_bin nsf dirdep depamt_bin(ref = '1') ddabal_bin(ref='1') dda checks_bin(ref='1') 
		cashbk acctage_bin(ref='1') teller_bin(ref='1') savbal_bin(ref='1') sav posamt_bin(ref='1') 
		pos_bin(ref='1') phone_bin(ref='1') cdbal_bin(ref='1') cd atmamt_bin(ref='1') atm res(ref='R')
		moved lores_bin(ref='1') inarea hmval_bin(ref='1') hmown(ref='-1') crscore_bin(ref='1') 
		branch(ref='B1') age_bin(ref='1') mm locbal_bin(ref='1') loc irabal_bin ira invbal_bin(ref='1') 
		inv(ref='-1') ilsbal_bin ils sdb mtgbal_bin(ref='1') mtg mmcred(ref='1') mmbal_bin income_bin(ref='1')
		ccpurc(ref='-1') ccbal_bin(ref='1') cc(ref='-1')/ param=ref; 
	model ins(event='1') = &all / selection=backward slstay=0.002 clodds=pl clparm=pl;
	title 'Modeling Purchase of Insurance Products';
run;
quit;


/* Forward selection on significant variables and interactions 
Significant Variables: NSF DDA DDABAL_Bin CHECKS_Bin TELLER_Bin SAVBAL_Bin CDBAL_Bin ATMAMT_Bin BRANCH MM
IRA INV ILS CC
New Interactions: DDABAL_Bin*SAVBAL_Bin MM*DDABAL_Bin DDA*IRA */
proc logistic data=temp plots(only)=(oddsratio);
	class nsf dda ddabal_bin(ref='1') checks_bin(ref='1') teller_bin(ref='1') savbal_bin(ref='1') cdbal_bin(ref='1') 
		atmamt_bin(ref='1')	branch(ref='B1') mm ira	inv(ref='-1') ils cc(ref='-1') 
		/ param=ref; 
	model ins(event='1') = nsf|dda|ddabal_bin|checks_bin|teller_bin|savbal_bin|cdbal_bin|atmamt_bin|branch|mm|ira|inv|ils|cc@2 / selection=forward slentry=0.002 clodds=pl clparm=pl;
	title 'Modeling Purchase of Insurance Products';
run;
quit;
