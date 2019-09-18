/* Initialize the libref */
libname hw3 "Z:\Documents\Fall I 2019\Logistic Regression\Orange2-HW\Logistic Regression HW 2";


/****** Backward selection on all variables ******/

/* Macro for all variables */
%let all = nsfamt_bin nsf dirdep depamt_bin ddabal_bin dda checks_bin cashbk acctage_bin teller_bin 
	savbal_bin sav posamt_bin pos_bin phone_bin cdbal_bin cd atmamt_bin atm res moved lores_bin 
	inarea hmval_bin hmown crscore_bin branch age_bin mm locbal_bin loc irabal_bin ira invbal_bin inv 
	ilsbal_bin ils sdb mtgbal_bin mtg mmcred mmbal_bin income_bin ccpurc ccbal_bin cc;

/* Backward selection on all variables
Remaining Variables:
NSF DDABAL_Bin DDA CHECKS_Bin TELLER_Bin SAVBAL_Bin CDBAL_Bin ATMAMT_Bin BRANCH MM IRA INV ILS CC*/
proc logistic data=hw3.insurance_t_imputed plots(only)=(oddsratio);
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
proc logistic data=hw3.insurance_t_imputed plots(only)=(oddsratio);
	class nsf dda ddabal_bin(ref='1') checks_bin(ref='1') teller_bin(ref='1') savbal_bin(ref='1') cdbal_bin(ref='1') 
		atmamt_bin(ref='1')	branch(ref='B1') mm ira	inv(ref='-1') ils cc(ref='-1') 
		/ param=ref; 
	model ins(event='1') = nsf|dda|ddabal_bin|checks_bin|teller_bin|savbal_bin|cdbal_bin|atmamt_bin|branch|mm|ira|inv|ils|cc@2 / selection=forward slentry=0.002 clodds=pl clparm=pl;
	title 'Modeling Purchase of Insurance Products';
run;
quit;
