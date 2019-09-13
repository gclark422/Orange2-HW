/*Logistic Homework Phase II Variable Selection and Modeling Building*/


/*Using imputed data set*/
libname hw2 'Z:\Documents\Fall I 2019\Logistic Regression\Orange2-HW\Logistic Regression HW 2';

/* Macro for all significant variables */

/*A Total of 30 vars*/
%let sig = NSFAMT_Bin NSF DIRDEP DEPAMT_Bin DDABAL_Bin DDA CHECKS_Bin CASHBK SAVBAL_bin SAV POSAMT_bin POS_bin
PHONE_bin CDBAL_bin CD ATMAMT_bin ATM INAREA HMVAL_bin BRANCH  MM IRABAL_Bin IRA INVBAL_Bin INV SDB MMCRED 
MMBAL_Bin CCPURC CC;

/* Backward selection on all significant variables
		Type 3 Analysis of Effects 
Effect		DF	Wald Chi-Square 	Pr > ChiSq 
NSF 		1	13.2164 			0.0003 
DIRDEP 		1 	9.8434 				0.0017 
DDABAL_Bin	7 	305.1093 			<.0001 
DDA 		1 	12.9226 			0.0003 
CHECKS_Bin 	3 	57.4732 			<.0001 
SAVBAL_Bin 	6 	553.8556 			<.0001 
CDBAL_Bin 	2 	170.8756 			<.0001 
ATMAMT_Bin	2 	29.7816 			<.0001 
BRANCH 		18 	118.1193 			<.0001 
MM 			1 	100.0636 			<.0001 
IRA 		1 	15.7044 			<.0001 
INV 		1 	15.5038 			<.0001 
CC 			1 	16.4197 			<.0001 
*/


proc logistic data=hw2.insurance_t_imputed plots(only)=(oddsratio);
	class nsfamt_bin nsf dirdep depamt_bin(ref = '1') ddabal_bin(ref='1') dda checks_bin(ref='1') 
		cashbk savbal_bin(ref='1') sav posamt_bin(ref='1') pos_bin(ref='1') phone_bin(ref='1')
		cdbal_bin(ref='1') cd atmamt_bin(ref='1') atm inarea hmval_bin(ref='1')  
		branch(ref='B1') mm irabal_bin ira invbal_bin(ref='1') 
		inv(ref='-1') sdb mmcred(ref='1') mmbal_bin 
		ccpurc(ref='-1') cc(ref='-1')/ param=ref; 
	model ins(event='1') = &sig / selection=backward slstay=0.002 clodds=pl clparm=pl;
	title 'Modeling Purchase of Insurance Products';
run;
quit;


/* Forward selection on significant variables and interactions 
New Interactions: DDA*SAVBAL_Bin CHECKS_Bin*SAVBAL_Bin SAVBAL_Bin*MM DDA*IRA */
proc logistic data=hw2.insurance_t_imputed plots(only)=(oddsratio);
	class NSF DIRDEP DDABAL_Bin(ref='1') DDA CHECKS_Bin(ref='1') SAVBAL_Bin(ref='1') CDBAL_Bin(ref='1')
			ATMAMT_Bin(ref='1') BRANCH(ref='B1') MM IRA  INV(ref='-1') CC(ref='-1')/ param=ref; 
	model ins(event='1') = NSF|DIRDEP|DDABAL_Bin|DDA|CHECKS_Bin|SAVBAL_Bin|CDBAL_Bin|ATMAMT_Bin|BRANCH|MM|IRA|INV|CC@2 / selection=forward slentry=0.002 clodds=pl clparm=pl;
	title 'Modeling Purchase of Insurance Products';
run;
quit;








libname hw2 'Z:\Documents\Fall I 2019\Logistic Regression\Homework2_LR';

*Examining Missing values from original data set;
proc freq data=hw2.insurance_t_bin;
    tables SDB MTGBAL_Bin MTG MMCRED MMBAL_Bin INCOME_Bin CCPURC CCBAL_Bin CC;
run; 

/*CC and CCPURC has 1075 missing values*/

/*Variable Type and Level
Test: Mantel-Haenszel for Ordinal and Binary Variable

Variable Name		Type(Original --> New)	Level
SDB:  	     		Binary					(0,1
MTGBAL_Bin:  		Ordinal					(1, 2, 3)
MTG:  	     		Binary					(0,1)
MMCRED: 	 		Ordinal					(0,1,2,3,4,5)
MMBAL_Bin:	 		Binary					(1, 2)
INCOME_Bin:  		Ordinal					(1, 2, 3) 
CCPURC:	     		Ordinal	--> Ordinal		(-1,0,1,2,3,4)
CCBAL_Bin:  		Ordinal					(1, 2, 3)
CC: 	    		Binary	--> Nominal		(-1,0,1)
*/

/*Newest data set is hw2.insurance_t_new1*/

/*MMCRED quasi-separation */
proc freq data=hw2.insurance_t_bin;
	tables mmcred*ins;
run;

data hw2.insurance_t_new1;
	set hw2.insurance_t_new;
	IF mmcred = 5 then mmcred = 3;
run;

proc freq data=hw2.insurance_t_new1;
	tables mmcred mmcred*INS/cl chisq measures;
RUN;

/*CCPURC */
/*--------------------Create an extra category -1 for CCPURC-----------------*/
data hw2.insurance_t_ccpurc;
	set hw2.insurance_t_bin;
	IF ccpurc = . then ccpurc = -1;
run;

proc freq data=hw2.insurance_t_ccpurc;
	tables ccpurc ccpurc*INS;
RUN;
/* No seperation problem */


/*CC Mantel-Haenszel  missing value 1075*/

data hw2.insurance_t_new;
	set hw2.insurance_t_ccpurc;
	IF cc = . then cc = -1;
run;

proc freq data=hw2.insurance_t_new;
	tables cc cc*INS/cl chisq measures;
RUN;

/*-------------------Seperation Concern Check-----------------*/
proc freq data=hw2.insurance_t_new1;
	tables (SDB MTGBAL_Bin MTG MMCRED MMBAL_Bin INCOME_Bin CCPURC CCBAL_Bin CC)*INS
	/nopercent nocol nocum norow;
run;

/* No Seperation Concerns after imputing MMCRED, CC, and CCPURC */

/*---------------------Check Significance, stats, and OR----------------*/

proc freq data=hw2.insurance_t_new1;
	tables (SDB MTGBAL_Bin MTG MMCRED MMBAL_Bin INCOME_Bin CCPURC CCBAL_Bin CC)*INS/	cl chisq measures;
	title "association with INS";
run;

*/ 
SDB:  	     p-value < 0.0001 (significant)     MH = 38.9693 	odds ratio 1.5497 (1.3495, 1.77952.8503)
MTGBAL_Bin:  p-value = 0.3855 (insignificant)   MH = 0.7531  	Spearman = 0.0060
MTG:  	     p-value = 0.5281 (insignificant)   MH = 0.3980   	odds ratio 1.3169 (0.8873, 1.2779)
MMCRED: 	 p-value < 0.0001 (significant)     MH = 65.8808 
MMBAL_Bin:	 p-value < 0.0001 (significant)     MH = 252.4873 	odds ratio 2.8503 (2.4949, 3.2562) 
INCOME_Bin:  p-value = 0.2932 (insignificant)   MH = 1.1046   
CCPURC:	     p-value < 0.0001 (significant)     MH = 87.7339  
CCBAL_Bin:   p-value = 0.7606 (insignificant)   MH = 0.0928   
CC: 	     p-value < 0.0001 (significant)   	MH = 87.7339    Pearson
*/
 
  
