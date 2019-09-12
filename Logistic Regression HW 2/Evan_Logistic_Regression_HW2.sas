libname hw2 "C:\Users\97420\OneDrive\Documents\MSA Fall\Logistic Regression\Homework\Homework2_LR";
run;

/*--------------------Check Missing Values ----------------------*/
proc means data=hw2.insurance_t_bin nmiss;
	var IRA IRABAL_Bin LOC LOCBAL_Bin INV INVBAL_Bin ILS ILSBAL_Bin MM INS;
run;
/* INV still has 1075 rows of missing values*/


/*--------------------Create an extra category -1 for INV-----------------*/
data hw2.insurance_t_inv;
	set hw2.insurance_t_bin;
	IF INV = . then INV = -1;
RUN;

proc freq data=hw2.insurance_t_inv;
	tables INV INV*INS;
RUN;
/* No seperation problem */


/*--------------------Check Variable Types----------------------*/
proc freq data=hw2.insurance_t_bin;
	tables IRA IRABAL_Bin LOC LOCBAL_Bin INV INVBAL_Bin ILS ILSBAL_Bin MM;
run;
/*
MM:		Binary(0,1)		
LOCBAL_Bin:	Ordinal(1,2,3)
LOC:		Binary(0,1)
IRABAL_Bin:	Binary(1,2)
IRA:		Binary(0,1)
INVBAL_Bin:	Ordinal(1,2,3)
INV:		Binary(0,1)
ILSBAL_Bin:	Binary(1,2)
ILS:		Binary(0,1)
*/

/*-------------------Seperation Concern Check-----------------*/
proc freq data=hw2.insurance_t_inv;
	tables (MM LOCBAL_Bin LOC IRABAL_Bin IRA INVBAL_Bin INV ILSBAL_Bin ILS)*INS
	/nopercent nocol nocum norow;
run;
/* No seperation concern (individually) for all 9 variables */


/*---------------------Check Significance, stats, and OR----------------*/
proc freq data=hw2.insurance_t_inv;
	tables (MM LOCBAL_Bin LOC IRABAL_Bin IRA INVBAL_Bin INV ILSBAL_Bin ILS)*INS/
	chisq measures cl relrisk;
	title "association with INS";
run;
	
*/ 
MM:  	     p-value < 0.0001 (significant)     MH = 252.4873 odds ratio 2.8503 (2.4949, 3.2562)
LOCBAL_Bin:  p-value = 0.8989 (insignificant)   MH = 0.0161   Spearman = 0.0054
LOC:  	     p-value = 0.4995 (insignificant)   MH = 0.4560   odds ratio 1.0648 (0.8873, 1.2779)
IRABAL_Bin:  p-value < 0.0001 (significant)     MH = 145.8576 odds ratio 3.2160 (2.6362, 3.9233)
IRA:	     p-value < 0.0001 (significant)     MH = 161.6669 odds ratio 3.1848 (2.6419, 3.8393)
INVBAL_Bin:  p-value < 0.0001 (significant)     MH = 46.1903  Spearman = 0.0783
INV:	     p-value < 0.0001 (significant)     MH = 91.6184  odds ratio 3.4720 (2.6506, 4.5480)
		afterwards: still significant   MH = 105.4614 
ILSBAL_Bin:  p-value = 0.0561 (insignificant)   MH = 3.6498   odds ratio 0.7951 (0.6281, 1.0064)
ILS: 	     p-value = 0.0073 (insignificant)   MH = 7.2062   odds ratio 0.7452 (0.6008, 0.9243)
*/





