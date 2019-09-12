/*Logistic Homework Phase II Variable Selection and Modeling Building*/

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

/*MMCRED  */
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
 
  
