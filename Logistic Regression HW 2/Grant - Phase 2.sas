libname logreg "C:\Users\grant\Desktop\IAA\Orange2-HW\Data\Logistic HW 2";

/***** Check missing values ******/

proc means data=logreg.insurance_t_bin nmiss;
	var MOVED LORES_Bin INAREA HMVAL_Bin HMOWN CRSCORE_Bin AGE_Bin;
run;

proc freq data=logreg.insurance_t_bin;
	table RES;
run;