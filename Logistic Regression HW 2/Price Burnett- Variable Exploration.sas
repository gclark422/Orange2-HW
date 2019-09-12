/*Library creation*/
libname logist2 "C:\Users\Price Burnett\Downloads\Homework2_LR";run;

/*-------------Teller Variable-------------*/
/*Check Missing*/
proc freq data = logist2.insurance_t_bin;
	tables teller_bin;
run;

/*Check Separation*/
proc freq data = logist2.insurance_t_bin ;
	tables teller_bin*ins/ norow nocol nopercent;
run;

/*Check Significance*/
/*MH chi sq and Odds Ratio*/
proc freq data=logist2.insurance_t_bin;
 tables ins*teller_bin / chisq expected cellchi2 nocol nopercent oddsratio;
run;

/*-------------SavBal_bin Variable-------------*/
/*Check Missing*/
proc freq data = logist2.insurance_t_bin;
	tables savbal_bin;
run;

/*Check Separation*/
proc freq data = logist2.insurance_t_bin ;
	tables savbal_bin*ins/ norow nocol nopercent;
run;

/*Check Significance*/
/*MH chi sq and Odds Ratio*/
proc freq data=logist2.insurance_t_bin;
 tables ins*savbal_bin / chisq expected cellchi2 nocol nopercent oddsratio;
run;
