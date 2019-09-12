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

/*-------------Sav Variable-------------*/
/*Check Missing*/
proc freq data = logist2.insurance_t_bin;
	tables sav;
run;

/*Check Separation*/
proc freq data = logist2.insurance_t_bin ;
	tables sav*ins/ norow nocol nopercent;
run;

/*Check Significance*/
/*MH chi sq and Odds Ratio*/
proc freq data=logist2.insurance_t_bin;
 tables ins*sav / chisq expected cellchi2 nocol nopercent oddsratio;
run;

/*-------------posamt_bin Variable-------------*/
/*Check Missing*/
proc freq data = logist2.insurance_t_bin;
	tables posat_bin;
run;

/*Check Separation*/
proc freq data = logist2.insurance_t_bin ;
	tables posamt_bin*ins/ norow nocol nopercent;
run;

/*Check Significance*/
/*MH chi sq and Odds Ratio*/
proc freq data=logist2.insurance_t_bin;
 tables posamt_bin*sav / chisq expected cellchi2 nocol nopercent oddsratio;
run;

/*-------------pos_bin Variable-------------*/
/*Check Missing*/
proc freq data = logist2.insurance_t_bin;
	tables pos_bin;
run;

/*Check Separation*/
proc freq data = logist2.insurance_t_bin ;
	tables pos_bin*ins/ norow nocol nopercent;
run;

/*Check Significance*/
/*MH chi sq and Odds Ratio*/
proc freq data=logist2.insurance_t_bin;
 tables ins*pos_bin / chisq expected cellchi2 nocol nopercent oddsratio;
run;

/*-------------phone_bin Variable-------------*/
/*Check Missing*/
proc freq data = logist2.insurance_t_bin;
	tables phone_bin;
run;

/*Check Separation*/
proc freq data = logist2.insurance_t_bin ;
	tables phone_bin*ins/ norow nocol nopercent;
run;

/*Check Significance*/
/*MH chi sq and Odds Ratio*/
proc freq data=logist2.insurance_t_bin;
 tables ins*phone_bin / chisq expected cellchi2 nocol nopercent oddsratio;
run;

/*-------------CDBAL_bin Variable-------------*/
/*Check Missing*/
proc freq data = logist2.insurance_t_bin;
	tables cdbal_bin;
run;

/*Check Separation*/
proc freq data = logist2.insurance_t_bin ;
	tables cdbal_bin*ins/ norow nocol nopercent;
run;

/*Check Significance*/
/*MH chi sq and Odds Ratio*/
proc freq data=logist2.insurance_t_bin;
 tables ins*cdbal_bin / chisq expected cellchi2 nocol nopercent oddsratio;
run;

/*-------------CD Variable-------------*/
/*Check Missing*/
proc freq data = logist2.insurance_t_bin;
	tables cd;
run;

/*Check Separation*/
proc freq data = logist2.insurance_t_bin ;
	tables cd*ins/ norow nocol nopercent;
run;

/*Check Significance*/
/*MH chi sq and Odds Ratio*/
proc freq data=logist2.insurance_t_bin;
 tables ins*cd / chisq expected cellchi2 nocol nopercent oddsratio;
run;

/*-------------atmamt_bin Variable-------------*/
/*Check Missing*/
proc freq data = logist2.insurance_t_bin;
	tables atmamt_bin;
run;

/*Check Separation*/
proc freq data = logist2.insurance_t_bin ;
	tables atmamt_bin*ins/ norow nocol nopercent;
run;

/*Check Significance*/
/*MH chi sq and Odds Ratio*/
proc freq data=logist2.insurance_t_bin;
 tables ins*atmamt_bin / chisq expected cellchi2 nocol nopercent oddsratio;
run;

/*-------------ATM Variable-------------*/
/*Check Missing*/
proc freq data = logist2.insurance_t_bin;
	tables atm;
run;

/*Check Separation*/
proc freq data = logist2.insurance_t_bin ;
	tables atm*ins/ norow nocol nopercent;
run;

/*Check Significance*/
/*MH chi sq and Odds Ratio*/
proc freq data=logist2.insurance_t_bin;
 tables ins*atm / chisq expected cellchi2 nocol nopercent oddsratio;
run;
