/* Initialize the libref */
libname hw2 "C:\Users\Asus\Documents\Learning\Advanced Analytics\Logistic Regression\Homework\Homework 2";

/* NSFAMT_Bin, None missing, No Separation Concerns, MH = 42.4893, OR = 0.4892 */
proc freq data=hw2.insurance_t_bin;
	table ins*nsfamt_bin / chisq expected cellchi2 nocol nopercent oddsratio;
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

