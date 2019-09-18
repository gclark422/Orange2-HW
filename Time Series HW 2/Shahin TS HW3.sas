/* Initialize Library */
libname tshw3 "C:/Users/Asus/Documents/Learning/Advanced Analytics/Time Series and Forecasting/Homework/Homework 2";

/* Import Data */
proc import file="C:/Users/Asus/Documents/Learning/Advanced Analytics/Time Series and Forecasting/Homework/Homework 2/Training.csv"
dbms=csv out=hw3.training1;
run;

proc import file="C:/Users/Asus/Documents/Learning/Advanced Analytics/Time Series and Forecasting/Homework/Homework 2/Validation.csv"
dbms=csv out=hw3.validation1;
run;

/* Cleaning the Data - Completed */
 /*
data hw3.training2 (drop=obs);
	set tshw3.training1(rename=(date = Mean_Concentration) rename=( mo = obs));
	mo = input(obs, n.);
run;
	
data hw3.validation2;
	set tshw3.validation1(rename=(date = Mean_Concentration) rename=(mo=obs));
	mo = input(obs, n.);
run;
*/


/* ADF Test - There appears to be stationarity around the trend line as evidenced by
the Trend component of the ADF test and the Zero Mean component of the ADF Test with 1 Lag 
Thus, not Random Walk, need to account for a linear trend */
proc arima data=tshw3.training2 plot=all;
	identify var=mean_concentration nlag=10 stationarity=(adf=2);
	identify var=mean_concentration(1) nlag=10 stationarity=(adf=2);
run;
quit;

/* Checking the Model */
/* Original Model with Trend Line Accounted For -  We have...some White Noise? */
proc arima data=tshw3.training2 plot(unpack)=all;
	identify var=mean_concentration crosscorr=Mo nlag=10;
	estimate input=Mo method=ML;
run;
quit;






