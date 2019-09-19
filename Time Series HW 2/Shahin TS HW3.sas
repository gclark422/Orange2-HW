/* Initialize Library */
libname tshw3 "C:/Users/Asus/Documents/Learning/Advanced Analytics/Time Series and Forecasting/Homework/Homework 2";

/* Import Data */
proc import file="C:/Users/Asus/Documents/Learning/Advanced Analytics/Time Series and Forecasting/Homework/Homework 2/Training.csv"
dbms=csv out=tshw3.training1 replace;
guessingrows=max;
run;

proc import file="C:/Users/Asus/Documents/Learning/Advanced Analytics/Time Series and Forecasting/Homework/Homework 2/Validation.csv"
dbms=csv out=tshw3.validation1 replace;
run;

/* Cleaning the Data - Completed */
/* Two Notes about editing the Training and Validation CSV files:
1) Added an additional header: "ID"
2) Changed the concentration variable to Mean_Concentration

data tshw3.training2 (drop=obs);
	set tshw3.training1(rename=( id = obs));
	id = input(obs, n.);
run;
	
data tshw3.validation2;
	set tshw3.validation1(rename=(mo=obs));
	id = input(obs, n.);
run;
*/


/* ADF Test - There appears to be stationarity around the trend line as evidenced by
the Trend component of the ADF test. Thus, not Random Walk, need to account for a linear trend */
proc arima data=tshw3.training2 plot=all;
	identify var=mean_concentration nlag=10 stationarity=(adf=2);
	identify var=mean_concentration(1) nlag=10 stationarity=(adf=2);
run;
quit;

/* Checking the Model */
/* Original Model with Trend Line Accounted For -  We have...some White Noise? */
proc arima data=tshw3.training2 plot(unpack)=all;
	identify var=mean_concentration crosscorr=id nlag=10;
	estimate input=id method=ML;
run;
quit;

/* Trend line accounted for with AR(1) based on PACF - White Noise!!! SBC = 228.9375 */
proc arima data=tshw3.training2 plot(unpack)=all;
	identify var=mean_concentration crosscorr=id nlag=10;
	estimate p=1 q=1 input=id method=ML;
run;
quit;

/* Trend line accounted for with MA(1) based on ACF - White Noise!!! SBC = 224.9831 */
proc arima data=tshw3.training2 plot(unpack)=all;
	identify var=mean_concentration crosscorr=id nlag=10;
	estimate q=1 input=id method=ML;
run;
quit;



