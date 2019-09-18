/* Initialize Library */
libname hw3 "C:/Users/Asus/Documents/Learning/Advanced Analytics/Time Series and Forecasting/Homework/Homework 2";

/* Import Data */
proc import file="C:/Users/Asus/Documents/Learning/Advanced Analytics/Time Series and Forecasting/Homework/Homework 2/Training.csv"
dbms=csv out=hw3.training;
run;

proc import file="C:/Users/Asus/Documents/Learning/Advanced Analytics/Time Series and Forecasting/Homework/Homework 2/Validation.csv"
dbms=csv out=hw3.validation;
run;

/* Cleaning the Data */

data hw3.training;
	set hw3.training(rename=(date = Mean_Concentration));
run;
	
data hw3.validation;
	set hw3.validation(rename=(date = Mean_Concentration));
run;

/* Checking the Model */
/* Original Model - We don't have White Noise. The model needs further adjustment */
proc arima data=hw3.training plot(unpack)=all;
	identify var=mean_concentration nlag=10 outcov=Corr;
	estimate method=ML;
run;
quit;

/* ADF Test - There appears to be stationarity around the trend line as evidenced by
the Trend component of the ADF test and the Zero Mean component of the ADF Test with 1 Lag */
proc arima data=hw3.training plot=all;
	identify var=mean_concentration nlag=10 stationarity=(adf=2);
	identify var=mean_concentration(1) nlag=10 stationarity=(adf=2);
run;
quit;



