/*Create Library*/
libname ts3 "C:/Users/Price Burnett/Documents/SASandSQL";

/* Import Training Data */
proc import file="C:/Users/Price Burnett/Documents/Orange2-HW/Data/Time Series HW 2/Training.csv"
dbms=csv out=ts3.training replace;
guessingrows=max;
run;

/*Import Validation Data*/
proc import file="C:/Users/Price Burnett/Documents/Orange2-HW/Data/Time Series HW 2/Validation.csv"
dbms=csv out=ts3.validation replace;
run;

/*Make Id numeric*/
data ts3.training;
	set ts3.training;
	ID2 = input(ID,int8.);
run;

/*Check for Stationarity*/
proc arima data=ts3.training plot=all;
	identify var=mean_concentration nlag=10 stationarity=(adf=2);
	identify var=mean_concentration(1) nlag=10 stationarity=(adf=2);
run;
quit;


/*White Noise Evaluation*/
proc arima data=ts3.training plot(unpack)=all;
	identify var=mean_concentration crosscorr=id2 nlag=10;
	estimate input=id2 method=ML;
run;
quit;