libname tshw3 "C:\Users\grant\Desktop\IAA\Orange2-HW\Data\Time Series HW 2";

/* Import Data */
proc import 
		file="C:\Users\grant\Desktop\IAA\Orange2-HW\Data\Time Series HW 2\Training.csv" 
		dbms=csv out=tshw3.training1 replace;
	guessingrows=max;
run;

proc import file="C:\Users\grant\Desktop\IAA\Orange2-HW\Data\Time Series HW 2\Validation.csv" 
		dbms=csv out=tshw3.validation1 replace;
run;

data tshw3.training2 (drop=obs);
	set tshw3.training1(rename=(id=obs));
	id=input(obs, n.);
run;

data tshw3.validation2;
	set tshw3.validation1(rename=(mo=obs));
	id=input(obs, n.);
run;

/* Checking for stationarity */
proc arima data=tshw3.training2 plot=all;
	identify var=mean_concentration nlag=10 stationarity=(adf=2);
	identify var=mean_concentration(1) nlag=10 stationarity=(adf=2);
	run;
quit;

/* accounting for trend */
proc arima data=tshw3.training2 plot(unpack)=all;
	identify var=mean_concentration crosscorr=id nlag=10;
	estimate input=id method=ML;
	run;
quit;