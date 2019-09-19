/*Library creation*/
libname logist1 "C:\Users\Price Burnett\Downloads\Homework2_LR";run;

/*Data Transformation*/
data temp;
	set logist1.insurance_t_bin;
	if hmown = . then hmown = -1;
	if inv = . then inv = -1;
	if ccpurc = . then ccpurc = -1;
	if cc = . then cc = -1;
	if mmcred = 5 then mmcred = 3;
	if cashbk = 2 then cashbk = 1;
run;

