/* Initialize the libref */
libname hw3 "Z:\Documents\Fall I 2019\Logistic Regression\Homework2_LR";

/* Data Cleaning */
data train;
	set hw3.insurance_t_bin;
	if hmown = . then hmown = -1;
	if inv = . then inv = -1;
	if ccpurc = . then ccpurc = -1;
	if cc = . then cc = -1;
	if mmcred = 5 then mmcred = 3;
	if cashbk = 2 then cashbk = 1;
run;

data val;
	set hw3.insurance_v_bin;
	if hmown = . then hmown = -1;
	if inv = . then inv = -1;
	if ccpurc = . then ccpurc = -1;
	if cc = . then cc = -1;
	if mmcred = 5 then mmcred = 3;
	if cashbk = 2 then cashbk = 1;
run;

proc freq data=val;
	tables (NSFAMT_Bin NSF DIRDEP DEPAMT_Bin DDABAL_Bin DDA CHECKS_Bin CASHBK ACCTAGE_Bin TELLER_bin SAVBAL_bin SAV
			POSAMT_bin POS_bin PHONE_bin CDBAL_bin CD ATMAMT_bin ATM RES MOVED LORES_bin INAREA HMVAL_bin
			HMOWN CRSCORE_bin BRANCH AGE_bin MM LOCBAL_Bin LOC IRABAL_Bin IRA INVBAL_Bin INV ILSBAL_Bin ILS SDB MTGBAL_Bin
			MTG MMCRED MMBAL_Bin INCOME_Bin CCPURC CCBAL_Bin CC)* INS;
run;

/*Check for quasi-separtion in validation data among vars in the final model*/
proc freq data=val;
	tables (nsf dda ddabal_bin checks_bin teller_bin savbal_bin cdbal_bin
		atmamt_bin branch mm ira inv ils cc ddabal_bin*savbal_bin mm*ddabal_bin dda*ira) *ins;
run;

/* Concordance = 80.8 and AUC = 80.85, ROC curve at bottom of output 
17 vars significant from the forward selection w/ interactions from Phase II*/
proc logistic data=train plots(only) = ROC;
	class nsf dda ddabal_bin(ref='1') checks_bin(ref='1') teller_bin(ref='1') savbal_bin(ref='1') 
	cdbal_bin(ref='1') atmamt_bin(ref='1')	branch(ref='B1') mm ira	inv(ref='-1') ils cc(ref='-1') 
	/ param=ref; 
	model ins(event='1') = nsf dda ddabal_bin checks_bin teller_bin savbal_bin cdbal_bin
		atmamt_bin branch mm ira inv ils cc ddabal_bin*savbal_bin mm*ddabal_bin dda*ira / clodds=pl clparm=pl;
run;
quit;

/* Discrimination Slope 0.2648*/
ods trace on;
ods graphics on;
proc logistic data=train noprint;
	class nsf dda ddabal_bin(ref='1') checks_bin(ref='1') teller_bin(ref='1') savbal_bin(ref='1') 
	cdbal_bin(ref='1') atmamt_bin(ref='1')	branch(ref='B1') mm ira	inv(ref='-1') ils cc(ref='-1') 
	/ param=ref; 
	model ins(event='1') = nsf dda ddabal_bin checks_bin teller_bin savbal_bin cdbal_bin
		atmamt_bin branch mm ira inv ils cc ddabal_bin*savbal_bin mm*ddabal_bin dda*ira / clodds=pl clparm=pl;
	output out=predprobs p=phat;
run;
 
proc sort data=predprobs;
	by descending ins;
run;

proc ttest data=predprobs order=data;
	ods select statistics summarypanel;
	class ins;
	var phat;
	title 'Coefficient of Discrimination and Plots';
run;
ods trace off;

/*Change the Title of the histogram*/

proc template;
   source  Stat.TTest.Graphics.Summary2;
run;

 proc template;
define statgraph Stat.TTest.Graphics.Summary2;
   notes
      "Comparative histograms with normal/kernel densities and boxplots,             (two-sample)";
   dynamic _Y1 _Y2 _Y _VARNAME _XLAB _SHORTXLAB _CLASS1 _CLASS2 _CLASSNAME _LOGNORMAL _OBSVAR
      _byline_ _bytitle_ _byfootnote_;
   BeginGraph;
      entrytitle "Distribution of Parameter Estimates";
      layout lattice / rows=3 columns=1 columndatarange=unionall rowweights=(.4 .4 .2)
         shrinkfonts=true;
         columnaxes;
            columnaxis / display=(ticks tickvalues label) label=_XLAB shortlabel=_SHORTXLAB
               griddisplay=auto_on;
         endcolumnaxes;
         layout overlay / xaxisopts=(display=none);
            histogram _Y1 / binaxis=false primary=true;
            if ((NOT EXISTS(_LOGNORMAL)) AND (NOT(EXISTS(_PAIRED) AND EXISTS(_RATIO))))
               densityplot _Y1 / normal () name="Normal" legendlabel="Normal" lineattrs=GRAPHFIT;
            endif;
            densityplot _Y1 / kernel () name="Kernel" legendlabel="Kernel" lineattrs=GRAPHFIT2;
            entry _CLASS1 / autoalign=(topleft topright top);
         endlayout;
         layout overlay / xaxisopts=(display=none);
            histogram _Y2 / binaxis=false primary=true;
            if ((NOT EXISTS(_LOGNORMAL)) AND (NOT(EXISTS(_PAIRED) AND EXISTS(_RATIO))))
               densityplot _Y2 / normal () name="Normal" legendlabel="Normal" lineattrs=GRAPHFIT;
            endif;
            densityplot _Y2 / kernel () name="Kernel" legendlabel="Kernel" lineattrs=GRAPHFIT2;
            entry _CLASS2 / autoalign=(topleft topright top);
         endlayout;
         columnheaders;
            layout gridded / shrinkfonts=true;
               discreteLegend "Normal" "Kernel" / across=4 BackgroundColor=GraphWalls:Color
                  Opaque=true;
            endlayout;
         endcolumnheaders;
         layout overlay / xaxisopts=(display=none) yaxisopts=(label=_CLASSNAME reverse=true);
            if (EXISTS(_LOGNORMAL))
               boxplot X=CLASS Y=_Y / orient=horizontal display=(caps fill median outliers);
            else
               boxplot X=CLASS Y=_Y / orient=horizontal;
            endif;
         endlayout;
         columnheaders;
            layout gridded / shrinkfonts=true;
               discreteLegend "Normal" "Kernel" / across=4 BackgroundColor=GraphWalls:Color
                  Opaque=true;
            endlayout;
         endcolumnheaders;
      endlayout;
      if (_BYTITLE_)
         entrytitle _BYLINE_ / textattrs=GRAPHVALUETEXT;
      else
         if (_BYFOOTNOTE_)
            entryfootnote halign=left _BYLINE_;
         endif;
      endif;
   EndGraph;
end;
run;

/* K-S Statistic 0.299877*/
proc npar1way data=predprobs d plot=edfplot;
	class ins;
	var phat;
run;

/* Classification Table Using Validation Data*/
/*Percent Concordant 82.2 */
proc logistic data=val plots(only)=(oddsratio);
	class nsf dda ddabal_bin(ref='1') checks_bin(ref='1') teller_bin(ref='1') savbal_bin(ref='1') 
	cdbal_bin(ref='1') atmamt_bin(ref='1')	branch(ref='B1') mm ira	inv(ref='-1') ils cc(ref='-1') 
	/ param=ref; 
	model ins(event='1') = nsf dda ddabal_bin checks_bin teller_bin savbal_bin cdbal_bin
		atmamt_bin branch mm ira inv ils cc ddabal_bin*savbal_bin mm*ddabal_bin dda*ira / ctable pprob =0.299877;
	ods output classification=classtable;
	title 'Modeling Insurance Products';
run;
quit;

proc print data=classtable;
run;

/* Lift Chart Validation Data */
/*Total 0's & 1's in the validation data 2124*/
/*Population proportion of 1 in training data set .3493*/
proc freq data=val;
	tables ins;
run;

proc logistic data=train plots(only)=(oddsratio);
	class nsf dda ddabal_bin(ref='1') checks_bin(ref='1') teller_bin(ref='1') savbal_bin(ref='1') 
	cdbal_bin(ref='1') atmamt_bin(ref='1')	branch(ref='B1') mm ira	inv(ref='-1') ils cc(ref='-1') 
	/ param=ref; 
	model ins(event='1') = nsf dda ddabal_bin checks_bin teller_bin savbal_bin cdbal_bin
		atmamt_bin branch mm ira inv ils cc ddabal_bin*savbal_bin mm*ddabal_bin dda*ira / clodds=pl clparm=pl;
	score data=val fitstat outroc=roc;
run;
quit;

data work.roc; 
	set work.roc; 
	cutoff = _PROB_; 
	specif = 1-_1MSPEC_; 
	depth=(_POS_+_FALPOS_)/2124*100; 
	precision=_POS_/(_POS_+_FALPOS_); 
	acc=_POS_+_NEG_;
	lift=precision/0.3493; 
run;



/*Lift Plot*/
proc sgplot data=work.roc; 
	series y=lift x=depth; 
	refline 1.0 / axis=y; 
	title1 "Lift Chart for Training Data"; 
	xaxis label="Depth (%)";
	yaxis label="Lift";
run; 
quit;
