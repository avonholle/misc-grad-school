* ----------------------------;
* Investigator: Jen Yourkavitch
* Analyst/Programmer: Jen Yourkavitch
* Program: wts_mi_code_avh_check.sas
* Task: debug mi algorith for dissertation analyses
* Date: April, 2017
* --------------------------------;

options nofmterr nonotes;
libname dissert "/pine/scr/v/o/vonholle/jen.y";
*libname dissert "C:\Users\vonholle\Dropbox\unc.grad.school\misc\programs\misc-grad-school\mi\";
ods select attributes;
proc contents data=dissert.diss1; run;

* Note: over 3000 variables in data set. Need to include only variables used in analyses;
* variables to use in analysis;
%let vars = time expo work bied white nobfexp p31 careplan ret earlyprac 
			noprohelp infageret mbsep1 wenvir no2worksup  event1 income 
			lpt m2bfed marital educ p9 n34 anybf2mod sampmiq;

%let imputevars = time work bied white nobfexp p31 careplan ret earlyprac 
			noprohelp infageret mbsep1 wenvir no2worksup  event1 income 
			lpt m2bfed marital educ p9 n34;

data diss1;
set dissert.diss1(keep=&vars.);
miss_n = cmiss(of &vars.);
run;

proc freq data=diss1; table miss_n; run; run; * check;


data diss1; set  diss1(where=(miss_n<12 )); * leave in people with 11 or fewer missing observations of xx variables included in analyses (see list above);
run;


proc means data=diss1 n nmiss mean sum min max; 
	var &imputevars.;
	where m2bfed=1;
	title "Var check";
	run;

/*
* Look at missing patterns;
	proc mi data=diss1 nimpute=0;
	var &imputevars.;
	run;
*/

*MI, assuming MVN.;

	proc mi data=diss1 seed=3 nimpute=1000 out=c noprint; 
	var &imputevars.;
 	em maxiter=1000 converge=1e-4 ;
	run;

	proc means data=c n nmiss mean sum min max; 
	var &imputevars.;
	where m2bfed=1;
	title "Var check";
	run;

* create weights with this dataset c;

* This is 718 code from 2016. QUESTION 3 ;
	** Estimate PS for each individual in the dataset using a well-specified model **;
	proc logistic data=c descending noprint ;
	 	model anybf2mod =	educ p9 n34 marital;
					/* add in a bunch of interaction terms to get at multidimensional standardization */
					
		by _imputation_;
	 	output out=tables2 predicted=ps;
		run;
		proc univariate data = tables2;
			var ps;
			run;

			proc univariate data = tables2;
			var ps;
			run;

			ods exclude all;
			proc genmod data = tables2 descending;
			model anybf2mod = / link = log dist = binomial;
			by _imputation_;
			estimate 'marg' int 1 / exp;
			output out=iptw p=marg_pr_exp;
			run; *not working: insufficient space in file work.iptw.data; *disk full";
			ods exclude none;
					
	* Create stabilized and unstabilized inverse probability weights ;
		data iptw; set iptw;
			sw = anybf2mod*marg_pr_exp/ps + (1-anybf2mod)*(1-marg_pr_exp)/(1-ps);
			w = 1/ps + (1-anybf2mod)/(1-ps);
			run;
			proc univariate data = iptw;
				var w sw;
				run;

/*
	* Sort for characterization by exposure status ;
		proc sort data = iptw out = iptw;
			by descending anybf2mod;
			run;


	** Check distribution of weights **;

		* first, in the iptw dataset ;
			proc univariate data = iptw nextrobs=0;
				var sw;
				title 'iptw data'; run; title;
			
		* second, by anybf2mod status ;
			proc univariate data = iptw nextrobs=0;
				by descending anybf2mod;
				var sw;
				title 'by anybf2mod status'; run; title;
*/

	*IP WEIGHTS;

	*Model for numerator of weights, to stabilize variance;
proc logistic data=iptw desc noprint; *this is not MI dataset;
	model expo=; 
	by _imputation_;
	output out=n p=n;
	run;

*Model for denominator of weights, to control confounding;
proc logistic data=iptw desc noprint; 
	model expo=income bied work white nobfexp p31 careplan ret earlyprac noprohelp infageret mbsep1 lpt wenvir no2worksup; 
	by _imputation_;
	output out=d p=d;
	run; *this blows up SAS: "file work.n.data is damaged";

*Construct weights;
data iptw;
	merge iptw n d;
	by _imputation_;
	if expo then w=n/d;
	else w=(1-n)/(1-d);
	label n= d=;
	drop _level_;
	run;
proc means data=iptw;
	var n d w;
	title "IP confounding weights";
	run;
data iptw1;
set iptw;
scon=sw*w; *this is the selection weight times the confounding weight;
run;

****IP-censoring weights ;

data iptw2;
set iptw1;
if event1=0 and time LE 52 then drop=1;
else drop=0;
run;
proc freq data=iptw2;
tables drop/missing;
where m2bfed=1;
run;

*Add a constant for merging;
data iptw2; set iptw2; retain z 1;
run;

*Grab quintiles of the observed drop out times to merge with data;
proc univariate data=iptw2 noprint;
	where drop=1; var time;
	output out=q pctlpts= 50  pctlpre=p;
	by _imputation_;
	run; 
data q; set q; p0=0; p100=65; z=1; * 65 is max time in years for this data set;
run;

proc print data=q(obs=10) noobs; 
	var p0 p50 p100;
	title "Quantiles of the drop out distribution";
	run; *Note that p20 and p60 have 0 obs--?;

	*NOTE: these data don't have a lot of variability of time to event. dropped percentiles to just median. ;
	* many (over 25%) of people have 0 time to event. Need to look into that.;
*Expand data to up to xx records per unit;
data e; merge iptw2 q; 
by z _imputation_;
	array j{3} p0 p50 p100;
	do k=1 to 2;
		in=j(k);
		if j(k)<time<=j(k+1) then do; 
			out=time; 
			event2=event1; *make a time-varying event indicator;
			_drop=drop; *make a time-varying drop indicator;
			output; 
		end;
		else if j(k+1)<time then do; out=j(k+1); event2=0; _drop=0; output; end;
	end;
	run;

proc sort data=e; by _imputation_ sampmiq in;
run;

*drop-out numerator model. ;
proc logistic data=e noprint; 
	class in/param=ref desc; 
	by _imputation_;
	model _drop=in;
	output out=nm2(keep=sampmiq _drop nm2 in out _imputation_) prob=nm2;
	run;
*drop-out denominator model. ;
proc logistic data=e noprint; 	
	class in/param=ref desc; 
	model _drop=in expo work income bied white nobfexp p31 careplan ret earlyprac noprohelp infageret mbsep1 lpt wenvir no2worksup;
	by _imputation_;
	output out=dn2(keep=sampmiq _drop dn2 in out _imputation_) prob=dn2;
	run;
*drop-out weights;
proc sort data=nm2; by _imputation_ sampmiq in; run;
proc sort data=dn2; by  _imputation_ sampmiq in; run;
data f; merge e nm2 dn2; by _imputation_ sampmiq in; retain num den;
	if first.sampmiq then do; num=1; den=1; end;
	num=num*nm2;
	den=den*dn2;
	if _drop then w2=(1-num)/(1-den); else w2=num/den;
	w3=scon*w2; *this is IP weight (times selection wt) times censoring wt;
	label nm2= dn2=;
	run;

proc means data=f; 
	var scon w2 w3 num den;  
	title "Weights";
	run;


*IP-confounding-and-drop-out weighted curves;
*The following code gives crude curves, too. From Paul Allison book. 3-11: use this. has legends.;
	
proc means data=f n nmiss mean sum min max; 
	var time event1 expo work income bied white nobfexp p31 careplan ret earlyprac noprohelp infageret mbsep1 lpt wenvir no2worksup w3 sw w w2;
	where m2bfed=1;
	title "Var check";
	run;
	
*MI, assuming MVN.;

	proc phreg data=f covout outest=d noprint;
	model time*event1(0)=expo /rl; * NOTE: no need to include confounders here since you have iptw;
	weight w3;
	by _imputation_; 
	run;

	proc mianalyze data=d; 
	modeleffects expo ;	
	title "Multiple imputation";
	run;


