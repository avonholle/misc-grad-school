* ----------------------------;
* Investigator: Jen Yourkavitch
* Analyst/Programmer: Jen Yourkavitch
* Program: wts_mi_code_avh_check.sas
* Task: debug mi algorith for dissertation analyses
* Date: April, 2017
* --------------------------------;

options nofmterr nonotes;
libname dissert "/pine/scr/v/o/vonholle/jen.y";

data diss1;
set dissert.diss1;
run;


proc means data=diss1 n nmiss mean sum min max; 
	var time event1 expo work income bied white nobfexp p31 careplan ret earlyprac noprohelp infageret mbsep1 lpt wenvir no2worksup;
	where m2bfed=1;
	title "Var check";
	run;


*MI, assuming MVN.;

	proc mi data=diss1 seed=3 nimpute=2 out=c; 
	var expo work bied white nobfexp p31 careplan ret earlyprac noprohelp infageret mbsep1 wenvir no2worksup time event1 income lpt m2bfed marital;
	mcmc; *default is chain=single nbiter=200 niter=100 prior=Jeffreys initial=EM;
	run;

	proc means data=c n nmiss mean sum min max; 
	var time event1 expo work income bied white nobfexp p31 careplan ret earlyprac noprohelp infageret mbsep1 lpt wenvir no2worksup;
	where m2bfed=1;
	title "Var check";
	run;

* create weights with this dataset c;

* This is 718 code from 2016. QUESTION 3 ;
	** Estimate PS for each individual in the dataset using a well-specified model **;
	proc logistic data=c descending ;
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
			proc genmod data = tables2 descending;
			model anybf2mod = / link = log dist = binomial;
by _imputation_;
			estimate 'marg' int 1 / exp;
			output out=iptw p=marg_pr_exp;
			run; *not working: insufficient space in file work.iptw.data; *disk full";

					
	* Create stabilized and unstabilized inverse probability weights ;
		data iptw; set iptw;
			sw = anybf2mod*marg_pr_exp/ps + (1-anybf2mod)*(1-marg_pr_exp)/(1-ps);
			w = 1/ps + (1-anybf2mod)/(1-ps);
			run;
			proc univariate data = iptw;
				var w sw;
				run;

		
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


	*IP WEIGHTS;

	*Model for numerator of weights, to stabilize variance;
proc logistic data=iptw desc noprint; *this is not MI dataset;
	model expo=; 
	*by _imputation_;
	output out=n p=n;
	run;

*Model for denominator of weights, to control confounding;
proc logistic data=iptw desc noprint; 
	model expo=income bied work white nobfexp p31 careplan ret earlyprac noprohelp infageret mbsep1 lpt wenvir no2worksup; 
	*by _imputation_;
	output out=d p=d;
	run; *this blows up SAS: "file work.n.data is damaged";

*Construct weights;
data iptw;
	merge iptw n d;
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
	output out=q pctlpts=20 40 60 80 pctlpre=p;
	run; 
data q; set q; p0=0; p100=10; z=1;
run;
proc print data=q noobs; 
	var p0 p20 p40 p60 p80 p100;
	title "Quantiles of the drop out distribution";
	run; *Note that p20 and p60 have 0 obs--?;

*Expand data to up to 5 records per unit;
data e; merge iptw2 q; by z;
	array j{6} p0 p20 p40 p60 p80 p100;
	do k=1 to 5;
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
proc sort data=e; by sampmiq in;
run;

*drop-out numerator model. ;
proc logistic data=e noprint; 
	class in/param=ref desc; 
	model _drop=in;
	output out=nm2(keep=sampmiq _drop nm2 in out) prob=nm2;
	run;
*drop-out denominator model. ;
proc logistic data=e noprint; 	
	class in/param=ref desc; 
	model _drop=in expo work income bied white nobfexp p31 careplan ret earlyprac noprohelp infageret mbsep1 lpt wenvir no2worksup;
	output out=dn2(keep=sampmiq _drop dn2 in out) prob=dn2;
	run;
*drop-out weights;
proc sort data=nm2; by sampmiq in; run;
proc sort data=dn2; by sampmiq in; run;
data f; merge e nm2 dn2; by sampmiq in; retain num den;
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
	model time*event1(0)=expo work income bied white nobfexp p31 careplan ret earlyprac noprohelp infageret mbsep1 lpt wenvir no2worksup/rl; 
	weight w3;
	by _imputation_; 
	run;

	proc mianalyze data=d; 
	modeleffects expo bied white nobfexp work income p31 careplan ret earlyprac noprohelp infageret mbsep1 wenvir lpt no2worksup;	
	title "Multiple imputation";
	run;


