﻿* Bootstrapping survival diffs at last time point;


* 1) make fake data;

/*Simulating outcomes: hospitalization for CVD, all cause-mortality*/
/* See Bender 2005 (doi: 10.1002/sim.2059) */

data fake;
	call streaminit(722);
	shape1=1;
	do i = 1 to 100;
		exposure = (rand('NORMAL',0,1)>0); * make binary exposure;

		lp1 = log(0.5)*exposure;

		v = rand("uniform");
		t1 = (-log(v) / ( 0.011*exp(lp1)))**(1/shape1) ; 

		c=rand("exponential")*200;  *random drop out;
	
		t = min(t1,c);

		if t=t1 then delta=1;
		if t=c then delta=2;

	if t>10 then do; 
          t=10; delta=0; *admin censoring at 10 y; 
    end; 

	output;
	end;
run;

proc means data=fake; var exposure; run; * check;

ods select parameterestimates;
proc phreg data=fake;
	class exposure / desc;
 	 model t*delta(0,2) = exposure;
	 baseline out=surv1 survival=s;
run;

proc print data=surv1(obs=10); run;

* 2) Get observed statistic of interest;

data covs2;
input exposure;
datalines;
0
1
;
run;
proc print data=covs2; run;

ods select parameterestimates;
proc phreg data=fake;
	class exposure / desc;
	model t*delta(0,2) = exposure;
	baseline covariates=covs2 out=base survival=s stderr=se / method=pl; *output survival function and se;
run;

proc print data=base; run;

* Select last observation for each exposure to get comparison;

proc sort data=base; by exposure t; run;

data base2; set base;
	by exposure;
	if last.exposure;
	run;

proc print data=base2; run;

proc transpose data=base2 (keep=exposure s) out=base3; run;
data base3; set base3; if _n_=2;  diff = col1-col2; run; * risk diff for col2-col1;
proc print data=base3; run;

*store the estimated diff;
data _null_;
 set base3;
 if _n_=1 then  call symput('diffbar', diff);
run;

* 3) get bootstrap sample;
* SEE http://www.ats.ucla.edu/stat/sas/faq/bootstrap.htm;
* -------------------------------------------------------;

%let rep = 10;	
proc surveyselect data= fake out=bootsample
     seed = 124 method = urs
	 samprate = 1 outhits rep = &rep;
run;
ods listing close;

* 4) generate the data for diff in each bootstrap sample;
* -----------------------------------------------;

* get survival estimates for each replicate for each exposure;
proc phreg data=bootsample;
by replicate;
	class exposure / desc;
	model t*delta(0,2) = exposure;
	baseline covariates=covs2 out=base survival=s stderr=se / method=pl; *output survival function and se;
run;

proc freq data=base; table replicate; run; * check;

* pull out survival estimate for last time for each replicate;
proc sort data=base; by replicate exposure; run;
data base2; set base; 
by replicate exposure;
if last.exposure; 
run;

proc print data=base2(obs=20); run; * check;

* set up to get diff in S by exposure;
proc transpose data=base2 (keep=replicate exposure s) out=base3; by replicate; run;
data base3; set base3(where=(_name_='s')); diff=col1-col2; run;
proc print data=base3; run;


* 5) creating confidence interval, normal distribution theory method;
* using the t-distribution;
* -------------------------------------------;
%put &diffbar.;

%let alphalev = .05;
ods listing;
proc sql;
  select  &diffbar as diff,
          mean(diff) - &diffbar as bias, 
		  std(diff) as std_err,
          &diffbar - tinv(1-&alphalev/2, &rep-1)*std(diff) as lb,
          &diffbar + tinv(1-&alphalev/2, &rep-1)*std(diff) as hb
  from base3;
quit; 
