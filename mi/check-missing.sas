* ----------------------------;
* Investigator: Jen Yourkavitch
* Analyst/Programmer: Jen Yourkavitch
* Program: wts_mi_code_avh_check.sas
* Task: check missing data patterns in data
* Date: April, 2017
* --------------------------------;

options nofmterr nonotes;
libname dissert "/pine/scr/v/o/vonholle/jen.y";

data diss1;
set dissert.diss1;
run;

	proc mi data=diss1 nimpute=0;
	var expo work bied white nobfexp p31 careplan ret earlyprac noprohelp infageret mbsep1 wenvir no2worksup time event1 income lpt m2bfed marital anybf2mod;
	run;
