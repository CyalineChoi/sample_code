/* set up SAS dataset */
libname MyLibRef "/home/u39221714";

data pa_0;
  set MyLibRef.paexercise;
run;

proc contents data=pa_0;run;

/* exclude procedure codes other than 720, 721, 724, 726, 728, 729, 731, 733, 736, 738, 740, 741, 742, 744 */
/* create dataset with 2 new variables: pr_delivery, pr_csection */
data pa_clean_1;
set pa_0;
if whichn('720', of pr1-pr3)  then pr_delivery = 1;
if whichn('721', of pr1-pr3)  then pr_delivery = 1;
if whichn('724', of pr1-pr3)  then pr_delivery = 1;
if whichn('726', of pr1-pr3)  then pr_delivery = 1;
if whichn('728', of pr1-pr3)  then pr_delivery = 1;
if whichn('729', of pr1-pr3)  then pr_delivery = 1;
if whichn('731', of pr1-pr3)  then pr_delivery = 1;
if whichn('733', of pr1-pr3)  then pr_delivery = 1;
if whichn('736', of pr1-pr3)  then pr_delivery = 1;
if whichn('738', of pr1-pr3)  then pr_delivery = 1;
if whichn('740', of pr1-pr3)  then pr_delivery = 1;
if whichn('741', of pr1-pr3)  then pr_delivery = 1;
if whichn('742', of pr1-pr3)  then pr_delivery = 1;
if whichn('744', of pr1-pr3)  then pr_delivery = 1;

if whichn('740', of pr1-pr3)  then pr_csection = 1;
if whichn('741', of pr1-pr3)  then pr_csection = 1;
if whichn('742', of pr1-pr3)  then pr_csection = 1;
if whichn('744', of pr1-pr3)  then pr_csection = 1;
run;

data pa_clean_2;
set pa_clean_1;
if pr_delivery^=1 then delete;
drop paf sex id age pzip dx1 dx2 dx3;
run;

/* group pr_delivery and pr_csection by att_id */
proc sort data=pa_clean_2;
by att_id;
run;

/* identify physician who's race is black */
proc freq data=pa_clean_2;
table att_id*race/nocol nocum nofreq nopercent out=pa_freq ;
run;

data pa_freq_black;
set pa_freq;
if race^='B' then delete;
*drop paf sex id age pzip dx1 dx2 dx3;
run;

proc print data=pa_freq_black;run;
/* percentage of mothers delivering babies who are black */
proc freq data=pa_clean_2;
table race;*/nocol nocum nofreq nopercent;
run;

/* report */
PROC REPORT data = pa_clean_2 NOWINDOWS HEADLINE out=pa_report;
     column att_id pr_delivery pr_csection;
	 define att_id / group 'att_id';
	 define pr_delivery / analysis 'pr_delivery';
	 define pr_csection / analysis 'pr_csection';
RUN;

/* merge 2 dataset by att_id */
data master;
merge pa_report pa_freq_black;
run;
proc print data=master;run;

/* distribution */
proc sort data=master;
by att_id;
run;
proc univariate data=master plots;
var pr_delivery ;
run;
proc means data=master;
var pr_delivery  pr_csection;
run;

/* export data */
proc export data=master
    outfile="/home/u39221714/master.csv"
    dbms=csv;
run;

/* subset based on the average number of delivery */
data highv;
set master;
if pr_delivery<15.3409091 then delete;
run;
proc means data=highv;
var percent ;
title 'high volume doctors';
run;
data lowv;
set master;
if percent>15.3409091 then delete;
run;
proc means data=lowv;
var percent;
title 'low volume doctors';
run;




