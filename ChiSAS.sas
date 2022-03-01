* Q1 import 2 datasets;
FILENAME REFFILE '/home/u39221714/sasuser.v94/SAS code test_data sets.xlsx';

PROC IMPORT DATAFILE=REFFILE
	DBMS=XLSX
	OUT=Q1_dat1;
	GETNAMES=YES;
RUN;
PROC IMPORT DATAFILE=REFFILE
	DBMS=XLSX
	OUT=Q1_dat2;
	GETNAMES=YES;
	SHEET="Q1_dat2";
RUN;

PROC CONTENTS DATA=Q1_dat1; RUN;
PROC CONTENTS DATA=Q1_dat2; RUN;

proc print data=Q1_dat1;
proc print data=Q1_dat2;

proc sort data=Q1_dat1;by Respondent_ID;run;
proc sort data=Q1_dat2;by Respondent_ID;run;
data Q1;
* merge datasets;
merge Q1_dat1 Q1_dat2;
by Respondent_ID;
* change formats;
chht_6mo_n = input(chht_6mo, 4.);drop chht_6mo;
label chht_6mo_n=Child’s height at 6 mo (cm); 
chht_30mo_n = input(chht_30mo, 4.);drop chht_30mo;
label chht_30mo_n=Child’s height at 30 mo (cm); 
chht_48mo_n = input(chht_48mo, 4.);drop chht_48mo;
label chht_48mo_n=Child’s height at 48 mo (cm); 
chwt_6mo_n = input(chwt_6mo, 4.);drop chwt_6mo;
label chwt_6mo_n=Child’s weight at 6 mo (kg); 
chwt_30mo_n = input(chwt_30mo, 4.);drop chwt_30mo;
label chwt_30mo_n=Child’s weight at 30 mo (kg); 
chwt_48mo_n = input(chwt_48mo, 4.);drop chwt_48mo;
label chwt_48mo_n=Child’s weight at 48 mo (kg);
run;
proc CONTENTS data=Q1;
proc print data=Q1;
*select first child;
proc sort data=Q1 nodupkey;by Respondent_ID;run;
proc export 
  data=Q1
  dbms=xlsx 
  outfile="/home/u39221714/sasuser.v94/firstchild.xlsx" 
  replace;
run;
proc sort data=Q1 nodupkey;by ch_dob;run;
* 07/27/2013 first, 05/25/2017 last;
* descriptive analysis by age;
data Q1_age;
set Q1;
age=floor(YRDIF(ch_dob, today(),"AGE"));
run;
proc sort data=Q1_age ;by age;run;
proc univariate data=Q1_age;var chht_6mo_n chht_30mo_n chht_48mo_n chwt_6mo_n chwt_30mo_n chwt_48mo_n;
by age; run;

*Q2;
data q2;
set '/home/u39221714/q2_dat (1).sas7bdat';
run;
* create new variable;
data q2_whz;
set q2;
chwhz = 0;
if chwhz_6mo=1 then chwhz = 1; 
else if chwhz_14mo=1 then chwhz = 1;
else if chwhz_22mo=1 then chwhz = 1;
else if chwhz_30mo=1 then chwhz = 1;
else if chwhz_42mo=1 then chwhz = 1;
else if chwhz_48mo=1 then chwhz = 1;
else if chwhz_84mo=1 then chwhz = 1;
run;
* create frenquency table;
proc freq data=q2_whz;
table chwhz_6mo chwhz_14mo chwhz_22mo chwhz_30mo chwhz_42mo chwhz_48mo chwhz_84mo chwhz/MISSPRINT;
run;
* 5 years old variables;
proc print data=q2_whz;
data q2_whz_5;
set q2_whz;
if chage_6mo>5 and chwhz=1 then over_5_whz = 1; 
else if chage_14mo>5 and chwhz=1 then over_5_whz = 1;
else if chage_22mo>5 and chwhz=1 then over_5_whz = 1;
else if chage_30mo>5 and chwhz=1 then over_5_whz = 1;
else if chage_42mo>5 and chwhz=1 then over_5_whz = 1;
else if chage_48mo>5 and chwhz=1 then over_5_whz = 1;
else if chage_84mo>5 and chwhz=1 then over_5_whz = 1;

if chage_6mo<=5 and chwhz=1 then lt_5_whz = 1; 
else if chage_14mo<=5 and chwhz=1 then lt_5_whz = 1;
else if chage_22mo<=5 and chwhz=1 then lt_5_whz = 1;
else if chage_30mo<=5 and chwhz=1 then lt_5_whz = 1;
else if chage_42mo<=5 and chwhz=1 then lt_5_whz = 1;
else if chage_48mo<=5 and chwhz=1 then lt_5_whz = 1;
else if chage_84mo<=5 and chwhz=1 then lt_5_whz = 1;
run;
proc freq data=q2_whz_5;
table lt_5_whz over_5_whz/MISSPRINT;
run;
* create first whz;
data q2_whz_first;
set q2_whz;
firstFU=.;
if chwhz_6mo=1 then firstFU=6;
else if chwhz_14mo=1 then firstFU=14;
else if chwhz_22mo=1 then firstFU=22;
else if chwhz_30mo=1 then firstFU=30;
else if chwhz_42mo=1 then firstFU=42;
else if chwhz_48mo=1 then firstFU=48;
else if chwhz_84mo=1 then firstFU=84;
run;
proc means data=q2_whz_first min q1 median q3 max n nmiss;var firstFU;run;

*Q3 transpose;
data q3;
set '/home/u39221714/q3_dat (1).sas7bdat';
run;
proc transpose data=q3 out=q3_wide_1 prefix=id;
    by id ;
    id tube;
    var cortisol;
run;

data q3_wide_1;
set q3_wide_1;
drop _NAME_ _LABEL_;
rename id1-id9=tube1-tube9;
run;

proc transpose data=q3 out=q3_wide_2 prefix=id;
    by id ;
    id tube;
    var military_time;
run;
* rename;
data q3_wide_2;
set q3_wide_2;
drop _NAME_ _LABEL_;
rename id1-id9=mt1-mt9;
run;
* build dataset;
data q3_wide;
merge q3_wide_1 q3_wide_2;
by ID;
run;
* sort columns;
data q3_wide;
   retain ID tube1 tube2 tube3 tube4 tube5 tube6 tube7 tube8 tube9 mt1 mt2 mt3 mt4 mt5 mt6 mt7 mt8 mt9;
   set q3_wide;
run;
*q3_b: calculate minutes from baseline;
data q3_time;
set q3_wide;
time1=0;
time2=(mt2-mt1)/60;
if mt3/60 <= 720 then time3=(mt3-mt1)/60+1440; else time3=(mt3-mt1)/60;
time4=0;
if mt5/60 <= 720 then time5=(mt5-mt4)/60+1440; else time5=(mt5-mt4)/60;
if mt6/60 <= 720 then time6=(mt6-mt4)/60+1440; else time6=(mt6-mt4)/60;
time7=0;
time8=(mt8-mt7)/60;
if mt9/60 <= 720 then time9=(mt6-mt7)/60+1440; else time9=(mt6-mt7)/60;
drop mt1-mt9;
run;
proc print data=q3_time;
*q3_c transpose again;
*subset first - could use a macro to reduce code but I am lazy so I just copy and paste;
data q3_day1;
set q3_time;
day=1;
cortisol1 = tube1;
cortisol2 = tube2;
cortisol3 = tube3;
keep ID day time1 cortisol1 time2 cortisol2 time3 cortisol3   ;
run;
proc print data=q3_day1;

data q3_day2;
set q3_time;
day=2;
cortisol1 = tube4;
cortisol2 = tube5;
cortisol3 = tube6;
time1 = time4;
time2 = time5;
time3 = time6;
keep ID day time1 cortisol1 time2 cortisol2 time3 cortisol3   ;
run;
proc print data=q3_day2;

data q3_day3;
set q3_time;
day=3;
time1 = time7;
cortisol1 = tube7;
time2 = time8;
cortisol2 = tube8;
time3 = time9;
cortisol3 = tube9;
keep ID day time1 cortisol1 time2 cortisol2 time3 cortisol3   ;
run;
proc print data=q3_day3;
* build dataset and reorder columns;
data q3_c;
retain ID day time1 cortisol1 time2 cortisol2 time3 cortisol3;
merge q3_day1 q3_day2 q3_day3;
by day;
run;
*add mean;
data q3_c_mean;
set q3_c;
cortisol_mean = mean(of cortisol1-cortisol3);
run;
*sorted;
proc sort data=q3_c_mean; by ID day;run;
*final dataset output;
proc print data=q3_c_mean;



