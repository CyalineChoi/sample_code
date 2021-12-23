*HHC test code;
*Q1 import dataset;
options msglevel=I;
FILENAME REFFILE '/home/u39221714/sasuser.v94/Test_DataCore.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=core;
	GETNAMES=YES;
RUN;
PROC CONTENTS DATA=core; RUN;
proc print data=core(obs=5);run;
*Q2;
proc sql;
  create table patient_count as
  select  patient_ID,       /* <= added, as we are grouping by */
          count(distinct(patient_ID)) as COUNT
  from    core
  group by patient_ID;   /*  <--- this part here is important! */
quit;
proc print;
   title 'Number of distinct values for patient ID'; 
run;
*Q3;
proc sql;
  create table death_count as
  select  patient_ID,       /* <= added, as we are grouping by */
          count(distinct(date_of_death)) as COUNT
  from    core
  group by patient_ID
  order by COUNT desc;   /*  <--- this part here is important! */
quit;
proc print;
   title 'Number of death'; 
run;
*Q4;
*step 1: import second dataset;
options msglevel=I;
FILENAME REFFILE '/home/u39221714/sasuser.v94/Test_DataCore_VitalStats.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=vital;
	GETNAMES=YES;
RUN;

PROC CONTENTS DATA=vital; RUN;
*step 2: sort and merge by left join;
proc sort data=vital;by patient_ID;run;
Data join;        
Merge core (IN = X) vital (IN=Y);
by patient_ID;
If X ;
run;
proc sql;
  create table death_count as
  select  patient_ID,       /* <= added, as we are grouping by */
          count(distinct(date_of_death)) as COUNT
  from    join
  group by patient_ID
  having COUNT=1
  order by COUNT desc;   /*  <--- this part here is important! */
quit;
proc print;
   title 'Number of death'; 
run;
*Q5 - visit day as the visit activation date;
*convert datetime to date format;
data join;
set join;
VISIT_ACTIVATION_DATE=datepart(VISIT_ACTIVATION_DATE_TIME);
format VISIT_ACTIVATION_DATE date9.;
run;
proc sql;
select  distinct(patient_ID),
        date_of_death - max(VISIT_ACTIVATION_DATE) as length_of_days,
        count(distinct(date_of_death)) as COUNT
from    join 
group by patient_ID
having length_of_days>=1 and COUNT=1;
quit;
*Q6;
proc freq data=join;
table race;
run;
*Q7;
*step 1: generate age;
*format date;
data join_age;
set join;
BIRTH_DATE=datepart(BIRTHDATE);
format BIRTH_DATE date9.;
run;
*subset;
data join_age_death;
set join_age;
where date_of_death > 0;
run;
data join_age_alive;
set join_age;
where date_of_death = .;
run;
*calculate age;
proc sql;
create table alive as
select  distinct(patient_ID),
        date_of_death,
        BIRTH_DATE,
        int(yrdif(BIRTH_DATE,today(),'ACTUAL')) as Age,
        sex
from    join_age_alive;
quit;
proc sql;
create table death as
select  distinct(patient_ID),
        date_of_death,
        BIRTH_DATE,
        int(yrdif(BIRTH_DATE,date_of_death,'ACTUAL')) as Age,
        sex
from    join_age_death;
quit;
*merge;
proc sort data=death;by patient_ID;run;
Data join_new;        
Merge alive (IN = X) death (IN=Y);
by patient_ID;
run;
*distribution;
proc sgpanel data=join_new;
  panelby sex;
  histogram age;
run;proc sgpanel data=join_new;
  panelby sex;
  vbox age;
run;
*Q8;
data join;
set join;
DISCHARGE_DATE=datepart(DISCHARGE_DATE_TIME);
ADMISSION_DATE=datepart(ADMISSION_DATE_TIME);
format DISCHARGE_DATE date9.;
format ADMISSION_DATE date9.;
visit = DISCHARGE_DATE - ADMISSION_DATE;
run;
proc sql;
create table totalVisit as
select  patient_ID,
		ADMISSION_DATE,
		DISCHARGE_DATE,
		sum(Visit) as totalVisit
from    join
group by patient_ID
order by totalVisit desc; 
quit;
*visualization;
proc sgplot data=totalVisit;
  scatter x=patient_ID y=totalVisit;
run;
*without outliers;
data join_out;
set totalvisit;
if totalVisit>1825 then delete;
run;
proc sgplot data=join_out;
  scatter x=patient_ID y=totalVisit;
run;
proc sgplot data=join_out;
  histogram totalVisit;
run;
proc sgplot data=join_out;
  vbox totalVisit;
run;
*Q9;
data join_2015;
set join;
DISCHARGE_DATE=datepart(DISCHARGE_DATE_TIME);
ADMISSION_DATE=datepart(ADMISSION_DATE_TIME);
format DISCHARGE_DATE date9.;
format ADMISSION_DATE date9.;
if year(ADMISSION_DATE) = 2015 then visit = DISCHARGE_DATE - ADMISSION_DATE;
run;
proc print data=totalvisit (obs=10); var patient_id;run;







