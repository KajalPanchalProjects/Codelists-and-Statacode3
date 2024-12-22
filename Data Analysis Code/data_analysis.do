////////////////////////////////////////////////////////////////////////////////
*********************************DATA ANALYSIS**********************************
////////////////////////////////////////////////////////////////////////////////

*load data 
use "...\nonfatalHF_clean2.dta", clear 

*Redefine survival time variables in CPRD GOLD and Aurum with new indexdate (first diagnosis HF code)
*end date - last linkage date of ONS is 29th March 2021. 
drop tt_end 
gen tt_end = date("29/03/2021", "DMY") - date(indexdate2, "DMY")
replace tt_end=tt_end/365

*death (ONS)
drop tt_dod 
gen tt_dod = date(dod, "DMY") - date(indexdate2, "DMY")
replace tt_dod=tt_dod/365
gen dod_out=0 if missing(dod)
replace dod_out=1 if missing(dod_out)

*linkdate (HES)
drop tt_linkdate 
gen tt_linkdate = date(linkdate, "DMY") - date(indexdate2, "DMY")
replace tt_linkdate=tt_linkdate/365

*TOD (CPRD)
drop tt_tod 
gen tt_tod = date(tod, "DMY") - date(indexdate2, "DMY")
replace tt_tod=tt_tod/365

*LCD (CPRD)
drop tt_lcd 
gen tt_lcd = date(lcd, "DMY") - date(indexdate2, "DMY")
replace tt_lcd = tt_lcd/365

*define survival time to death (ONS), with right censoring 
generate tte_dod_out=. 
replace tte_dod_out = date("29/03/2021", "DMY") - date(indexdate2, "DMY") if date(dod, "DMY") > date("29/03/2021", "DMY") & dod_out==1 
replace dod_out=0 if date(dod, "DMY") > date("29/03/2021", "DMY") & dod_out==1 
replace tte_dod_out = date(dod, "DMY") - date(indexdate2, "DMY") if dod_out==1 
replace tte_dod_out = date("29/03/2021", "DMY") - date(indexdate2, "DMY") if dod_out==0 
replace tte_dod_out = tte_dod_out/365

*Checks: This code generates the same result as above code 
*egen tte_dod_out2 = rowmin(tt_dod tt_end tt_linkdate tt_tod tt_lcd) 
*Create dod_out binary variable (censored)
*gen dod_out2=1 if tte_dod_out==tt_dod 
*replace dod_out2=0 if missing(dod_out2) 

//check 
tab hf_out 
tab hfi_out 
tab hfn_out

*Generate age variable 
gen index22 = date(indexdate2, "DMY")
format index22 %td
gen indexyear22 = yofd(index22)
gen age = (index22 - mdy(1, 1, yob)) / 365.25
format age %9.2f

*Generate age variable (check)
gen index2 = date(indexdate2, "DMY")
format index2 %td
gen indexyear2 = yofd(index2) 
gen age2 = indexyear2 - yob 

*Comorbid variables (0,1)
foreach ot in af anaemia asthma cancer cld copd dementia depression oa pvd ra stroke thy htn {	
destring `ot'_BL, replace 
replace `ot'_BL=0 if `ot'_BL==.
}

*Treatments (0,1)
foreach ot in ace met meg sglt2 alphab bb antiplat cc glp1ra other_lipid other_hyper statin nitrates angioIIrb digoxin diuretics insulin sulf tzd {	
destring `ot', replace 
replace `ot'=0 if `ot'==.
}

*Generate treatments categories 
*antiplatelet 
*insulin 
*digoxin 
*oral_glucose
*oral_hyper
*oral_lipid 

gen oral_glucose=1 if glp1ra==1 | meg==1 | met==1 | sglt2==1 | sulf==1 | tzd==1
replace oral_glucose=0 if oral_glucose==.

gen oral_hyper=1 if ace==1 | alphab==1 | angioIIrb==1 | bb==1 | cc==1 | diuretics==1 | other_hyper==1 | nitrates==1 
replace oral_hyper=0 if oral_hyper==. 

gen oral_lipid=1 if other_lipid==1 | statin==1
replace oral_lipid=0 if oral_lipid==.

*Smoking category 
gen smokstatus_cut="ever" if smokstatus=="current or ex-smoker" | smokstatus=="current smoker" | smokstatus=="ex-smoker"
replace smokstatus_cut="non" if smokstatus=="non-smoker"

*Clean data: 
*Obesity (using test file)
*obesity = medcodeid (clinical files)
replace Obesity=0 if Obesity==. 
count if missing(Obesity)

*alcohol
tab alcstatus  
replace alcstatus="non" if alcstatus=="non "
tab alcstatus 

*Clean data CKD (using test file)
*ckd = medcodeid (clinical file)
replace CKD=0 if CKD==.
count if missing(CKD)

//////////////////////////////////////////////////////////////////////////////
**Flowchart: Exclude if someone has HF after the end date, OR dies and HF is later documented  
*drop if prevalent IHD at t2d date of diagnosis 
tab t2d cprd 
tab t2d
drop if ihdb_BL==1 
tab t2d 
tab t2d cprd 

*Flowchart: Exclude if people without diabetes have been prescribed insulin or glucose-lowering drugs 
tab oral_glucose t2d
tab insulin t2d 
*count data (supplement)
count if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="gold")
count if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="aurum")
*drop glucose-lowering drugs in people without diabetes - GOLD 
drop if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="gold")
*drop glucose-lowering drugs in people without diabetes - Aurum 
drop if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="aurum")
*check 
tab oral_glucose t2d 
tab insulin t2d 
*Box3: add numbers to flowchart 
tab t2d cprd  
tab hf_out 

*Box4: add numbers to flowchart, drop if date of death or study end before date of HF diagnosis 
tab t2d cprd 
drop if date(indexdate2, "DMY") > date("29/03/2021", "DMY") 
drop if date(indexdate2, "DMY") == date("29/03/2021", "DMY") 
drop if date(indexdate2, "DMY") > date(dod, "DMY") & dod_out==1 
drop if date(indexdate2, "DMY") == date(dod, "DMY") & dod_out==1 

*checks 
tab t2d hfi_out 
tab t2d hfn_out 
tab t2d cprd 
distinct patid 

*Flowchart: CPRD GOLD: counts and (%) missing at baseline 
*total population: 15,544
tab cprd 
foreach ot in age gender ethnic imd systolic smokstatus BMI alcstatus {
describe `ot'
count if missing(`ot') & cprd=="gold"
gen `ot'_=1 if missing(`ot') & cprd=="gold"
egen `ot'_miss=sum(`ot'_) 
display (`ot'_miss/15544)*100
drop `ot'_ `ot'_miss 
}

*Flowchart: CPRD Aurum: counts and (%) missing at baseline 
*total population: 68,774 
tab cprd 
foreach ot in age gender ethnic imd systolic smokstatus BMI alcstatus {
describe `ot'
count if missing(`ot') & cprd=="aurum"
gen `ot'_=1 if missing(`ot') & cprd=="aurum"
egen `ot'_miss=sum(`ot'_) 
display (`ot'_miss/68774)*100
drop `ot'_ `ot'_miss 
}

*Complete case analysis 
tab t2d cprd 
*CPRD Gold 
drop if missing(age) & cprd=="gold" 
drop if missing(gender) & cprd=="gold" 
drop if missing(BMI) & cprd=="gold" 
drop if missing(systolic) & cprd=="gold"
drop if missing(ethnic) & cprd=="gold"
drop if missing(imd) & cprd=="gold"
drop if missing(smokstatus) & cprd=="gold"
tab t2d cprd 

*CPRD Aurum 
drop if missing(age) & cprd=="aurum" 
drop if missing(gender) & cprd=="aurum" 
drop if missing(BMI) & cprd=="aurum" 
drop if missing(systolic) & cprd=="aurum" 
drop if missing(ethnic) & cprd=="aurum" 
drop if missing(imd) & cprd=="aurum" 
drop if missing(smokstatus) & cprd=="aurum" 
tab t2d cprd 

*Flowchart: Final Cohort (AFTER COMPLETE CASE ANALYSIS)
tab t2d 
tab t2d cprd 

*check numbers 
tab hfi_out dod_out if gender=="female"
tab hfi_out dod_out if gender=="male"
tab hfn_out dod_out if gender=="female"
tab hfn_out dod_out if gender=="male" 
	 
*Save complete case cohort 
save "...\nonfatalHF_cleancc.dta", replace 

////////////////////////////////////////////////////////////////////////////////
*Supplement: Table 1 (missing data %, cholesterol and alcohol)
////////////////////////////////////////////////////////////////////////////////
*load data 
use "...\nonfatalHF_cleancc.dta", clear 
*missing data - table s1 (chol & alcohol)
tab t2d cprd 
tab t2d 
*total population = 73,344
foreach ot in chol alcstatus {
describe `ot'
count if missing(`ot') 
gen `ot'_=1 if missing(`ot') 
egen `ot'_miss=sum(`ot'_) 
display (`ot'_miss/73344)*100
drop `ot'_ `ot'_miss 
}

*people with diabetes = 27,311
foreach ot in chol alcstatus {
describe `ot'
count if missing(`ot') & t2d==1 
gen `ot'_=1 if missing(`ot') & t2d==1 
egen `ot'_miss=sum(`ot'_) 
display (`ot'_miss/27311)*100
drop `ot'_ `ot'_miss 
}

*people without diabetes = 46,033
foreach ot in chol alcstatus {
describe `ot'
count if missing(`ot') & t2d==0 
gen `ot'_=1 if missing(`ot') & t2d==0 
egen `ot'_miss=sum(`ot'_) 
display (`ot'_miss/46033)*100
drop `ot'_ `ot'_miss 
}

***ischaemic HF 
use "...\nonfatalHF_cleancc.dta", clear 
keep if hfi_out==1 
tab t2d 

*missing data - table s1 (chol & alcohol)
tab t2d cprd 
tab t2d 
*total population = 18,296
foreach ot in chol alcstatus {
describe `ot'
count if missing(`ot') 
gen `ot'_=1 if missing(`ot') 
egen `ot'_miss=sum(`ot'_) 
display (`ot'_miss/18296)*100
drop `ot'_ `ot'_miss 
}

*people with diabetes = 7,253
foreach ot in chol alcstatus {
describe `ot'
count if missing(`ot') & t2d==1 
gen `ot'_=1 if missing(`ot') & t2d==1 
egen `ot'_miss=sum(`ot'_) 
display (`ot'_miss/7253)*100
drop `ot'_ `ot'_miss 
}

*people without diabetes = 11,043
foreach ot in chol alcstatus {
describe `ot'
count if missing(`ot') & t2d==0 
gen `ot'_=1 if missing(`ot') & t2d==0 
egen `ot'_miss=sum(`ot'_) 
display (`ot'_miss/11043)*100
drop `ot'_ `ot'_miss 
}

***non-ischaemic HF 
use "...\nonfatalHF_cleancc.dta", clear 
keep if hfn_out==1 
tab t2d 

*missing data - table s1 (chol & alcohol)
tab t2d cprd 
tab t2d 
*total population = 55,048
foreach ot in chol alcstatus {
describe `ot'
count if missing(`ot') 
gen `ot'_=1 if missing(`ot') 
egen `ot'_miss=sum(`ot'_) 
display (`ot'_miss/55048)*100
drop `ot'_ `ot'_miss 
}

*people with diabetes = 20,058
foreach ot in chol alcstatus {
describe `ot'
count if missing(`ot') & t2d==1 
gen `ot'_=1 if missing(`ot') & t2d==1 
egen `ot'_miss=sum(`ot'_) 
display (`ot'_miss/20058)*100
drop `ot'_ `ot'_miss 
}

*people without diabetes = 34990
foreach ot in chol alcstatus {
describe `ot'
count if missing(`ot') & t2d==0 
gen `ot'_=1 if missing(`ot') & t2d==0 
egen `ot'_miss=sum(`ot'_) 
display (`ot'_miss/34990)*100
drop `ot'_ `ot'_miss 
}

////////////////////////////////////////////////////////////////////////////////
*Follow-up (median, IQR)
////////////////////////////////////////////////////////////////////////////////

*****Ischaemic HF total, median IQR follow-up (in-text)
use "...\nonfatalHF_cleancc.dta", clear 
keep if hfi_out==1 
stset tte_dod_out, id(patid) failure(dod_out==1)
gen followup_time = _t 
sum followup_time, detail
total followup_time

*****Non-ischaemic HF total, median IQR follow-up (in-text)
use "...\nonfatalHF_cleancc.dta", clear 
keep if hfn_out==1 
stset tte_dod_out, id(patid) failure(dod_out==1)
gen followup_time = _t 
sum followup_time, detail
total followup_time

*****Total population 
*1.follow-up years in men overall - mean, median, IQR 
use "...\nonfatalHF_cleancc.dta", clear 
foreach cv in dod {
sum tte_`cv'_out, d
}

*2.follow-up years in people with t2d 
use "...\nonfatalHF_cleancc.dta", clear 
keep if t2d==1 
foreach cv in dod {
sum tte_`cv'_out, d
}

*3.follow-up years in people without diabetes  
use "...\nonfatalHF_cleancc.dta", clear 
keep if t2d==0
foreach cv in dod {
sum tte_`cv'_out, d
}

*****Women 
*1.follow-up years in women overall - mean, median, IQR 
use "...\nonfatalHF_cleancc.dta", clear 
keep if gender=="female"
foreach cv in dod {
sum tte_`cv'_out, d
}

*2.follow-up years in women with t2d 
use "...\nonfatalHF_cleancc.dta", clear 
keep if gender=="female"
keep if t2d==1 
foreach cv in dod {
sum tte_`cv'_out, d
}

*3.follow-up years in women without diabetes 
use "...\nonfatalHF_cleancc.dta", clear 
keep if gender=="female"
keep if t2d==0
foreach cv in dod {
sum tte_`cv'_out, d
}

*****Men
*1.follow-up years in men overall - mean, median, IQR 
use "...\nonfatalHF_cleancc.dta", clear 
keep if gender=="male"
foreach cv in dod {
sum tte_`cv'_out, d
}

*2.follow-up years in men with t2d 
use "...\nonfatalHF_cleancc.dta", clear 
keep if gender=="male"
keep if t2d==1 
foreach cv in dod {
sum tte_`cv'_out, d
}

*3.follow-up years in men without diabetes 
use "...\nonfatalHF_cleancc.dta", clear 
keep if gender=="male"
keep if t2d==0
foreach cv in dod {
sum tte_`cv'_out, d
}

////////////////////////////////////////////////////////////////////////////////
*************************Table 1: Baseline characteristics**********************
////////////////////////////////////////////////////////////////////////////////
***Women with ischaemic heart failure 
use "...\nonfatalHF_cleancc.dta", clear 
keep if gender=="female"
keep if hfi_out==1 
*continous variables (mean, standard deviation)
foreach cv in age BMI systolic chol {
destring `cv', replace 
tab t2d, summarize(`cv')
sum `cv', d
}

*categorical variables 
foreach b in gender ethnic imd smokstatus_cut alcstatus anaemia asthma af_BL cancer CKD cld copd dementia depression htn_BL oa ra thy oral_hyper antiplat digoxin oral_lipid {
destring `b', replace 
tab `b' t2d, column 
}

***Men with ischaemic heart failure 
use "...\nonfatalHF_cleancc.dta", clear 
keep if gender=="male"
keep if hfi_out==1 
*continous variables (mean, standard deviation)
foreach cv in age BMI systolic chol {
destring `cv', replace 
tab t2d, summarize(`cv')
sum `cv', d
}

*categorical variables 
foreach b in gender ethnic imd smokstatus_cut alcstatus anaemia asthma af_BL cancer CKD cld copd dementia depression htn_BL oa ra thy oral_hyper antiplat digoxin oral_lipid {
destring `b', replace 
tab `b' t2d, column 
}

***Women with non-ischaemic heart failure 
use "...\nonfatalHF_cleancc.dta", clear
keep if gender=="female" 
keep if hfn_out==1 
*continous variables (mean, standard deviation)
foreach cv in age BMI systolic chol {
destring `cv', replace 
tab t2d, summarize(`cv')
sum `cv', d
}

*categorical variables 
foreach b in gender ethnic imd smokstatus_cut alcstatus anaemia asthma af_BL cancer CKD cld copd dementia depression htn_BL oa ra thy oral_hyper antiplat digoxin oral_lipid {
destring `b', replace 
tab `b' t2d, column 
}

***Men with non-ischaemic heart failure 
use "...\nonfatalHF_cleancc.dta", clear
keep if gender=="male" 
keep if hfn_out==1 
*continous variables (mean, standard deviation)
foreach cv in age BMI systolic chol {
destring `cv', replace 
tab t2d, summarize(`cv')
sum `cv', d
}

*categorical variables 
foreach b in gender ethnic imd smokstatus_cut alcstatus anaemia asthma af_BL cancer CKD cld copd dementia depression htn_BL oa ra thy oral_hyper antiplat digoxin oral_lipid {
destring `b', replace 
tab `b' t2d, column 
}

////////////////////////////////////////////////////////////////////////////////
*******************************Data analysis************************************
////////////////////////////////////////////////////////////////////////////////
*Format dataset 
use "...\nonfatalHF_cleancc.dta", clear
sencode ethnic, replace 
sencode alcstatus, replace 
sencode smokstatus_cut, replace 
destring imd, replace 
save "...\nonfatalHF_cleancc.dta", replace 

*Mean population (at total population)
use "...\nonfatalHF_cleancc.dta", clear 
*continous variables (mean, standard deviation)
foreach cv in age {
destring `cv', replace 
tab t2d, summarize(`cv')
}

***Women with ischaemic heart failure 
use "...\nonfatalHF_cleancc.dta", clear
keep if gender=="female"
keep if hfi_out==1 

//female with ischaemic heart failure - Survival Time Crude IR (95%CI) and IRR (95%CI) 
foreach cv in dod {
*set data 
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*rates 
stptime, title(person-years) by(t2d) per(100)
stir t2d, noshow 
*total population, number of events 
tab t2d `cv'_out 
}

*format population per 100 person years 
gen pop_dod=tte_dod_out/100

//female with ischaemic heart failure - Poisson Crude IR (95%CI) and IRR (95%CI) - robust 
foreach cv in dod {
poisson `cv'_out i.t2d, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) post 
}

//female with ischaemic heart failure - Poisson age-standardised IR (95%CI) and IRR(95%CI) - robust 
foreach cv in dod {
poisson `cv'_out i.t2d age, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) at(age=78) post 
}

//female with ischaemic heart failure - Cox-regression model (knots - 25, 50, 75) 
foreach cv in dod {
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*1.Unadjusted 
stcox i.t2d 
stpm2 i.t2d, scale(hazard) knots(25 50 75) knscale(centile) eform 
*2.Age adjusted
stcox i.t2d age
stpm2 i.t2d age, scale(hazard) knots(25 50 75) knscale(centile) eform 
*3.Adjusted (age, index of multiple deprivation, alcohol, ethnicity, and smoker status)
stcox i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut 
stpm2 i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut, scale(hazard) knots(25 50 75) knscale(centile) eform 
*4.Add comorbidites 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
*5.Full-model: Adding treatments 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
}

***Men with ischaemic heart failure 
use "...\nonfatalHF_cleancc.dta", clear
keep if gender=="male"
keep if hfi_out==1 

//male with ischaemic heart failure - Survival Time Crude IR (95%CI) and IRR (95%CI) 
foreach cv in dod {
*set data 
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*rates 
stptime, title(person-years) by(t2d) per(100)
stir t2d, noshow 
*total population, number of events 
tab t2d `cv'_out 
}

*format population per 100 person years 
foreach cv in dod {
gen pop_`cv'=tte_`cv'_out/100
}

//male with ischaemic heart failure - Poisson Crude IR (95%CI) and IRR (95%CI) - robust 
foreach cv in dod {
poisson `cv'_out i.t2d, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) post 
}

*male with ischaemic heart failure - Age standardised IR (95% CI) and IRR (95% CI) - robust 
foreach cv in dod {
poisson `cv'_out i.t2d age, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) at(age=78) post 
}

//male with ischaemic heart failure - Cox-regression model (knots - 25, 50, 75) 
foreach cv in dod {
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*1.Unadjusted 
stcox i.t2d 
stpm2 i.t2d, scale(hazard) knots(25 50 75) knscale(centile) eform 
*2.Age adjusted
stcox i.t2d age
stpm2 i.t2d age, scale(hazard) knots(25 50 75) knscale(centile) eform 
*3.Adjusted (age, index of multiple deprivation, alcohol, ethnicity, and smoker status)
stcox i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut 
stpm2 i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut, scale(hazard) knots(25 50 75) knscale(centile) eform 
*4.Add comorbidites 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
*5.Full-model: Adding treatments 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
}

***Women with non-ischaemic heart failure 
use "...\nonfatalHF_cleancc.dta", clear
keep if gender=="female"
keep if hfn_out==1 

//female with non-ischaemic heart failure - Survival Time Crude IR (95%CI) and IRR (95%CI) per 100,000 person-years 
foreach cv in dod {
*set data 
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*rates 
stptime, title(person-years) by(t2d) per(100)
stir t2d, noshow 
*total population, number of events 
tab t2d `cv'_out 
}

*format population per 100 person years 
foreach cv in dod {
gen pop_`cv'=tte_`cv'_out/100
}

//female with non-ischaemic heart failure - Poisson Crude IR (95%CI) and IRR (95%CI) - robust 
foreach cv in dod {
poisson `cv'_out i.t2d, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) post 
}

*female with non-ischaemic heart failure - Age standardised IR (95% CI) and IRR (95% CI) - robust 
foreach cv in dod {
poisson `cv'_out i.t2d age, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) at(age=78) post 
}

//female with non-ischaemic heart failure - Cox-regression model (knots - 25, 50, 75) 
foreach cv in dod {
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*1.Unadjusted 
stcox i.t2d 
stpm2 i.t2d, scale(hazard) knots(25 50 75) knscale(centile) eform 
*2.Age adjusted
stcox i.t2d age
stpm2 i.t2d age, scale(hazard) knots(25 50 75) knscale(centile) eform 
*3.Adjusted (age, index of multiple deprivation, alcohol, ethnicity, and smoker status)
stcox i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut 
stpm2 i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut, scale(hazard) knots(25 50 75) knscale(centile) eform 
*4.Add comorbidites 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
*5.Full-model: Adding treatments 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
}

***Men with non-ischaemic heart failure 
use "...\nonfatalHF_cleancc.dta", clear
keep if gender=="male"
keep if hfn_out==1 

//male with non-ischaemic heart failure - Survival Time Crude IR (95%CI) and IRR (95%CI) 
foreach cv in dod {
*set data 
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*rates 
stptime, title(person-years) by(t2d) per(100)
stir t2d, noshow 
*total population, number of events 
tab t2d `cv'_out 
}

*format population per 100 person years 
foreach cv in dod {
gen pop_`cv'=tte_`cv'_out/100
}

//male with non-ischaemic heart failure - Poisson Crude IR (95%CI) and IRR (95%CI) - robust 
foreach cv in dod {
poisson `cv'_out i.t2d, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) post 
}

*male with non-ischaemic heart failure - Age standardised IR (95% CI) and IRR (95% CI) - robust 
foreach cv in dod {
poisson `cv'_out i.t2d age, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) at(age=78) post 
}

//male with non-ischaemic heart failure - Cox-regression model (knots - 25, 50, 75) 
foreach cv in dod {
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*1.Unadjusted 
stcox i.t2d 
stpm2 i.t2d, scale(hazard) knots(25 50 75) knscale(centile) eform 
*2.Age adjusted
stcox i.t2d age
stpm2 i.t2d age, scale(hazard) knots(25 50 75) knscale(centile) eform 
*3.Adjusted (age, index of multiple deprivation, alcohol, ethnicity, and smoker status)
stcox i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut 
stpm2 i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut, scale(hazard) knots(25 50 75) knscale(centile) eform 
*4.Add comorbidites 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
*5.Full-model: Adding treatments 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
}

*log file 
cap log close 
log using "...\data_analysis3final_withprevIHD.log", replace 

/////////////////////////////////////////////////////////////////////////////////
****Sensitivity analysis: with people with prevalent IHD at indexdate of t2d (corresponding date for those without diabetes)

*load data 
use "...\nonfatalHF_clean2.dta", clear 

*Redefine survival time variables in CPRD GOLD and Aurum with new indexdate (first diagnosis HF code)
*end date - last linkage date of ONS is 29th March 2021. 
drop tt_end 
gen tt_end = date("29/03/2021", "DMY") - date(indexdate2, "DMY")
replace tt_end=tt_end/365

*death (ONS)
drop tt_dod 
gen tt_dod = date(dod, "DMY") - date(indexdate2, "DMY")
replace tt_dod=tt_dod/365
gen dod_out=0 if missing(dod)
replace dod_out=1 if missing(dod_out)

*linkdate (HES)
drop tt_linkdate 
gen tt_linkdate = date(linkdate, "DMY") - date(indexdate2, "DMY")
replace tt_linkdate=tt_linkdate/365

*TOD (CPRD)
drop tt_tod 
gen tt_tod = date(tod, "DMY") - date(indexdate2, "DMY")
replace tt_tod=tt_tod/365

*LCD (CPRD)
drop tt_lcd 
gen tt_lcd = date(lcd, "DMY") - date(indexdate2, "DMY")
replace tt_lcd = tt_lcd/365

*define survival time to death (ONS), with right censoring 
generate tte_dod_out=. 
replace tte_dod_out = date("29/03/2021", "DMY") - date(indexdate2, "DMY") if date(dod, "DMY") > date("29/03/2021", "DMY") & dod_out==1 
replace dod_out=0 if date(dod, "DMY") > date("29/03/2021", "DMY") & dod_out==1 
replace tte_dod_out = date(dod, "DMY") - date(indexdate2, "DMY") if dod_out==1 
replace tte_dod_out = date("29/03/2021", "DMY") - date(indexdate2, "DMY") if dod_out==0 
replace tte_dod_out = tte_dod_out/365

*Generate age variable 
gen index22 = date(indexdate2, "DMY")
format index22 %td
gen indexyear22 = yofd(index22)
gen age = (index22 - mdy(1, 1, yob)) / 365.25
format age %9.2f

*Generate age variable (check)
gen index2 = date(indexdate2, "DMY")
format index2 %td
gen indexyear2 = yofd(index2) 
gen age2 = indexyear2 - yob 

*Comorbid variables (0,1)
foreach ot in af anaemia asthma cancer cld copd dementia depression oa pvd ra stroke thy htn {	
destring `ot'_BL, replace 
replace `ot'_BL=0 if `ot'_BL==.
}

*Treatments (0,1)
foreach ot in ace met meg sglt2 alphab bb antiplat cc glp1ra other_lipid other_hyper statin nitrates angioIIrb digoxin diuretics insulin sulf tzd {	
destring `ot', replace 
replace `ot'=0 if `ot'==.
}

*Generate treatments categories 
*antiplatelet 
*insulin 
*digoxin 
*oral_glucose
*oral_hyper
*oral_lipid 

gen oral_glucose=1 if glp1ra==1 | meg==1 | met==1 | sglt2==1 | sulf==1 | tzd==1
replace oral_glucose=0 if oral_glucose==.

gen oral_hyper=1 if ace==1 | alphab==1 | angioIIrb==1 | bb==1 | cc==1 | diuretics==1 | other_hyper==1 | nitrates==1 
replace oral_hyper=0 if oral_hyper==. 

gen oral_lipid=1 if other_lipid==1 | statin==1
replace oral_lipid=0 if oral_lipid==.

*Smoking category 
gen smokstatus_cut="ever" if smokstatus=="current or ex-smoker" | smokstatus=="current smoker" | smokstatus=="ex-smoker"
replace smokstatus_cut="non" if smokstatus=="non-smoker"

*Clean data: 
*Obesity (using test file)
*obesity = medcodeid (clinical files)
replace Obesity=0 if Obesity==. 
count if missing(Obesity)

*alcohol
tab alcstatus  
replace alcstatus="non" if alcstatus=="non "
tab alcstatus 

*Clean data CKD (using test file)
*ckd = medcodeid (clinical file)
replace CKD=0 if CKD==.
count if missing(CKD)
	  
////////////////////////////////////////////////////////////////////////////////
***Survival time (checked - this is correct; already ran with data cleaning code0 
*define ischaemic HF (including prevalent IHD)
*gen tt_hfi_out= tt_hf_out if (tt_hf_out==tt_ihdb_out & hf_out==1 & ihdb_out==1) | (tt_ihdb_out < tt_hf_out & hf_out==1 & ihdb_out==1) | (ihdb_BL==1 & hf_out==1) 
*indicator variable 
*gen hfi_out =1 if (tt_hf_out==tt_ihdb_out & hf_out==1 & ihdb_out==1) | (tt_ihdb_out < tt_hf_out & hf_out==1 & ihdb_out==1) | (ihdb_BL==1 & hf_out==1) 
*replace hfi_out=0 if missing(hfi_out)

*define non-ischaemic HF (including prevalent IHD)
*gen tt_hfn_out= tt_hf_out if (tt_hf_out==tt_ihdb_out & hf_out==1 & ihdb_out==0 & ihdb_BL==0) | (tt_ihdb_out < tt_hf_out & hf_out==1 & ihdb_out==0 & ihdb_BL==0) | (tt_hf_out < tt_ihdb_out & hf_out==1 & ihdb_out==1 & ihdb_BL==0) | (tt_hf_out < tt_ihdb_out & hf_out==1 & ihdb_out==0 & ihdb_BL==0) 
*indicator variable 
*gen hfn_out =1 if (tt_hf_out==tt_ihdb_out & hf_out==1 & ihdb_out==0 & ihdb_BL==0) | (tt_ihdb_out < tt_hf_out & hf_out==1 & ihdb_out==0 & ihdb_BL==0) | (tt_hf_out < tt_ihdb_out & hf_out==1 & ihdb_out==1 & ihdb_BL==0) | (tt_hf_out < tt_ihdb_out & hf_out==1 & ihdb_out==0 & ihdb_BL==0) 
*replace hfn_out=0 if missing(hfn_out)

////////////////////////////////////////////////////////////////////////////////

*Flowchart: Exclude if people without diabtes have been prescribed insulin or glucose-lowering drugs 
tab oral_glucose t2d
tab insulin t2d 
*count data (supplement)
count if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="gold")
count if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="aurum")
*drop glucose-lowering drugs in people without diabetes - GOLD 
drop if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="gold")
*drop glucose-lowering drugs in people without diabetes - Aurum 
drop if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="aurum")
*check 
tab oral_glucose t2d 
tab insulin t2d 
*explore data 
tab t2d cprd  
tab hf_out 

**Flowchart: Exclude if someone has HF after the end date, OR dies and HF is later documented  
tab t2d cprd 
drop if date(indexdate2, "DMY") > date("29/03/2021", "DMY") 
drop if date(indexdate2, "DMY") == date("29/03/2021", "DMY") 
drop if date(indexdate2, "DMY") > date(dod, "DMY") & dod_out==1 
drop if date(indexdate2, "DMY") == date(dod, "DMY") & dod_out==1 
tab t2d cprd 
distinct patid 

*Complete case analysis 
tab t2d cprd 
*CPRD Gold 
drop if missing(age) & cprd=="gold" 
drop if missing(gender) & cprd=="gold" 
drop if missing(BMI) & cprd=="gold" 
drop if missing(systolic) & cprd=="gold"
drop if missing(ethnic) & cprd=="gold"
drop if missing(imd) & cprd=="gold"
drop if missing(smokstatus) & cprd=="gold"
tab t2d cprd 

*CPRD Aurum 
drop if missing(age) & cprd=="aurum" 
drop if missing(gender) & cprd=="aurum" 
drop if missing(BMI) & cprd=="aurum" 
drop if missing(systolic) & cprd=="aurum" 
drop if missing(ethnic) & cprd=="aurum" 
drop if missing(imd) & cprd=="aurum" 
drop if missing(smokstatus) & cprd=="aurum" 
tab t2d cprd 

*Flowchart: Final Cohort (AFTER COMPLETE CASE ANALYSIS)
tab t2d 
tab t2d cprd 

*Format dataset 
sencode ethnic, replace 
sencode alcstatus, replace 
sencode smokstatus_cut, replace 
destring imd, replace 

*check numbers 
tab hfi_out dod_out if gender=="female"
tab hfi_out dod_out if gender=="male"
tab hfn_out dod_out if gender=="female"
tab hfn_out dod_out if gender=="male" 

*Save complete case cohort 
save "...\nonfatalHF_cleancc2.dta", replace 

////////////////////////////////////////////////////////////////////////////////
*******************************Data analysis************************************
////////////////////////////////////////////////////////////////////////////////
*Mean population (at total population == 78)
use "...\nonfatalHF_cleancc2.dta", clear 
*continous variables (mean, standard deviation)
foreach cv in age {
destring `cv', replace 
tab t2d, summarize(`cv')
}

***Women with ischaemic heart failure 
use "...\nonfatalHF_cleancc2.dta", clear
keep if gender=="female"
keep if hfi_out==1 

//female with ischaemic heart failure - Survival Time Crude IR (95%CI) and IRR (95%CI) 
foreach cv in dod {
*set data 
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*rates 
stptime, title(person-years) by(t2d) per(100)
stir t2d, noshow 
*total population, number of events 
tab t2d `cv'_out 
}

*format population per 100 person years 
foreach cv in dod {
gen pop_`cv'=tte_`cv'_out/100
}

//female with ischaemic heart failure - Poisson Crude IR (95%CI) and IRR (95%CI) - robust 
foreach cv in dod {
poisson `cv'_out i.t2d, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) post 
}

//female with ischaemic heart failure - Poisson age-standardised IR (95%CI) and IRR(95%CI) - robust 
foreach cv in dod {
poisson `cv'_out i.t2d age, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) at(age=78) post 
}

//female with ischaemic heart failure - Cox-regression model (knots - 25, 50, 75) 
foreach cv in dod {
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*1.Unadjusted 
stcox i.t2d 
stpm2 i.t2d, scale(hazard) knots(25 50 75) knscale(centile) eform 
*2.Age adjusted
stcox i.t2d age
stpm2 i.t2d age, scale(hazard) knots(25 50 75) knscale(centile) eform 
*3.Adjusted (age, index of multiple deprivation, alcohol, ethnicity, and smoker status)
stcox i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut 
stpm2 i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut, scale(hazard) knots(25 50 75) knscale(centile) eform 
*4.Add comorbidites 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
*5.Full-model: Adding treatments 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
}

***Men with ischaemic heart failure 
use "...\nonfatalHF_cleancc2.dta", clear
keep if gender=="male"
keep if hfi_out==1 

//male with ischaemic heart failure - Survival Time Crude IR (95%CI) and IRR (95%CI) 
foreach cv in dod {
*set data 
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*rates 
stptime, title(person-years) by(t2d) per(100)
stir t2d, noshow 
*total population, number of events 
tab t2d `cv'_out 
}

*format population per 100 person years 
foreach cv in dod {
gen pop_`cv'=tte_`cv'_out/100
}

//male with ischaemic heart failure - Poisson Crude IR (95%CI) and IRR (95%CI) - robust 
foreach cv in dod {
poisson `cv'_out i.t2d, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) post 
}

*male with ischaemic heart failure - Age standardised IR (95% CI) and IRR (95% CI) - robust 
foreach cv in dod {
poisson `cv'_out i.t2d age, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) at(age=78) post 
}

//male with ischaemic heart failure - Cox-regression model (knots - 25, 50, 75) 
foreach cv in dod {
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*1.Unadjusted 
stcox i.t2d 
stpm2 i.t2d, scale(hazard) knots(25 50 75) knscale(centile) eform 
*2.Age adjusted
stcox i.t2d age
stpm2 i.t2d age, scale(hazard) knots(25 50 75) knscale(centile) eform 
*3.Adjusted (age, index of multiple deprivation, alcohol, ethnicity, and smoker status)
stcox i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut 
stpm2 i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut, scale(hazard) knots(25 50 75) knscale(centile) eform 
*4.Add comorbidites 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
*5.Full-model: Adding treatments 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
}

***Women with non-ischaemic heart failure 
use "...\nonfatalHF_cleancc2.dta", clear
keep if gender=="female"
keep if hfn_out==1 

//female with non-ischaemic heart failure - Survival Time Crude IR (95%CI) and IRR (95%CI) per 100,000 person-years 
foreach cv in dod {
*set data 
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*rates 
stptime, title(person-years) by(t2d) per(100)
stir t2d, noshow 
*total population, number of events 
tab t2d `cv'_out 
}

*format population per 100 person years 
foreach cv in dod {
gen pop_`cv'=tte_`cv'_out/100
}

//female with non-ischaemic heart failure - Poisson Crude IR (95%CI) and IRR (95%CI) - robust 
foreach cv in dod {
poisson `cv'_out i.t2d, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) post 
}

*female with non-ischaemic heart failure - Age standardised IR (95% CI) and IRR (95% CI) - robust 
foreach cv in dod {
poisson `cv'_out i.t2d age, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) at(age=78) post 
}

//female with non-ischaemic heart failure - Cox-regression model (knots - 25, 50, 75) 
foreach cv in dod {
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*1.Unadjusted 
stcox i.t2d 
stpm2 i.t2d, scale(hazard) knots(25 50 75) knscale(centile) eform 
*2.Age adjusted
stcox i.t2d age
stpm2 i.t2d age, scale(hazard) knots(25 50 75) knscale(centile) eform 
*3.Adjusted (age, index of multiple deprivation, alcohol, ethnicity, and smoker status)
stcox i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut 
stpm2 i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut, scale(hazard) knots(25 50 75) knscale(centile) eform 
*4.Add comorbidites 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
*5.Full-model: Adding treatments 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
}

***Men with non-ischaemic heart failure 
use "...\nonfatalHF_cleancc2.dta", clear
keep if gender=="male"
keep if hfn_out==1 

//male with non-ischaemic heart failure - Survival Time Crude IR (95%CI) and IRR (95%CI) 
foreach cv in dod {
*set data 
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*rates 
stptime, title(person-years) by(t2d) per(100)
stir t2d, noshow 
*total population, number of events 
tab t2d `cv'_out 
}

*format population per 100 person years 
foreach cv in dod {
gen pop_`cv'=tte_`cv'_out/100
}

//male with non-ischaemic heart failure - Poisson Crude IR (95%CI) and IRR (95%CI) - robust 
foreach cv in dod {
poisson `cv'_out i.t2d, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) post 
}

*male with non-ischaemic heart failure - Age standardised IR (95% CI) and IRR (95% CI) - robust 
foreach cv in dod {
poisson `cv'_out i.t2d age, exposure(pop_`cv') irr vce(robust)
margins i.t2d, predict(ir) at(age=78) post 
}

//male with non-ischaemic heart failure - Cox-regression model (knots - 25, 50, 75) 
foreach cv in dod {
stset tte_`cv'_out, id(patid) failure(`cv'_out==1)
*1.Unadjusted 
stcox i.t2d 
stpm2 i.t2d, scale(hazard) knots(25 50 75) knscale(centile) eform 
*2.Age adjusted
stcox i.t2d age
stpm2 i.t2d age, scale(hazard) knots(25 50 75) knscale(centile) eform 
*3.Adjusted (age, index of multiple deprivation, alcohol, ethnicity, and smoker status)
stcox i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut 
stpm2 i.t2d age imd i.ethnic ib2.alcstatus ib2.smokstatus_cut, scale(hazard) knots(25 50 75) knscale(centile) eform 
*4.Add comorbidites 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
*5.Full-model: Adding treatments 
stcox i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL
stpm2 i.t2d age BMI imd chol systolic i.CKD i.ethnic ib2.alcstatus ib2.smokstatus_cut i.asthma i.anaemia i.dementia i.cancer i.cld i.ra i.oa i.copd i.depression i.thy i.antiplat i.digoxin i.oral_hyper i.oral_lipid i.htn_BL i.af_BL, scale(hazard) knots(25 50 75) knscale(centile) eform 
}

//log close 
log close 











