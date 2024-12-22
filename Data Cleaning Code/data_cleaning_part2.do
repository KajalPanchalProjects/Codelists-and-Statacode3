////////////////////////////////////////////////////////////////////////////////
**************************Data cleaning Non-fatal HF****************************
////////////////////////////////////////////////////////////////////////////////
*load dataset from 1st CPRD paper 
use "...\gold_cohort1.dta", clear 
append using "...\aurum_cohort1.dta", force

*inclusion criterion check: drop if <18 age 
gen index = date(indexdate, "DMY")
format index %td
gen indexyear = yofd(index)
destring yob, replace 
gen age = indexyear - yob 
drop if age<18 

*inclusion criterion check: keep if gender is female or male only 
keep if gender=="female" | gender=="male" 
tab gender 

*count population 
tab t2d

*prepare data: convert days to years
replace tt_dod=tt_dod/365
replace tt_end=tt_end/365 
replace tt_linkdate=tt_linkdate/365
replace tt_lcd=tt_lcd/365
replace tt_tod=tt_tod/365 
*event date in ONS, HES or CPRD 
foreach ot in hf ihdb {
replace tt_`ot'_dod=tt_`ot'_dod/365 
replace tt_`ot'_outh=tt_`ot'_outh/365
replace tt_`ot'_outc=tt_`ot'_outc/365
}

*count total participants 
tab t2d cprd 
tab t1d cprd 

**sensitivity: If a patient has code for t2d and t1d, then replace t1d=0 and t2d=1 
replace t2d=1 if t2d==1 & t1d==1

*keep t2d cohort only i.e. drop if t2d == missing 
drop if t2d==. 
*count total population 
tab t2d cprd 

*First diagnosis code for HF in either HES or CPRD (0,1)
foreach ot in hf ihdb {
egen tt_`ot'_out = rowmin(tt_`ot'_outh tt_`ot'_outc) 
gen `ot'_out=0 if missing(tt_`ot'_out) 
replace `ot'_out=1 if missing(`ot'_out) 
}

*check
tab t2d cprd 
tab hf_out cprd 
tab t2d hf_out 

*Flowchart: Keep people with a first diagnosis code for HF in CPRD OR HES only 
keep if hf_out==1 
tab t2d cprd 
tab hf_out cprd 
tab t2d hf_out 

*Flowchart: Exclude if people with type 2 diabetes (or matched controls) have HF diagnosis at study entry prior to or at index date (this keeps people with incident HF in the study only)
drop if hf_BL==1
tab t2d cprd  
tab t2d hf_out 

*Flowchart: Exclude CVD (pvd baseline and stroke baseline only) in people with type 2 diabetes (or matched controls)
*This keeps people with prevalent ischaemic heart disease [coronary heart disease, angina, myocardial infarction] at baseline
egen excl_CVD= rowtotal (pvd_BL stroke_BL) 
tab excl_CVD 
*CPRD Gold 
count if excl_CVD==0 & cprd=="gold" 
*CPRD Aurum 
count if excl_CVD==0 & cprd=="aurum" 
*keep if prevalent CVD=0 
keep if excl_CVD==0 
*explore data 
tab t2d cprd  
tab hf_out 

*Explore people without diabtes have been prescribed insulin or glucose-lowering drugs 
*later drop people without diabetes and insulin or glucose-lowering drugs at diagnosis of incident HF date or prior to 
tab oral_glucose t2d
tab insulin t2d 
*count data (supplement)
count if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="gold")
count if (oral_glucose==1 & t2d==0) | (insulin==1 & t2d==0) & (cprd=="aurum")
*check 
tab oral_glucose t2d 
tab insulin t2d 
*explore data 
tab t2d cprd  
tab hf_out 

*Define ischaemic and non-ischaemic HF (ischaemic HF if someone has IHD prior to type 2 diabetes diagnosis date (corresponding to those without diabetes))
*1.define ischaemic-HF time to event variable 
gen tt_hfi_out= tt_hf_out if (tt_hf_out==tt_ihdb_out & hf_out==1 & ihdb_out==1) | (tt_ihdb_out < tt_hf_out & hf_out==1 & ihdb_out==1) | (ihdb_BL==1 & hf_out==1) 
*indicator variable 
gen hfi_out =1 if (tt_hf_out==tt_ihdb_out & hf_out==1 & ihdb_out==1) | (tt_ihdb_out < tt_hf_out & hf_out==1 & ihdb_out==1) | (ihdb_BL==1 & hf_out==1) 
replace hfi_out=0 if missing(hfi_out)

*2.define non-ischaemic HF time to event variable 
*defined by developing HF (without prior ischaemic heart disease)
gen tt_hfn_out= tt_hf_out if (tt_hf_out==tt_ihdb_out & hf_out==1 & ihdb_out==0 & ihdb_BL==0) | (tt_ihdb_out < tt_hf_out & hf_out==1 & ihdb_out==0 & ihdb_BL==0) | (tt_hf_out < tt_ihdb_out & hf_out==1 & ihdb_out==1 & ihdb_BL==0) | (tt_hf_out < tt_ihdb_out & hf_out==1 & ihdb_out==0 & ihdb_BL==0)
*indicator variable 
gen hfn_out =1 if (tt_hf_out==tt_ihdb_out & hf_out==1 & ihdb_out==0 & ihdb_BL==0) | (tt_ihdb_out < tt_hf_out & hf_out==1 & ihdb_out==0 & ihdb_BL==0) | (tt_hf_out < tt_ihdb_out & hf_out==1 & ihdb_out==1 & ihdb_BL==0) | (tt_hf_out < tt_ihdb_out & hf_out==1 & ihdb_out==0 & ihdb_BL==0)
replace hfn_out=0 if missing(hfn_out)

*check 
tab hf_out 
tab hfi_out 
tab hfn_out 

*generate new indexdate2 - when someone develops incident heart failure 
*indexdate2 = "DMY", string format 
gen indexdate2 = event_hf_outc if event_hf_outc < event_hf_outh
replace indexdate2 = event_hf_outh if event_hf_outh < event_hf_outc 
replace indexdate2 = event_hf_outc if event_hf_outc == event_hf_outh 
replace indexdate2 = event_hf_outc if missing(event_hf_outh)
replace indexdate2 = event_hf_outh if missing(event_hf_outc)
*drop if they did not develop incident heart failure (HES OR CPRD) - should be 0 
drop if missing(indexdate2)
*explore data 
tab t2d cprd  
tab hf_out  

*drop baseline cohort from previous analysis; prepare for redefining baseline variables 
foreach baseline in age eGFR chol systolic smokstatus alcstatus asthma anaemia dementia cancer CKD cld copd depression ra Obesity oa thy ace angioIIrb alphab antiplat bb cc diuretics glp1ra meg met sglt2 sulf tzd insulin oral_lipid oral_hyper oral_glucose statin digoxin nitrates htn_BL af_BL other_lipid other_hyper stroke_BL pvd_BL BMI hf_BL BMI_rnd BMI_cat BMI_cat4 smokstatus_cut {
drop `baseline'
}

*check population same 
distinct patid 
tab t2d cprd 
tab hf_out 

*count data 
distinct patid 
tab t2d cprd 
tab hf_out 

*save new dataset 
save "...\non_fatalHF2.dta", replace  

*************************DATA CLEANING - CPRD AURUM*****************************
*BL characteristics 
*Comorbid conditons (BL)
clear 
foreach ot in af anaemia asthma cancer cld copd dementia depression oa pvd ra stroke thy htn {
cd "..."
use aurum_hes_diagnosis_hosp_21_000355_DM_baseline, clear 
cd "...\HES\"
merge m:1 icd using `ot'_hes, force keep(3) nogenerate 
cd "...\append\"
append using `ot', force 
rename obsdate eventdate 
drop if missing(eventdate)
generate str `ot'_BL="1"
keep patid eventdate `ot'_BL
sort patid eventdate 
merge m:1 patid using "...\non_fatalHF2.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate2, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
keep `ot'_BL patid 
merge 1:1 patid using "...\non_fatalHF2.dta", force
drop if _merge==1
drop _merge
save "...\non_fatalHF2.dta", replace
}

*Smoking (BL) (CPRD only)
clear 
foreach ot in smoking {
cd "...\append\"
use `ot', clear
rename obsdate eventdate 
drop if missing(eventdate)
keep patid smokstatus eventdate 
sort patid eventdate  
merge m:1 patid using "...\non_fatalHF2.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate2, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
keep patid smokstatus  
merge 1:1 patid using "...\non_fatalHF2.dta", force
drop if _merge==1
drop _merge
save "...\non_fatalHF2.dta", replace
}

*Alcohol (BL) (CPRD only)
clear 
foreach ot in alcohol {
cd "...\append\"
use `ot', clear 
rename obsdate eventdate 
drop if missing(eventdate)
keep patid alcstatus eventdate
sort patid eventdate  
merge m:1 patid using "...\non_fatalHF2.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate2, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
keep patid alcstatus 
merge 1:1 patid using "...\non_fatalHF2.dta", force
drop if _merge==1
drop _merge
save "...\non_fatalHF2.dta", replace
}

********************************************************************************
*Treatments 
foreach dm in alphab ace angioIIrb antiplat bb cc diuretics glp1ra insulin meg met statin sglt2 sulf tzd other_hyper other_lipid digoxin nitrates {
clear 
cd "...\append\"
use `dm', clear 
rename issuedate eventdate
drop if missing(eventdate)
merge m:1 patid using "...\non_fatalHF2.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate2, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
keep patid `dm'
merge 1:1 patid using "...\non_fatalHF2.dta", force
drop if _merge==1
drop _merge
save "...\non_fatalHF2.dta", replace
}

********************************************************************************
*Add test files 
********************************************************************************
*add lipids
import delimited "...\NumUnit.txt", stringcols(_all) clear 
merge 1:m numunitid using "...\append\lipids.dta", force
drop if _merge==1 
drop _merge 
tab description 

*keep valid units only mm/L 
keep if description=="mmol/L" | description=="mmol /L" | description=="mm/l"

rename value chol 
destring chol, replace 
sum chol, d 
drop if chol>20 | chol<2
sum chol, d 
rename obsdate eventdate

*merge with main 
keep patid eventdate chol 
drop if missing(eventdate) 
sort patid eventdate 
merge m:1 patid using "...\non_fatalHF2.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate2, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
drop diff eventdate
merge 1:1 patid using "...\non_fatalHF2.dta", force
drop if _merge==1
drop _merge
save "...\non_fatalHF2.dta", replace

********************************************************************************
*add systolic blood pressure 
import delimited "...\NumUnit.txt", stringcols(_all) clear 
merge 1:m numunitid using "...\append\systolic.dta", force
drop if _merge==1 
drop _merge 

*keep valid units only (mmHg)
keep if description=="mmHg" | description=="mm Hg" | description=="mm[Hg]" | description=="Systolic" | description=="mm/Hg" 

drop systolic 
rename obsdate eventdate 
tab description 
rename value systolic 
destring systolic, replace 
sum systolic, d 
drop if systolic >300 | systolic<40
sum systolic, d 

*merge with main 
keep patid eventdate systolic  
drop if missing(eventdate)
sort patid eventdate 
merge m:1 patid using "...\non_fatalHF2.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate2, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
drop diff eventdate
merge 1:1 patid using "...\non_fatalHF2.dta", force
drop if _merge==1
drop _merge
save "...\non_fatalHF2.dta", replace 

********************************************************************************
*BMI calculation
*BMI = weight(kg)/height(m)2
*BMI = 703*weight(Ibs)/(height(inch))2

//weight
import delimited "...\NumUnit.txt", stringcols(_all) clear 
merge 1:m numunitid using "...\append\weight.dta", force
drop if _merge==1 
drop _merge 

tab description 
*"O/E - weight NOS" = value 0 = exclude 
*only keep if kg (considering there are ~30,000,000 results for kg and ~30 cases of stones/mg, it is justified to use kg only)
keep if description=="Kgs" | description=="kg" | description=="kilograms" | description=="kg." | description=="Kilos" | description=="/kg(body wt)" | description=="Weight in Kg" | description=="WEIGHT IN KILOS" | description=="Baseline Kg" 
rename value kg 
destring kg, replace 

*****merge with main (weight_BL_patid) 
keep patid obsdate kg
merge m:1 patid using "...\non_fatalHF2.dta", force keep(3) nogenerate
rename obsdate eventdate 
gen diff = date(indexdate2, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
save "...\append\weight_BL_patidm2", replace 

//height
import delimited "...\NumUnit.txt", stringcols(_all) clear 
merge 1:m numunitid using "...\append\height.dta", force
drop if _merge==1 
drop _merge 
drop height 

tab description 
keep if description=="cm" | description=="cms" | description=="m" | description=="metres"
*the rest are only 1-8 values or irrelevant units i.e. kg, which is irrelevant for height 
destring value, replace 
gen height = value/100 if description=="cm" | description=="cms"
sort height 
replace height=value if height==. & description=="m" 
replace height=value if height==. & description=="metres"
drop if height<1.38 | height>1.88 
gen height2 = height*height 

*****merge with main (height_BL_patid) 
keep patid obsdate height2  
merge m:1 patid using "...\non_fatalHF2.dta", force keep(3) nogenerate
rename obsdate eventdate 
gen diff = date(indexdate2, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
save "...\height_BL_patidm2.dta", replace 

********append weight/height********* 
use "...\append\weight_BL_patidm2", clear 
merge 1:1 patid using "...\append\height_BL_patidm2.dta", force keep(3) nogenerate
gen BMI=(kg/height2)
keep patid eventdate BMI 
save "...\append\BMI_patidm2.dta", replace 

//BMI 
import delimited "...\NumUnit.txt", stringcols(_all) clear 
merge 1:m numunitid using "...\append\BMI.dta", force
drop if _merge==1 
drop _merge 
tab description 
drop BMI 

*keep valid units [kg/m2]
keep if description=="kg/m2" | description=="kg/m^2" | description=="kg/m*m" | description=="Kg/m?" | description=="22.5 24.9" | description=="29 kg/m2" | description=="kg/m*2" | description=="kg/square m" | description=="kg/sq.m." | description=="kg/sq.m" | description=="KG/MM" | strpos(description,"Body mass index") | strpos(description,"Kg/mÃÂ²") | strpos(description,"BMI") | strpos(description,"18.5-24.9") | strpos(description,"18.5.-24.9") 

rename value BMI
destring BMI, replace 
rename obsdate eventdate 
keep patid eventdate BMI

********append BMI with calculated weight/height********* 
append using "...\append\BMI_patidm2.dta", force 
sort patid eventdate 
drop if BMI==. 
merge m:1 patid using "...\non_fatalHF2.dta", force
drop if _merge==1 
drop _merge 
drop if BMI<10.1 | BMI>80 
gen BMI_rnd=round(BMI)
gen diff = date(indexdate2, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1

keep patid BMI
merge 1:1 patid using "...\non_fatalHF2.dta", force 
drop if _merge==1 
drop _merge 

save "...\non_fatalHF2.dta", replace

********************************************************************************
*.eGFR calculation 

*5.1 serum creatinine - calculate eGFR*
import delimited "...\NumUnit.txt", stringcols(_all) clear 
merge 1:m numunitid using "...\append\serum.dta", force 
drop if _merge==1 
drop _merge 
tab description 

rename value serum_creatine 
destring serum_creatine, replace 

*Average creatinine levels are 0.7–1.2 milligrams per deciliter (mg/dL) for males and 0.5–1.0 for females. 
*Average creatinine levels: Male Adult: 59 - 104 μmol/L, Female Adult: 45 - 84 µmol/L

*umol/L = 32,382,624 observations 
gen CR_mgdL=serum_creatine/88.4 if description=="ÃÂµmol/L" | description=="umol/L" | description=="umol/Le" | description=="umol//l" | description=="Micromols/L" | description=="micmol/l" | description=="micromol/L" 

sum CR_mgdL, d 

rename obsdate eventdate 
keep patid CR_mgdL eventdate 

merge m:1 patid using "...\non_fatalHF2.dta", force 
drop if _merge==1
drop _merge

gen diff = date(indexdate2, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1

*generate age variable
destring yob, replace 
*date(indexdate2, "DMY") = date of diagnosis (with matched non-diabetes), added to the time to follow-up with incident heart failure date 
gen index2 = date(indexdate2, "DMY")
format index2 %td
gen indexyear2 = yofd(index2) 
gen age = indexyear2 - yob 

gen cr_k = CR_mgdL/0.7 if gender=="F"
replace cr_k = CR_mgdL/0.9 if gender=="M"

gen min =  cr_k if  cr_k<1
replace min=1 if cr_k >=1

gen max =  cr_k if  cr_k>1
replace max=1 if cr_k <=1

*women 
gen eGFR= 141 * (min)^-0.329 * (max)^-1.209 * (0.993)^age * 1.018 if gender =="F"
replace eGFR = 141 * (min)^-0.329 * (max)^-1.209 * (0.993)^age * 1.018 * 1.159 if gender =="F" & ethnic=="Black"

*men
replace eGFR = 141 * (min)^-0.411 * (max)^-1.209 * (0.993)^age  if gender =="M" 
replace eGFR = 141 * (min)^-0.411 * (max)^-1.209 * (0.993)^age * 1.159 if gender =="M" & ethnic=="Black"

sum  eGFR,d
drop if  eGFR>500 | eGFR==.
keep patid eGFR eventdate 
save "...\append\serum_patidm2.dta", replace 

*5.2 eGFR - department codes*
import delimited "...\NumUnit.txt", stringcols(_all) clear 
merge 1:m numunitid using "...\append\eGFR.dta", force 
drop if _merge==1 
drop _merge 
tab description 
drop eGFR 
rename value eGFR 
destring eGFR, replace 
rename obsdate eventdate 

*keep if valid units only: captures (ml/min/1.73m*2)
keep if strpos(description,"mL/min/1.7")

drop if  eGFR>500 | eGFR==.
sum eGFR, d
keep patid eGFR eventdate 
append using "...\append\serum_patidm2.dta", force 
sort patid eventdate 
merge m:1 patid using "...\non_fatalHF2.dta", force 
drop if _merge==1
drop _merge
gen diff = date(indexdate2, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1

keep patid eGFR
merge 1:1 patid using "...\non_fatalHF2.dta", force  
drop if _merge==1
drop _merge
gen CKD=1 if eGFR<60
replace CKD=0 if CKD >=601
replace CKD=. if eGFR==.
save "...\non_fatalHF2.dta", replace 

**********************CPRD GOLD DATA CLEANING***********************************

*HES baseline - comorbid conditions  
clear
foreach ot in af anaemia asthma cancer cld copd dementia depression oa pvd ra stroke thy htn {
cd "...\HES\"
use `ot'_hes, clear 
keep icd `ot'_hes
cd "..."
merge 1:m icd using hes_diagnosis_hosp_21_000355_DM_STATA_baseline, force keep(3) nogenerate 
keep patid `ot'_hes eventdate 
cd "\HES\merge2\"
save `ot'_hes, replace 
}

********************************************************************************
*2. Add baseline characteristics 

//Smoking (CPRD only)
clear 
foreach ot in smoking {
use "...\append_clinical_ref.dta", clear
cd "...\medcode\"
merge m:1 medcode using `ot', force keep(3) nogenerate  
generate str `ot'_BL="1"
drop if eventdate==""
sort patid 
keep patid eventdate `ot'_BL smokstatus 
merge m:1 patid using "...\non_fatalHF2.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate2, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
keep patid smokstatus 
merge 1:1 patid using "...\non_fatalHF2.dta", force
drop if _merge==1
drop _merge
save "...\non_fatalHF2.dta", replace
}

//Alcohol (CPRD only)
clear 
foreach ot in alcohol {
use "...\append_clinical_ref.dta", clear
cd "...\medcode\"
merge m:1 medcode using `ot', force keep(3) nogenerate  
generate str `ot'_BL="1"
drop if eventdate==""
sort patid 
keep patid eventdate `ot'_BL alcstatus 
merge m:1 patid using "...\non_fatalHF2.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate2, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
keep patid alcstatus 
merge 1:1 patid using "...\non_fatalHF2.dta", force
drop if _merge==1
drop _merge
save "...\non_fatalHF2.dta", replace
}

//BL comorbid conditions 
clear 
foreach ot in af anaemia asthma cancer cld copd dementia depression oa pvd ra stroke thy htn {
cd "...\medcode\"
use `ot', clear 
merge 1:m medcode using "...\append_clinical_ref.dta", force keep(3) nogenerate
cd "...\HES\merge2\"
append using `ot'_hes, force 
drop if eventdate==""
generate str `ot'_BL="1"
keep patid eventdate `ot'_BL
sort patid 
merge m:1 patid using "...\non_fatalHF2.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate2, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
keep `ot'_BL patid 
merge 1:1 patid using "...\non_fatalHF2.dta", force
drop if _merge==1
drop _merge
save "...\non_fatalHF2.dta", replace
}

********************************************************************************
//3. Treatments 
clear
foreach tm in ace angioIIrb alphab antiplat bb cc diuretics glp1ra meg met other_hyper other_lipid sglt2 sulf tzd insulin statin digoxin nitrates {
cd "...\prodcodes\"
use gold_`tm', clear 
keep prodcode `tm' 
cd "..."
merge 1:m prodcode using append_therapy, force keep(3) nogenerate 
drop if eventdate==""
merge m:1 patid using "...\non_fatalHF2.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate2, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
keep patid `tm'
merge 1:1 patid using "...\non_fatalHF2.dta", force
drop if _merge==1
drop _merge
save "...\non_fatalHF2.dta", replace
}

********************************************************************************
*Merging HES/ONS linkage files 

********************************************************************************
//Bringing in tests 

*BMI 
*Cholesterol
*Systolic
*HbA1c 
*eGFR 

*[Referral files DO NOT have adid/enttype - use ONLY CLINICAL FILES] 

*BMI 

***HEIGHT (code from 1st CPRD paper, already run)
*use "...\append_clinical.dta", clear 
*enttype 14 (height)
*keep if enttype=="14"
*sort patid eventdate adid
*merge m:1 patid enttype adid using "...\append_add1.dta", force keep(3) nogenerate 
*destring data1, gen(height_m) 
*sort patid eventdate 
*drop if eventdate==""
*drop medcode data1 data2 data3 enttype adid

*drop implausible values and save 
*drop if height_m<1.38 | height_m>1.88 
*save "...\height_BL", replace 

**WEIGHT (code from 1st CPRD paper, already run)
*use "...\append_clinical.dta", clear 
*enttype 13 (weight)
*keep if enttype=="13" 
*sort patid enttype adid
*merge m:1 patid enttype adid using "...\append_add1.dta", force keep(3) nogenerate 
*destring data1, gen(weight_kg)
*destring data3, gen(BMI)
*sum BMI, d
*sum weight_kg, d
*drop if eventdate==""
*drop medcode data1 data2 data3 enttype adid
*save "...\weight_BL.dta", replace 

// merge weight and BMI with patid 
use "...\weight_BL.dta", clear 
keep patid eventdate weight_kg 
merge m:1 patid using "...\non_fatalHF2.dta", force keep(3) nogenerate 
drop if missing(eventdate)
gen diff = date(indexdate2, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
keep patid eventdate weight_kg BMI 
save "...\weight_BL_patidm2.dta", replace 

// merge height with patid 
use "...\height_BL.dta", clear 
keep patid eventdate height_m 
merge m:1 patid using "...\non_fatalHF2.dta", force keep(3) nogenerate 
drop if missing(eventdate)
gen diff = date(indexdate2, "DMY") - date(eventdate, "DMY")
drop if diff<0 
sort patid diff
by patid: keep if _n == 1
keep patid eventdate height_m 
save "...\height_BL_patidm2.dta", replace 

// merge height and weight together 
use "...\weight_BL_patidm2.dta", clear 
merge 1:1 patid using "...\height_BL_patidm2.dta", force keep(3) nogenerate 
gen height2= height_m*height_m
replace BMI=(weight_kg/height2) if BMI==. 
drop if BMI<10.1 |  BMI>80 
drop if eventdate==""
keep patid eventdate BMI 
save "...\BMIm2.dta", replace 

*merge with main 
use "...\BMIm2.dta", clear
merge 1:1 patid using "...\non_fatalHF2.dta", force 
drop if _merge==1 
drop _merge 

gen BMI_cat= BMI
recode BMI_cat 0/18.49999=1 18.5/24.9999=2 25/29.9999=3 30/34.99999=4 35/39.9999=5 40/81=6
gen BMI_cat4= BMI
recode BMI_cat4 0/18.4999=1 18.5/29.9999=2 30/81=3 
gen Obesity=1 if  BMI_cat4==3
replace  Obesity=0 if  (Obesity==. &  BMI<.)
save "...\non_fatalHF2.dta", replace

********************************************************************************
*Cholesterol 
use "...\append_test.dta", clear
*enttype 163 (serum cholesterol)
keep if enttype=="163"
*SUM unit 96 (mmol/L) 
keep if data3=="96"
rename data2 chol
destring chol, replace 
drop if chol>20 | chol<2
drop if eventdate==""
drop medcode enttype data1 data3

****1:1 with main 
merge m:1 patid using "...\non_fatalHF2.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate2, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
drop diff eventdate  
merge 1:1 patid using "...\non_fatalHF2.dta", force 
drop if _merge==1
drop _merge 
save "...\non_fatalHF2.dta", replace

*************************
*Systolic
use "...\append_clinical.dta", clear 
*enttype = 1 (blood pressure)
keep if enttype=="1" 
merge m:1 patid enttype adid using "...\append_add1.dta", force keep(3) nogenerate 
drop if eventdate==""
destring data2, gen(systolic)
drop data1 data2 data3 enttype adid medcode 
sum systolic, d
*apply inclusion criterion 
drop if systolic >300 | systolic<40
sum systolic, d

keep patid eventdate systolic 
****1:1 with main 
merge m:1 patid using "...\non_fatalHF2.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate2, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
drop diff eventdate 
merge 1:1 patid using "...\non_fatalHF2.dta", force 
drop if _merge==1
drop _merge 
save "...\non_fatalHF2.dta", replace

****eGFR  
*1. Add eGFR from serum creatinine 
use "...\append_test.dta", clear
*enttype 165 (serum creatinine)
keep if enttype=="165" 
drop if eventdate==""
tab data3
*142=umol/L (1mg/dL = 88.4 umol/L) (12,395,986 observations)
keep if data3=="142" 
destring data2, replace 
gen CR_mgdL= data2/88.4 
*explore data
count if CR_mgdL==.
sum CR_mgdL, d 

**********Implement eGFR Equation 
keep patid CR_mgdL eventdate 
*load gold_cohort1 = includes ethnic data 
merge m:1 patid using "...\non_fatalHF2.dta", force 
drop if _merge==1
drop _merge
gen diff = date(indexdate2, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1

*generate age variable
destring yob, replace 
*date(indexdate2, "DMY") = date of diagnosis (with matched non-diabetes), added to the time to follow-up with incident heart failure date 
gen index2 = date(indexdate2, "DMY")
format index2 %td
gen indexyear2 = yofd(index2)
gen age = indexyear2 - yob 

gen cr_k = CR_mgdL/0.7 if gender=="2"
replace cr_k = CR_mgdL/0.9 if gender=="1"

gen min =  cr_k if  cr_k<1
replace min=1 if cr_k >=1

gen max =  cr_k if  cr_k>1
replace max=1 if cr_k <=1

*gender[2] = female in CPRD 
*women 
replace eGFR= 141 * (min)^-0.329 * (max)^-1.209 * (0.993)^age * 1.018 if gender =="2"
replace eGFR = 141 * (min)^-0.329 * (max)^-1.209 * (0.993)^age * 1.018 * 1.159 if gender =="2" & ethnic=="Black"

*gender[1] = male in CPRD 
*men
replace eGFR = 141 * (min)^-0.411 * (max)^-1.209 * (0.993)^age  if gender =="1" 
replace eGFR = 141 * (min)^-0.411 * (max)^-1.209 * (0.993)^age * 1.159 if gender =="1" & ethnic=="Black"

sum  eGFR,d
drop if  eGFR>500 |  eGFR==.

keep patid eventdate eGFR 
save "...\eGFR_serumm2.dta", replace 

*2. Add eGFR from test data 
use "...\append_test.dta", clear 
*enttype 466 (Glomerular Filtration Rate)
keep if enttype=="466"
drop if eventdate==""
rename data3 unit 
tab unit 
*keep valid units (ml/min/1.73m*2)
*207 = ml/min/1.73m
*208 = ml/min/1.73m*2
*249 = mL/24h/1.73m*2
*251 = mL/min/1.73m*2
*288 = mL/min/1.73m2
*298 = mL/min/1.72m*2
keep if unit=="251" | unit=="288" | unit=="298" | unit=="207" | unit=="208" | unit=="249"
rename data2 eGFR 
destring eGFR, replace 
sum eGFR, d 
drop if  eGFR>500 | eGFR==.
sum eGFR, d 
keep patid eventdate eGFR 

*Append with calculated eGFR (serum creatinine)
append using "...\eGFR_serumm2.dta", force 

*merge with main (keep if prior or same date as index date)
merge m:1 patid using "...\non_fatalHF2.dta", force 
drop if _merge==1
drop _merge
gen diff = date(indexdate2, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
keep patid eGFR 

*merge with main 
merge 1:1 patid using "...\non_fatalHF2.dta", force  
drop if _merge==1
drop _merge
replace CKD=1 if eGFR<60
replace CKD=0 if CKD >=60
replace CKD=. if eGFR==.
save "...\non_fatalHF2.dta", replace 

*check population same 
distinct patid 

****************
*save cleaned dataset 
save "...\nonfatalHF_clean2.dta", replace 
exit

