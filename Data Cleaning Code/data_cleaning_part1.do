**************************DATA CLEANING****************************************
**Defining ischaemic heart disease
//ischaemic heart disease broader (ihdb [combining codeslists for ischaemic heart disease, angina and myocardial infarction to be most comprehensive]) 

**CPRD Aurum**
cd "...\medcodeid\"
use aurum_mi_description, clear 
append using aurum_ihd_description, force 
append using aurum_angina_description, force 
duplicates drop medcodeid, force 
generate str ihdb = "1"
*save ihdb description 
save aurum_ihdb_description, replace 
keep medcodeid ihdb 
*save ihdb 
save aurum_ihdb, replace 

**CPRD GOLD**
cd "...\medcode\"
use mi_description, clear 
append using angina_description, force 
append using ihd_description, force 
duplicates drop medcode, force 
generate str ihdb = "1"
keep medcode ihdb readterm 
*save ihdb description 
save ihdb_description, replace 
keep medcode ihdb 
*save ihdb 
save ihdb, replace 

**HES**
cd "...\HES\"
use angina_hes, clear 
append using ihd_hes, force 
append using mi_hes, force 
duplicates drop icd, force 
generate str ihdb_hes = "1"
*save ihdb description (HES)
save ihdb_hes_description, replace 
keep icd ihdb_hes 
*save ihdb (HES)
save ihdb_hes, replace 

**********************CPRD AURUM DATA CLEANING**********************************
*load txt file aurum (all strings)
import delimited "...\matched_cohort_aurum.txt", stringcols(_all) clear  
distinct patid 
*save for merging 
save "...\aurum_cohort.dta", replace 
save "...\aurum_cohort1.dta", replace 

********************************************************************************
*Baseline characteristics 
*1. import aurum HES baseline and save 
clear 
import delimited "...\hes_diagnosis_hosp_21_000355_DM_baseline.txt", stringcols(_all) clear  
keep patid admidate icd 
rename admidate obsdate 
cd "...\STATA format\"
save aurum_hes_diagnosis_hosp_21_000355_DM_baseline, replace

********************************************************************************
*2. Baseline characteristics 
*Note: cd "...\HES\" = These are HES codeslists (same as used in both CPRD Aurum and GOLD data cleaning)
*Comorbid conditions
//Ischaemic heart disease broader (ihdb) = codeslist for ischaemic heart disease, myocardial infarction and angina combined to be comprehensive. 
clear 
foreach ot in af anaemia asthma cancer cld copd dementia depression oa pvd ra stroke thy hf htn ihdb {
cd "...\STATA format\"
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
merge m:1 patid using "...\aurum_cohort.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
keep `ot'_BL patid 
merge 1:1 patid using "...\aurum_cohort1.dta", force
drop if _merge==1
drop _merge
save "...\aurum_cohort1.dta", replace
}

*3. Smoking (BL) (CPRD Aurum only)
clear 
foreach ot in smoking {
cd "...\append\"
use `ot', clear 
rename obsdate eventdate 
drop if missing(eventdate)
keep patid smokstatus eventdate 
sort patid eventdate  
merge m:1 patid using "...\aurum_cohort.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
keep patid smokstatus  
merge 1:1 patid using "...\aurum_cohort1.dta", force
drop if _merge==1
drop _merge
save "...\aurum_cohort1.dta", replace
}

*4. Alcohol (BL) (CPRD Aurum only) 
foreach ot in alcohol {
cd "...\append\"
use `ot', clear  
rename obsdate eventdate 
drop if missing(eventdate)
keep patid alcstatus eventdate
sort patid eventdate  
merge m:1 patid using "...\aurum_cohort.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
keep patid alcstatus 
merge 1:1 patid using "...\aurum_cohort1.dta", force
drop if _merge==1
drop _merge
save "...\aurum_cohort1.dta", replace
}

********************************************************************************
*Treatments 
foreach dm in alphab ace angioIIrb antiplat bb cc diuretics glp1ra insulin meg met statin sglt2 sulf tzd other_hyper other_lipid digoxin nitrates {
clear 
cd "...\append\"
use `dm', clear 
rename issuedate eventdate
drop if missing(eventdate)
merge m:1 patid using "...\aurum_cohort.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
keep patid `dm'
merge 1:1 patid using "...\aurum_cohort1.dta", force
drop if _merge==1
drop _merge
save "...\aurum_cohort1.dta", replace
}

********************************************************************************
//1. Add CPRD outcomes 
clear 
foreach ot in hf ihdb {	
cd "...\append\"
use `ot', clear 
rename obsdate eventdate 
drop if missing(eventdate)
keep patid eventdate `ot'
merge m:1 patid using "...\aurum_cohort.dta", force
drop if _merge==1
drop _merge
gen diff = date(eventdate, "DMY") - date(indexdate, "DMY")
keep if diff>0 
sort patid diff
by patid: keep if _n == 1
gen tt_`ot'_outc = date(eventdate, "DMY") - date(indexdate, "DMY") 
rename eventdate event_`ot'_outc 
rename `ot' `ot'_outc 
drop diff  
merge 1:1 patid using "...\aurum_cohort1.dta", force
drop if _merge==1
drop _merge 
save "...\aurum_cohort1.dta", replace
}

********************************************************************************
//2. Add HES outcomes 
*import HES primary data 
import delimited "...\hes_primary_diag_hosp_21_000355_DM.txt", stringcols(_all) clear  
rename admidate eventdate 
rename icd_primary icd
keep patid eventdate icd 
cd "...\STATA format\"
save aurum_hes_primary_diag_hosp_21_000355_DM_outcomes, replace

*Define HES in aurum cohort 
foreach ot in hf ihdb {
cd "...\HES\"
use `ot'_hes, clear 
merge 1:m icd using "...\aurum_hes_primary_diag_hosp_21_000355_DM_outcomes", force keep(3) nogenerate 
keep patid `ot'_hes eventdate 
drop if missing(eventdate)
merge m:1 patid using "...\aurum_cohort.dta", force 
drop if _merge==1 
drop _merge 
gen diff = date(eventdate, "DMY") - date(indexdate, "DMY")
keep if diff>0 
sort patid diff
bysort patid (diff): keep if _n==1
gen tt_`ot'_outh = date(eventdate, "DMY") - date(indexdate, "DMY")
rename eventdate event_`ot'_outh 
rename `ot'_hes `ot'_outh 
drop diff 
merge 1:1 patid using "...\aurum_cohort1.dta", force
drop if _merge==1
drop _merge
save "...\aurum_cohort1.dta", replace
}

//3. Add CVD death data 
clear
foreach ot in hf ihdb {
import delimited "...\death_patient_21_000355_DM.txt", stringcols(_all) clear  
keep patid dod cause 
rename cause icd 
cd "...\HES\"
merge m:1 icd using `ot'_hes, force keep(3) nogenerate 
keep if `ot'_hes=="1"
drop if dod==""
drop if patid==""
merge 1:1 patid using "...\aurum_cohort.dta", force 
drop if _merge==1 
drop _merge 
gen diff = date(dod, "DMY") - date(indexdate, "DMY") 
keep if diff>0 
bysort patid (diff): keep if _n==1
gen tt_`ot'_dod = date(dod, "DMY") - date(indexdate, "DMY")
rename dod `ot'_dod 
keep patid tt_`ot'_dod `ot'_dod
merge 1:1 patid using "...\aurum_cohort1.dta", force 
drop if _merge==1 
drop _merge 
save "...\aurum_cohort1.dta", replace 
}

********************************************************************************
*Add ONS death data (dod)
********************************************************************************
*Format death data (allstrings)
import delimited "...\death_patient_21_000355_DM.txt", stringcols(_all) clear 
keep patid dod 
drop if dod==""
merge 1:1 patid using "...\aurum_cohort1.dta", force
drop if _merge==1
drop _merge
*death (0,1)
gen death="1" if !missing(dod)
replace death="0" if missing(dod)
destring death, replace
save "...\aurum_cohort1.dta", replace

********************************************************************************
*Merge with ONS aurum codes
//1. Ethnicity 
//Add ethnicity 
*gen_ethnicity (Ethnicity derived from HES data (including Admitted patient care, Outpatient, A&E, PROMs and DID))
import delimited "...\hes_patient_21_000355_DM.txt", stringcols(_all) clear 
gen ethnic= "SA" if gen_ethnicity=="Bangladesi" | gen_ethnicity=="Indian" | gen_ethnicity=="Pakistani" 
replace ethnic= "Black" if gen_ethnicity=="Bl_Afric" | gen_ethnicity=="Bl_Carib" | gen_ethnicity=="Bl_Other" 
replace ethnic= "MO" if gen_ethnicity=="Mixed" | gen_ethnicity=="Oth_Asian" | gen_ethnicity=="Other" | gen_ethnicity=="Chinese" 
replace ethnic="White" if gen_ethnicity=="White" 
replace ethnic="Unknown" if gen_ethnicity=="Unknown"
keep patid ethnic gen_ethnicity 
merge 1:1 patid using "...\aurum_cohort1.dta", force 
drop if _merge==1
drop _merge 
save "...\aurum_cohort1.dta", replace 

//2. IMD 
import delimited "...\patient_2019_imd_21_000355.txt", stringcols(_all) clear 
rename e2019_imd_5 imd 
keep patid imd 
merge 1:1 patid using "...\aurum_cohort1.dta", force 
*_merge==1 (0) from master 
drop if _merge==1
drop _merge 
save "...\aurum_cohort1.dta", replace 

//3. Add tod - exposed 
import delimited "...\Aurum_DIAB_Expo_Extract_Patient_001.txt", stringcols(_all) clear 
*regenddate = transfer out date or death date 
*patid is string; tod is string 
keep patid regenddate 
rename regenddate tod  
merge 1:1 patid using "...\aurum_cohort1.dta", force
*_merge==1 (0) from master
drop if _merge==1 
drop _merge 
save "...\aurum_cohort1.dta", replace 

//Add tod - unexposed
*patid is string; tod is string
import delimited "...\Aurum_DIAB_NonExpo_Extract_Patient_001.txt", stringcols(_all) clear 
keep patid regenddate 
rename regenddate tod  
merge 1:1 patid using "...\aurum_cohort1.dta", force
*_merge==1 (0) from master
drop if _merge==1 
drop _merge 
save "...\aurum_cohort1.dta", replace 

//4. Add locd - exposed and unexposed 
*using exposed lcd, but same as unexposed (only need to use 1 file)
import delimited "...\Aurum_DIAB_NonExpo_Extract_Practice_001.txt", stringcols(_all) clear 
*patid is strings; locd is string 
keep pracid lcd 
merge 1:m pracid using "...\aurum_cohort1.dta", force
drop if _merge==1 
drop _merge 
save "...\aurum_cohort1.dta", replace 
*check correct?
count if missing(lcd)
*0 observations lcd missing 
*I used exposed lcd to merge; same lcd in unexposed file 

//5. Add HES linkage date (linkdate)
*patid is string, linkdate is string 
import delimited "...\21_000355_linkage_eligibility_aurum.txt", stringcols(_all) clear 
keep patid linkdate 
merge 1:1 patid using "...\aurum_cohort1.dta", force 
*_merge==1 (0) from master 
drop if _merge==1 
drop _merge 
save "...\aurum_cohort1.dta", replace 

//6. Add end date - ONS 
*last linkage date in ONS - "29/03/2021"
use "...\aurum_cohort1.dta", clear 
*enddate for Aurum - ONS = 29/03/2021 (same as gold)
gen enddate = date("29/03/2021", "DMY")
format enddate %td
save "...\aurum_cohort1.dta", replace 

********************************************************************************
*Add test files 
********************************************************************************
*Bringing in test files 
*1.Add HbA1c (%) 
*units = % or mmol/mol 
*import delimited "...\NumUnit.txt", stringcols(_all) clear 
*merge 1:m numunitid using "...\append\HbA1c.dta", force 
*drop if _merge==1 
*drop _merge 
*save "...\append\HbA1c.dta", replace 
*explore units 
*tab description
*destring value, replace 
*sum value, d 

*explore units 
*tab description
*destring value, replace 
*sum value, d 

******1. convert mmol/mol to (%)
*Conversion formulas: (%)= (0.0915 × `mmol/mol') + 2.15

*keep if units == mmol/mol 
*keep if description=="IFCCmmol/mol" | description=="dcct" | description=="mmol/ mol" | description=="mmol / mol" | description=="mmol/mol" | description=="mmol/molHb" | description=="mmols/mol" 
*tab description 
*destring value, replace 
*convert mmol/mol to (%)
*gen hb=(value*0.0915) + 2.15
*sort patid obsdate
*keep patid obsdate hb

*save "...\append\HbA1c_perc1.dta", replace 

*****2. convert mmol/l to (%)
*HbA1c(%) = 'mmol/l'*0.6277 + 1.627 
*use "...\append\HbA1c.dta", clear 
*keep if description=="mmol/L"
*destring value, replace 
*convert mmol/l to (%)
*gen hb=(value*0.6277) + 1.627
*sort patid obsdate
*keep patid obsdate hb

*save "...\append\HbA1c_perc2.dta", replace 

******3. Append % (converted from mmol/mol) to % in HbA1c codeslist 

*load HbA1c data 
*use "...\append\HbA1c.dta", clear 
*tab description
*keep if units == (%)
*keep if strpos(description,"%") | strpos(description,"perc") | description=="per cent"
*tab description 
*rename value hb 
*destring hb, replace 
*keep patid obsdate hb
*append using "...\append\HbA1c_perc1.dta", force 
*append using "...\append\HbA1c_perc2.dta", force 
*drop if hb==. 
*sort patid obsdate 
*sum hb, d
*drop if hb<3 | hb>23
*sum hb, d 

*Add HbA1c (mmol/mol) column variable 
*gen hb_mm=(hb-2.15)/0.0915
*sum hb_mm, d 

*merge with main aurum cohort (keep closest to index date)
*keep patid obsdate hb hb_mm 
*rename obsdate eventdate 
*sort patid eventdate 
*merge m:1 patid using "...\aurum_cohort.dta", force
*drop if _merge==1
*drop _merge
*gen diff = date(indexdate, "DMY") - date(eventdate, "DMY")
*drop if diff<0
*sort patid diff
*by patid: keep if _n == 1
*drop diff eventdate
*merge 1:1 patid using "...\aurum_cohort1.dta", force
*drop if _merge==1
*drop _merge
*save "...\aurum_cohort1.dta", replace

********************************************************************************
*2. add lipids
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
merge m:1 patid using "...\aurum_cohort.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
drop diff eventdate
merge 1:1 patid using "...\aurum_cohort1.dta", force
drop if _merge==1
drop _merge
save "...\aurum_cohort1.dta", replace

********************************************************************************
*3. add systolic blood pressure 
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
merge m:1 patid using "...\aurum_cohort.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
drop diff eventdate
merge 1:1 patid using "...\aurum_cohort1.dta", force
drop if _merge==1
drop _merge
save "...\aurum_cohort1.dta", replace 

********************************************************************************
*4. BMI calculation
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
merge m:1 patid using "...\aurum_cohort.dta", force keep(3) nogenerate
rename obsdate eventdate 
gen diff = date(indexdate, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
save "...\append\weight_BL_patid", replace 

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
merge m:1 patid using "...\aurum_cohort.dta", force keep(3) nogenerate
rename obsdate eventdate 
gen diff = date(indexdate, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
save "...\append\height_BL_patid.dta", replace 

********append weight/height********* 
use "...\append\weight_BL_patid", clear 
merge 1:1 patid using "...\append\height_BL_patid.dta", force keep(3) nogenerate
gen BMI=(kg/height2)
keep patid eventdate BMI 
save "...\append\BMI_patid", replace 

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
append using "...\append\BMI_patid", force 
sort patid eventdate 
drop if BMI==. 
merge m:1 patid using "...\aurum_cohort.dta", force
drop if _merge==1 
drop _merge 
drop if BMI<10.1 | BMI>80 
gen BMI_rnd=round(BMI)
gen diff = date(indexdate, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1

gen BMI_cat= BMI
recode BMI_cat 0/18.49999=1 18.5/24.9999=2 25/29.9999=3 30/34.99999=4 35/39.9999=5 40/81=6
label define bmi 1 "Underweight" 2 "Normal weight" 3 "Pre-obesity" 4 "Obesity class I" 5 "Obesity class II" 6 "Obesity class III"
label values BMI_cat bmi

gen BMI_cat4= BMI
recode BMI_cat4 0/18.4999=1 18.5/29.9999=2 30/81=3 
label define bmi2 1 "Underweight" 2 "Normal weight" 3 "Obese" 
label values BMI_cat4 bmi2

gen Obesity=1 if  BMI_cat4==3
replace  Obesity=0 if  (Obesity==. &  BMI<.)

keep patid BMI BMI_rnd BMI_cat BMI_cat4 Obesity 
merge 1:1 patid using "...\aurum_cohort1.dta", force 
drop if _merge==1 
drop _merge 

save "...\aurum_cohort1.dta", replace

********************************************************************************
*5.eGFR calculation 

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

merge m:1 patid using "...\aurum_cohort1.dta", force 
drop if _merge==1
drop _merge

gen diff = date(indexdate, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1

*generate age variable
gen index = date(indexdate, "DMY")
format index %td
gen indexyear = yofd(index)
destring yob, replace 
gen age = indexyear - yob 

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
save "...\append\serum_patid.dta", replace 

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
append using "...\append\serum_patid.dta", force 
sort patid eventdate 
merge m:1 patid using "...\aurum_cohort.dta", force 
drop if _merge==1
drop _merge
gen diff = date(indexdate, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1

keep patid eGFR
merge 1:1 patid using "...\aurum_cohort1.dta", force  
drop if _merge==1
drop _merge
gen CKD=1 if eGFR<60
replace CKD=0 if CKD >=601
replace CKD=. if eGFR==.
save "...\aurum_cohort1.dta", replace 

*************************************************************
//Data preparation/continue cleaning 

*load data 
use "...\aurum_cohort1.dta", clear 

*1.binary values
*outcome (0,1)
foreach ot in hf ihdb {	
destring `ot'_outc, replace 
destring `ot'_outh, replace 
replace `ot'_outc=0 if `ot'_outc==.
replace `ot'_outh=0 if `ot'_outh==.
}

*comorbid variables (0,1)
foreach ot in oa copd cld cancer dementia asthma thy stroke pvd anaemia depression ra hf af ihdb htn {	
destring `ot'_BL, replace 
replace `ot'_BL=0 if `ot'_BL==.
}

*treatments (0,1)
foreach ot in ace met meg sglt2 alphab bb antiplat cc glp1ra other_lipid other_hyper statin nitrates angioIIrb digoxin diuretics insulin sulf tzd {	
destring `ot', replace 
replace `ot'=0 if `ot'==.
}

*2.gender define 
tab gender 
replace gender="male" if gender=="M"
replace gender="female" if gender=="F"
tab gender 

*3. alcstatus (clean category) 
tab alcstatus 
replace alcstatus="non" if alcstatus=="non "
tab alcstatus 

*4.Treatments 
*antiplat 
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

*5.Smoking category 
gen smokstatus_cut="ever" if smokstatus=="current or ex-smoker" | smokstatus=="current smoker" | smokstatus=="ex-smoker"
replace smokstatus_cut="non" if smokstatus=="non-smoker"


*6. prepare data for calculating survival time 

*end 
gen tt_end = date("29/03/2021", "DMY") - date(indexdate, "DMY")

*death (ONS)
gen tt_dod = date(dod, "DMY") - date(indexdate, "DMY")

*linkdate (HES)
gen tt_linkdate = date(linkdate, "DMY") - date(indexdate, "DMY")

*TOD (CPRD)
gen tt_tod = date(tod, "DMY") - date(indexdate, "DMY")

*LCD (CPRD)
gen tt_lcd = date(lcd, "DMY") - date(indexdate, "DMY")

*add indicator (gold vs. aurum)
generate str cprd="aurum"

*save data 
save "...\aurum_cohort1.dta", replace  

**********************CPRD GOLD DATA CLEANING***********************************
*Import cohort 
import delimited "...\matched_cohort_all.txt", stringcols(_all) clear 
*save for merging 
save "...\gold_cohort.dta", replace  
*save for cohort 
import delimited "...\matched_cohort_all.txt", stringcols(_all) clear 
save "...\gold_cohort1.dta", replace 

********************************************************************************
*1.Baseline Characterstics 
*import HES baseline (all strings)
import delimited "...\hes_diagnosis_hosp_21_000355_DM.txt", stringcols(_all) clear 
keep patid admidate icd 
rename admidate eventdate
cd "...\STATA format\"
save hes_diagnosis_hosp_21_000355_DM_STATA_baseline, replace 

********************************************************************************
*merges all HES baseline with codeslist 
//Ischaemic heart disease broader (ihdb) = codeslist for ischaemic heart disease, myocardial infarction and angina combined to be comprehensive. 
clear
foreach ot in af asthma cancer cld copd dementia htn oa ra thy anaemia depression hf ihdb pvd stroke {
cd "...\HES\"
use `ot'_hes, clear 
keep icd `ot'_hes
cd "...\STATA format\"
merge 1:m icd using hes_diagnosis_hosp_21_000355_DM_STATA_baseline, force keep(3) nogenerate 
keep patid `ot'_hes eventdate 
cd "...\HES\update_merge\"
save `ot'_hes, replace 
}

********************************************************************************
*2. Add smoking and alcohol (CPRD Gold only) 

//Smoking 
clear 
foreach ot in smoking {
use "...\append_clinical_ref.dta", clear
cd "...\update"
merge m:1 medcode using `ot', force keep(3) nogenerate  
drop if eventdate==""
sort patid 
keep patid eventdate smokstatus 
merge m:1 patid using "...\gold_cohort.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
keep patid smokstatus 
merge 1:1 patid using "...\gold_cohort1.dta", force
drop if _merge==1
drop _merge
save "...\gold_cohort1.dta", replace
}

//Alcohol 
clear 
foreach ot in alcohol {
use "...\append_clinical_ref.dta", clear
cd "...\update"
merge m:1 medcode using `ot', force keep(3) nogenerate  
drop if eventdate==""
sort patid 
keep patid eventdate alcstatus 
merge m:1 patid using "...\gold_cohort.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
keep patid alcstatus 
merge 1:1 patid using "...\gold_cohort1.dta", force
drop if _merge==1
drop _merge
save "...\gold_cohort1.dta", replace
}

//BL comorbid conditions 
//Ischaemic heart disease broader (ihdb) = codeslist for ischaemic heart disease, myocardial infarction and angina combined to be comprehensive. 
clear 
foreach ot in af asthma cancer cld copd dementia htn oa ra thy anaemia depression hf ihdb pvd stroke {
cd "...\medcode\"
use `ot', clear 
merge 1:m medcode using "...\append_clinical_ref.dta", force keep(3) nogenerate
cd "...\HES\update_merge\"
append using `ot'_hes, force 
drop if eventdate==""
generate str `ot'_BL="1"
keep patid eventdate `ot'_BL
sort patid 
merge m:1 patid using "...\gold_cohort.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
keep `ot'_BL patid 
merge 1:1 patid using "...\gold_cohort1.dta", force
drop if _merge==1
drop _merge
save "...\gold_cohort1.dta", replace
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
merge m:1 patid using "...\gold_cohort.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
keep patid `tm'
merge 1:1 patid using "...\gold_cohort1.dta", force
drop if _merge==1
drop _merge
save "...\gold_cohort1.dta", replace
}

********************************************************************************
//4. Define outcome variables in CPRD GOLD  
*Import HES outcome file (all strings)
import delimited "...\hes_primary_diag_hosp_21_000355_DM.txt", stringcols(_all) clear 
keep patid admidate icd_primary 
rename icd_primary icd
rename admidate eventdate 
cd "...\STATA format\"
save hes_primary_diag_hosp_21_000355_DM_STATA_outcomes, replace 

*Define HES outcomes in CPRD gold 
//Ischaemic heart disease broader (ihdb) = codeslist for ischaemic heart disease, myocardial infarction and angina combined to be comprehensive. 
clear
foreach ot in hf ihdb {
cd "...\HES\"
use `ot'_hes, clear 
cd "...\STATA format\"
merge 1:m icd using hes_primary_diag_hosp_21_000355_DM_STATA_outcomes, force keep(3) nogenerate 
keep patid `ot'_hes eventdate 
drop if eventdate==""
merge m:1 patid using "...\gold_cohort.dta", force 
drop if _merge==1 
drop _merge  
gen diff = date(eventdate, "DMY") - date(indexdate, "DMY") 
keep if diff>0 
bysort patid (diff): keep if _n==1
gen tt_`ot'_outh = date(eventdate, "DMY") - date(indexdate, "DMY") 
rename eventdate event_`ot'_outh 
rename `ot'_hes `ot'_outh 
drop diff 
merge 1:1 patid using "...\gold_cohort1.dta", force
drop if _merge==1
drop _merge 
save "...\gold_cohort1.dta", replace
}

//CPRD outcomes 
*Define CPRD outcomes in CPRD GOLD 
//Ischaemic heart disease broader (ihdb) = codeslist for ischaemic heart disease, myocardial infarction and angina combined to be comprehensive. 
clear 
foreach ot in hf ihdb {	
use "...\append_clinical_ref.dta", clear
cd "...\medcode\"
merge m:1 medcode using `ot', force keep(3) nogenerate  
drop medcode 
drop if eventdate==""
cd "..."
merge m:1 patid using gold_cohort, force
drop if _merge==1
drop _merge
gen diff = date(eventdate, "DMY") - date(indexdate, "DMY") 
keep if diff>0 
sort patid diff
by patid: keep if _n == 1
gen tt_`ot'_outc = date(eventdate, "DMY") - date(indexdate, "DMY") 
rename eventdate event_`ot'_outc 
rename `ot' `ot'_outc 
drop diff 
merge 1:1 patid using "...\gold_cohort1.dta", force
drop if _merge==1
drop _merge 
save "...\gold_cohort1.dta", replace
}

//CVD death data outcomes - with HES codes 
//Ischaemic heart disease broader (ihdb) = codeslist for ischaemic heart disease, myocardial infarction and angina combined to be comprehensive. 
clear
foreach ot in hf ihdb {
import delimited "...\death_patient_21_000355_DM.txt", stringcols(_all) clear  
keep patid dod cause 
rename cause icd 
cd "...\HES\"
merge m:1 icd using `ot'_hes, force keep(3) nogenerate 
keep if `ot'_hes=="1"
drop if dod==""
drop if patid==""
merge 1:1 patid using "...\gold_cohort.dta", force 
drop if _merge==1 
drop _merge 
gen diff = date(dod, "DMY") - date(indexdate, "DMY") 
keep if diff>0 
bysort patid (diff): keep if _n==1
gen tt_`ot'_dod = date(dod, "DMY") - date(indexdate, "DMY")
rename dod `ot'_dod 
keep patid tt_`ot'_dod `ot'_dod
merge 1:1 patid using "...\gold_cohort1.dta", force
drop if _merge==1
drop _merge 
save "...\gold_cohort1.dta", replace 
}

*************************************************************
*Merging HES/ONS linkage files 

//Add imd 
*distinct patid - 1094476 
import delimited "...\patient_2019_imd_21_000355.txt", stringcols(_all) clear 
rename e2019_imd_5 imd 
keep patid imd 
merge 1:1 patid using "...\gold_cohort1.dta", force 
*_merge==1 (0) from master 
drop if _merge==1
drop _merge 
save "...\gold_cohort1.dta", replace 

//Add ethnicity 
*gen_ethnicity (Ethnicity derived from HES data (including Admitted patient care, Outpatient, A&E, PROMs and DID))
import delimited "...\hes_patient_21_000355_DM.txt", stringcols(_all) clear 
gen ethnic= "SA" if gen_ethnicity=="Bangladesi" | gen_ethnicity=="Indian" | gen_ethnicity=="Pakistani" 
replace ethnic= "Black" if gen_ethnicity=="Bl_Afric" | gen_ethnicity=="Bl_Carib" | gen_ethnicity=="Bl_Other" 
replace ethnic= "MO" if gen_ethnicity=="Mixed" | gen_ethnicity=="Oth_Asian" | gen_ethnicity=="Other" | gen_ethnicity=="Chinese"
replace ethnic="White" if gen_ethnicity=="White" 
replace ethnic="Unknown" if gen_ethnicity=="Unknown"
keep patid ethnic gen_ethnicity 
merge 1:1 patid using "...\gold_cohort1.dta", force 
drop if _merge==1 
drop _merge 
save "...\gold_cohort1.dta", replace 

//Add tod - exposed 
import delimited "...\Proj69_DM_Extract_Patient_001.txt", stringcols(_all) clear 
*patid is string; tod is string 
keep patid tod 
merge 1:1 patid using "...\gold_cohort1.dta", force
*_merge==1 (0) from master
drop if _merge==1 
drop _merge 
save "...\gold_cohort1.dta", replace 

//Add tod - unexposed 
*patid is string; tod is string
import delimited "...\GOLD_NE_21_000355_Extract_Patient_001.txt", stringcols(_all) clear 
keep patid tod 
merge 1:1 patid using "...\gold_cohort1.dta", force
*_merge==1 (0) from master
drop if _merge==1 
drop _merge 
save "...\gold_cohort1.dta", replace 

//Add locd - exposed and unexposed 
*using exposed lcd, but same as unexposed (only need to use 1 file)
import delimited "...\Proj69_DM_Extract_Practice_001.txt", stringcols(_all) clear 
*patid is strings; locd is string 
keep pracid lcd 
merge 1:m pracid using "...\gold_cohort1.dta", force
drop if _merge==1 
drop _merge 
save "...\gold_cohort1.dta", replace 
*check correct?
count if missing(lcd)
*0 observations lcd missing 
*I used exposed lcd to merge; same lcd in unexposed file 

//Add HES linkage date (linkdate)
*patid is string, linkdate is string 
import delimited "...\21_000355_linkage_eligibility_gold.txt", stringcols(_all) clear 
keep patid linkdate 
merge 1:1 patid using "...\gold_cohort1.dta", force 
*_merge==1 (0) from master 
drop if _merge==1 
drop _merge 
save "...\gold_cohort1.dta", replace 

//Add end date - ONS 
*last linkage date in ONS - "29/03/2021"
gen enddate = date("29/03/2021", "DMY")
format enddate %td
save "...\gold_cohort1.dta", replace 

*************************************************************
*Add date of death 
import delimited "...\death_patient_21_000355_DM.txt", stringcols(_all) clear  
keep patid dod 
drop if dod==""
merge 1:1 patid using "...\gold_cohort1.dta", force
drop if _merge==1
drop _merge 
*death (0,1)
gen death="1" if !missing(dod)
replace death="0" if missing(dod)
destring death, replace
save "...\gold_cohort1.dta", replace  

********************************************************************************
//Bringing in tests 

*BMI 
*Cholesterol
*Systolic
*HbA1c 
*eGFR 

*[Referral files DO NOT have adid/enttype - use ONLY CLINICAL FILES] 

*BMI 

***HEIGHT 
use "...\append_clinical.dta", clear 
*enttype 14 (height)
keep if enttype=="14"
sort patid eventdate adid
merge m:1 patid enttype adid using "...\append_add1.dta", force keep(3) nogenerate 
destring data1, gen(height_m) 
sort patid eventdate 
drop if eventdate==""
drop medcode data1 data2 data3 enttype adid

*drop implausible values and save 
drop if height_m<1.38 | height_m>1.88 
save "...\height_BL", replace 

**WEIGHT 
use "...\append_clinical.dta", clear 
*enttype 13 (weight)
keep if enttype=="13" 
sort patid enttype adid
merge m:1 patid enttype adid using "...\append_add1.dta", force keep(3) nogenerate 
destring data1, gen(weight_kg)
destring data3, gen(BMI)
sum  BMI, d
sum weight_kg, d
drop if eventdate==""
drop medcode data1 data2 data3 enttype adid
save "...\weight_BL.dta", replace 

//merge height and weight with patid 
use "...\weight_BL.dta", clear 
merge m:1 patid using "...\gold_cohort.dta", force keep(3) nogenerate 
gen diff = date(indexdate, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
drop diff 
save "...\weight_BL_patid.dta", replace 

use "...\height_BL.dta", clear 
merge m:1 patid using "...\gold_cohort.dta", force keep(3) nogenerate 
gen diff = date(indexdate, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
drop diff 
save "...\height_BL_patid.dta", replace 

use "...\weight_BL_patid.dta", clear 
merge 1:1 patid using "...\height_BL_patid.dta", force keep(3) nogenerate 
gen height2= height_m*height_m
replace BMI=(weight_kg/height2) if BMI==. 
gen BMI_rnd=round(BMI)
drop if BMI<10.1 |  BMI>80 
drop if eventdate==""
drop eventdate height_m height2 weight_kg 
save "...\BMI.dta", replace 

****1:1 merge with main 
use "...\BMI.dta", clear
merge 1:1 patid using "...\gold_cohort1.dta", force 
drop if _merge==1 
drop _merge 

gen BMI_cat= BMI
recode BMI_cat 0/18.49999=1 18.5/24.9999=2 25/29.9999=3 30/34.99999=4 35/39.9999=5 40/81=6
label define bmi 1 "Underweight" 2 "Normal weight" 3 "Pre-obesity" 4 "Obesity class I" 5 "Obesity class II" 6 "Obesity class III"
label values BMI_cat bmi

gen BMI_cat4= BMI
recode BMI_cat4 0/18.4999=1 18.5/29.9999=2 30/81=3 
label define bmi2 1 "Underweight" 2 "Normal weight" 3 "Obese" 
label values BMI_cat4 bmi2

gen Obesity=1 if  BMI_cat4==3
replace  Obesity=0 if  (Obesity==. &  BMI<.)
save "...\gold_cohort1.dta", replace

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
merge m:1 patid using "...\gold_cohort.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
drop diff eventdate  
merge 1:1 patid using "...\gold_cohort1.dta", force 
drop if _merge==1
drop _merge 
save "...\gold_cohort1.dta", replace

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
merge m:1 patid using "...\gold_cohort.dta", force
drop if _merge==1
drop _merge
gen diff = date(indexdate, "DMY") - date(eventdate, "DMY")
drop if diff<0
sort patid diff
by patid: keep if _n == 1
drop diff eventdate 
merge 1:1 patid using "...\gold_cohort1.dta", force 
drop if _merge==1
drop _merge 
save "...\gold_cohort1.dta", replace

********HbA1c (%)
*use "...\append_test.dta", clear
*275 = HbA1c - diabetic control
*keep if enttype=="275" 
*drop if eventdate==""
*rename data3 unit 
*rename data2 value 
*destring unit, replace 
*destring value, replace 
*tab unit 
*tab value 

*keep if plausible unit (%) | mmol/mol | mmol/l 
**Percentage 
*1 = % 
*215 = %Hb
*gen hb = value if unit== 1 | unit== 215

**mmol/mol 
*97 = mmol/mol
*205 = mmol/mol Hb
*replace hb = (value*0.0915) + 2.15 if unit==97 | unit==205 & hb==.

**mmol/L 
*96 = mmol/L
*replace hb = (value*0.6277) + 1.627 if unit==96 & hb==.

*HbA1c (mmol/mol)
*gen hb_mm=(hb-2.15)/0.0915

*apply inclusion criterion 
*sum hb, d 
*sum hb_mm, d
*drop if hb<3 | hb>23
*sum hb, d 
*sum hb_mm, d

*keep patid eventdate hb_mm hb
****1:1 with main 
*merge m:1 patid using "...\gold_cohort.dta", force
*drop if _merge==1
*drop _merge
*gen diff = date(indexdate, "DMY") - date(eventdate, "DMY")
*drop if diff<0
*sort patid diff
*by patid: keep if _n == 1
*keep patid hb hb_mm 
*merge 1:1 patid using "...\gold_cohort1.dta", force 
*drop if _merge==1
*drop _merge
*save "...\gold_cohort1.dta", replace 

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
merge m:1 patid using "...\gold_cohort1.dta", force 
drop if _merge==1
drop _merge
gen event = date(eventdate, "DMY")
format event %td
gen index = date(indexdate, "DMY")
format index %td
gen diff = index - event
drop if diff<0
sort patid diff
by patid: keep if _n == 1

*generate age variable
gen indexyear = yofd(index)
destring yob, replace 
gen age = indexyear - yob 

gen cr_k = CR_mgdL/0.7 if gender=="2"
replace cr_k = CR_mgdL/0.9 if gender=="1"

gen min =  cr_k if  cr_k<1
replace min=1 if cr_k >=1

gen max =  cr_k if  cr_k>1
replace max=1 if cr_k <=1

*gender[2] = female in CPRD 
*women 
gen eGFR= 141 * (min)^-0.329 * (max)^-1.209 * (0.993)^age * 1.018 if gender =="2"
replace eGFR = 141 * (min)^-0.329 * (max)^-1.209 * (0.993)^age * 1.018 * 1.159 if gender =="2" & ethnic=="Black"

*gender[1] = male in CPRD 
*men
replace eGFR = 141 * (min)^-0.411 * (max)^-1.209 * (0.993)^age  if gender =="1" 
replace eGFR = 141 * (min)^-0.411 * (max)^-1.209 * (0.993)^age * 1.159 if gender =="1" & ethnic=="Black"

sum  eGFR,d
drop if  eGFR>500 |  eGFR==.

keep patid eventdate eGFR 
save "...\eGFR_serum.dta", replace 

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
append using "...\eGFR_serum.dta", force 

*merge with main (keep if prior or same date as index date)
merge m:1 patid using "...\gold_cohort.dta", force 
drop if _merge==1
drop _merge
gen event = date(eventdate, "DMY")
format event %td
gen index = date(indexdate, "DMY")
format index %td
gen diff = index - event
drop if diff<0
sort patid diff
by patid: keep if _n == 1
keep patid eGFR 

*merge with main 
merge 1:1 patid using "...\gold_cohort1.dta", force  
drop if _merge==1
drop _merge
gen CKD=1 if eGFR<60
replace CKD=0 if CKD >=60
replace CKD=. if eGFR==.
save "...\gold_cohort1.dta", replace 

********************************************************************************
*Prepare data/continue data cleaning 

use "...\gold_cohort1.dta", clear 

*1.binary values
*outcome (0,1)
foreach ot in hf ihdb {	
destring `ot'_outc, replace 
destring `ot'_outh, replace 
replace `ot'_outc=0 if `ot'_outc==.
replace `ot'_outh=0 if `ot'_outh==.
}

*comorbid variables (0,1)
foreach ot in oa copd cld cancer dementia asthma thy stroke pvd anaemia depression ra hf af ihdb htn {	
destring `ot'_BL, replace 
replace `ot'_BL=0 if `ot'_BL==.
}

*treatments (0,1)
foreach ot in ace met meg sglt2 alphab bb antiplat cc glp1ra other_lipid other_hyper statin nitrates angioIIrb digoxin diuretics insulin sulf tzd {	
destring `ot', replace 
replace `ot'=0 if `ot'==.
}

*2.gender define 
tab gender 
replace gender="male" if gender=="1"
replace gender="female" if gender=="2"
tab gender 

*3.generate t2d and t1d variable
tab dm_type	group
generate str t2d="1" if dm_type=="2" & group=="1"
replace t2d="0" if dm_type=="2" & group=="0"
destring t2d, replace 

generate str t1d="1" if dm_type=="1" & group=="1"
replace t1d="0" if dm_type=="1" & group=="0"
destring t1d, replace 

*4.Treatments 
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

*5.Smoking category 
gen smokstatus_cut="ever" if smokstatus=="current or ex-smoker" | smokstatus=="current smoker" | smokstatus=="ex-smoker"
replace smokstatus_cut="non" if smokstatus=="non-smoker"

*6. Prepare for survival time code 

*end 
gen tt_end = date("29/03/2021", "DMY") - date(indexdate, "DMY")

*death (ONS)
gen tt_dod = date(dod, "DMY") - date(indexdate, "DMY")

*linkdate (HES)
gen tt_linkdate = date(linkdate, "DMY") - date(indexdate, "DMY")

*TOD (CPRD)
gen tt_tod = date(tod, "DMY") - date(indexdate, "DMY")

*LCD (CPRD)
gen tt_lcd = date(lcd, "DMY") - date(indexdate, "DMY")

*add indicator (gold vs. aurum)
generate str cprd="gold"

*save data 
save "...\gold_cohort1.dta", replace 

