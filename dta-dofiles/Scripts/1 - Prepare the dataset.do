cd"/Users/pablobrugarolas/Documents/Cloud/2020-21/MILE msc/FIRST TERM/FAM ECONOMICS/Research proposal/do-dta files"
use "Data/FieldExp_Public.dta", clear


*******************************************************************************
*							DATA PREPROCESSING								  *
*******************************************************************************


* Rename and create new vars

rename id job_posting_id
gen cv_id=_n, before (job_posting_id)


encode msa, gen(nmsa) 
order nmsa, after(msa)
drop msa

gen workingmum = 0, after(parent)
replace workingmum = 1 if woman == 1 & parent ==1

gen blackworkingmum = 0, after(workingmum)
replace blackworkingmum = 1 if woman == 1 & parent ==1 & black ==1


gen ocup_low =  0 if occupation !=., before(occupation)
replace ocup_low = 1 if occupation == 2 | occupation == 3 | occupation == 5 


* Label the vars

label variable cv_id "Unique identifier for each CV"
label variable job_posting_id "Unique identifier for each job posting"
label variable callback "Indicates whether the application received a callback"
label variable black "Race of applicant"
label variable woman "Gender of applicant"
label variable workingmum "The applicant is a working mother"
label variable blackworkingmum "The applicant is a black working mother"
label variable parent "Parental status of applicant"
label variable nmsa "Metropolitan Statistical Area from which job posting was drawn"
label variable ocup_low "Occupations requiring less education"
label variable occupation "Occupation of job posting"


* Save the cleaned data

save "Data/FieldExp_Public_cleaned.dta", replace



