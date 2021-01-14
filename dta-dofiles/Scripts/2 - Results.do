

cd"/Users/pablobrugarolas/Documents/Cloud/2020-21/MILE msc/FIRST TERM/FAM ECONOMICS/Research proposal/do-dta files"
use "Data/FieldExp_Public_cleaned.dta", clear

set more off


*******************************************************************************
*							DESCRIPTIVE ANALYSIS							  *
*******************************************************************************


*----------------------------------------------------------------------------------------------------------------
* Figure 1. Distribution of Applications Submitted in the Field Experiment
*----------------------------------------------------------------------------------------------------------------

* Creating some dummies for Table 1 and balance plots.
tab nmsa, gen(nmsa)
tab occupation, gen(occupation)

save "Data/FieldExp_Public_cleaned_plot.dta", replace

global covs nmsa2 nmsa3 nmsa4 nmsa5 nmsa6 nmsa7 nmsa8 nmsa9 nmsa10 nmsa11 nmsa12 nmsa13 nmsa14 nmsa15 nmsa16 nmsa17 nmsa18 nmsa19 nmsa20 occupation2 occupation3 occupation4 occupation5 occupation6

		
balancetable (mean if woman==0) (mean if woman==1) (diff woman) ///
			 (mean if parent==0) (mean if parent==1) (diff parent) ///
			 (mean if black==0) (mean if black==1) (diff black) $covs using "Results/Table 1A.tex", replace


* No longer needed, I use factors from now on
drop  $covs nmsa1 occupation1


*----------------------------------------------------------------------------------------------------------------
* Table 1. Proportions of Applicants Receiving Callbacks by Gender and Parental Status
*----------------------------------------------------------------------------------------------------------------

* Some simple tabulations
forvalues i=0/1 {
tab  parent callback if woman ==`i', row
}
*

* A z-test for differences in proportions
forvalues i=0/1 {
prtest callback if woman ==`i', by(parent)
}
*

/* 

At a descriptive level, there seems to be no differences. But we need a regression model 
to account for the clustered structure of our data! 

*/



*******************************************************************************
*									RESULTS									  *
*******************************************************************************


*----------------------------------------------------------------------------------------------------------------
* Table 2. The Effect of Gender, Parental, and Race status on Callbacks from Employers
*----------------------------------------------------------------------------------------------------------------

/* Controls included for the occupation and labor market, as well as all interactions
between these two variables. */


* Model 1, without controls

	logit callback i.woman i.parent i.black, vce(cluster job_posting_id)
	margins, dydx(woman parent black) atmeans
	
	/* Note: to correctly interpret changes in discreate treatment variables, 
	I use the dydx  option, which gives the expected difference in DV 
	between say women = 1 and women = 0.*/
		
	/* Being black decreases the probabily of callback by 1.45%
	Being a women increases the prob of callback by 1.015% */
	
* Model 1, with controls

	logit callback i.woman i.parent i.black i.occupation##i.nmsa, vce(cluster job_posting_id)
 
	margins, dydx(woman parent black) atmeans
	/* After the inclusion of controls, still same effect (expected due to randomization).
	Being black decreases the probabily of callback by 1.48% 
	Being a women increases the prob of callback by 0,898% */

* Model 2, without controls

	logit callback i.woman i.parent i.black i.workingmum, vce(cluster job_posting_id)
	margins, dydx(workingmum black) atmeans
	* Being a working mum decreases the probability of callback by 3.13%. 
	* More than double the racial discrimination effect (1.47%).

* Model 2, with controls

	logit callback  i.woman i.parent i.workingmum i.black i.occupation##i.nmsa, vce(cluster job_posting_id)
	margins, dydx(workingmum black) atmeans

	/* After the inclusion of controls, still same effect.
	Being a working mum decreases the prob of callback by 3.15% */
	

* Model 3, without controls
	logit callback  i.woman i.parent i.workingmum i.black i.blackworkingmum, vce(cluster job_posting_id)
	margins, dydx(workingmum black blackworkingmum) atmeans
	* No evidence of a cumulative effect of being a black working mum
	
* Model 3, with controls	

	logit callback  i.woman i.parent i.workingmum i.black  i.blackworkingmum i.occupation##i.nmsa, vce(cluster job_posting_id)
	margins, dydx(workingmum black blackworkingmum) atmeans
	/* After the inclusion of controls, still same effect.
	No evidence of a cumulative effect of being a black working mum */
	
	
	
*----------------------------------------------------------------------------------------------------------------
* Table 2. - Export results
*----------------------------------------------------------------------------------------------------------------

* Model 1, with controls

	quietly logit callback i.woman i.parent i.black i.occupation##i.nmsa,  vce(cluster job_posting_id)
	predict pr2_callbacka, pr
	quietly su pr2_callbacka
	estadd scalar pr = r(mean)
	estadd margins, dydx(woman parent black) atmeans
	est store A


* Model 2, with controls

	quietly logit callback  i.woman i.parent i.workingmum i.black i.occupation##i.nmsa,  vce(cluster job_posting_id) 
	predict pr2_callbackb, pr
	quietly su pr2_callbackb
	estadd scalar pr = r(mean)
	estadd margins, dydx(woman parent black workingmum) atmeans
	est store B

	
* Model 3, with controls	

	quietly logit callback  i.woman i.parent i.workingmum i.black  i.blackworkingmum i.occupation##i.nmsa,  vce(cluster job_posting_id) 
	predict pr2_callbackc, pr
	quietly su pr2_callbackc
	estadd scalar pr = r(mean)
	estadd margins, dydx(woman parent black workingmum blackworkingmum) atmeans
	est store C

* Create the latex file for Table 2	

	esttab A B C using "Results/Tables/Table 2.tex", replace f ///
	label booktabs b(3) p(3) eqlabels(none) alignment(S S) collabels("\multicolumn{1}{c}{Mfx/ SE}") ///
	drop(_cons 0.woman 0.parent 0.black 0.workingmum 0.blackworkingmum *.occupation *.nmsa) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	cells("margins_b(star)" "se(fmt(3)par)") ///
	refcat(women "\emph{Treatment}", nolabel) ///
	stats(N r2_p pr, fmt(0 3) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"Pseudo \(R^{2}\)"' `"Baseline predicted probability"'))


*******************************************************************************
*							HETEREOGENEITY ANALYSIS							  *
*******************************************************************************


*----------------------------------------------------------------------------------------------------------------
* Table 3 - By occupations requiring less/more education: The Effect of Gender and Parental status on Callbacks from Employers
*----------------------------------------------------------------------------------------------------------------

* Models 1-2, without controls
	forvalues i=0/1 {
	quietly: logit callback  i.woman i.parent i.black i.workingmum if ocup_low==`i', vce(cluster job_posting_id)
	margins, dydx(woman parent black workingmum) atmeans  
	}
	/* Heterogeneity analysis show large effects for cook: -9.3 (!), low-skilled sales: -5.1%, software developer: -3.5%.
	Way bigger than het. effects for black.*/

* Models 1-2, with controls
	forvalues i=0/1 {
	quietly: logit callback i.woman i.parent i.black i.workingmum i.nmsa if ocup_low==`i',  vce(cluster job_posting_id)
	margins, dydx(woman parent black workingmum) atmeans
	}
	* Robust to the inclusion of labor market controls. 
	/* Heterogeneity analysis show large effects for cook: -8.9% (!), low-skilled sales: -5.1%.
	Way bigger than het. effects for black.*/


*----------------------------------------------------------------------------------------------------------------
* Table 3. - Export results
*----------------------------------------------------------------------------------------------------------------	

* Models 1-2, with controls
	forvalues i=0/1 {
	quietly: logit callback i.woman i.parent i.black i.workingmum i.nmsa if ocup_low==`i',  vce(cluster job_posting_id)
	predict pr2_callback_educ`i', pr
	quietly su  pr2_callback_educ`i'
	estadd scalar pr = r(mean)
	estadd margins, dydx(woman parent black workingmum) atmeans
	est store es_educ`i'
	}
	*

* Create the latex file for Table 3
	esttab es_educ0 es_educ1 using "Results/Tables/Table 3.tex", replace f ///
	label booktabs b(3) p(3) eqlabels(none) alignment(S S) collabels("\multicolumn{1}{c}{Mfx/ SE}") ///
	drop(_cons 0.woman 0.parent 0.black 0.workingmum *.nmsa) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	cells("margins_b(fmt(3)star)" "se(fmt(3)par)") ///
	refcat(women "\emph{Treatment}", nolabel) ///
	stats(N r2_p pr, fmt(0 3) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"Pseudo \(R^{2}\)"' `"Baseline predicted probability"'))


*******************************************************************************
*									APPENDIX								  *
*******************************************************************************
	
	
*----------------------------------------------------------------------------------------------------------------
* Table 1A - By occupation: The Effect of Gender and Parental status on Callbacks from Employers
*----------------------------------------------------------------------------------------------------------------

* Models 1-6, without controls
	forvalues i=1/6 {
	quietly: logit callback  i.woman i.parent i.black i.workingmum if occupation==`i', vce(cluster job_posting_id)
	margins, dydx(woman parent black workingmum) atmeans  
	}
	/* Heterogeneity analysis show large effects for cook: -9.3 (!), low-skilled sales: -5.1%, software developer: -3.5%.
	Way bigger than het. effects for black.*/

	
* Models 1-6, with controls
	forvalues i=1/6 {
	quietly: logit callback i.woman i.parent i.black i.workingmum i.nmsa if occupation==`i',  vce(cluster job_posting_id)
	margins, dydx(woman parent black workingmum) atmeans
	}
	* Robust to the inclusion of labor market controls. 
	/* Heterogeneity analysis show large effects for cook: -8.9% (!), low-skilled sales: -5.1%.
	Way bigger than het. effects for black.*/

quietly: logit callback i.woman i.parent i.black i.workingmum i.nmsa if ocup_low==1,  vce(cluster job_posting_id)
margins, dydx(woman parent black workingmum) atmeans

quietly: logit callback i.woman i.parent i.black i.workingmum i.nmsa if ocup_low==0,  vce(cluster job_posting_id)
margins, dydx(woman parent black workingmum) atmeans



*----------------------------------------------------------------------------------------------------------------
* Table 1A. - Export results
*----------------------------------------------------------------------------------------------------------------

* Models 1-6, with controls
	forvalues i=1/6 {
	quietly logit callback i.woman i.parent i.black i.workingmum i.nmsa if occupation==`i', cluster(job_posting_id)
	predict pr2_callback`i', pr
	quietly su pr2_callback`i'
	estadd scalar pr = r(mean)
	estadd margins, dydx(woman parent black workingmum) atmeans
	est store es`i'
	}
	*

* Create the latex file for Table 3
	esttab es1 es2 es3 es4 es5 es6 using "Results/Tables/Table 2A.tex", replace f ///
	label booktabs b(3) p(3) eqlabels(none) alignment(S S) collabels("\multicolumn{1}{c}{Mfx/ SE}") ///
	drop(_cons 0.woman 0.parent 0.black 0.workingmum *.nmsa) ///
	star(* 0.10 ** 0.05 *** 0.01) ///
	cells("margins_b(fmt(3)star)" "se(fmt(3)par)") ///
	refcat(women "\emph{Treatment}", nolabel) ///
	stats(N r2_p pr, fmt(0 3) layout("\multicolumn{1}{c}{@}" "\multicolumn{1}{S}{@}") labels(`"Observations"' `"Pseudo \(R^{2}\)"' `"Baseline predicted probability"'))
