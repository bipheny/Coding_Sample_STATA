clear all
set more off
set seed 469283

cap mkdir "D:\undergraduate_research\working_data\UHS"
global workingdir "D:\undergraduate_research\working_data\UHS"
cap mkdir "D:\undergraduate_research\output\0916"
global outdir "D:\undergraduate_research\output\0916"


global robust_bartik  bartik_layoff1 bartik_layoff2
global robust laid_off_proxy_emp laid_off_proxy_market_alt laid_off_proxy_long fraction_industrial_SOE_9496 laid_off_proxy_POE laid_off_proxy_SOE layoff_ratio
global FE i.dcode i.year i.prov#i.year i.bigcity#i.year
global parallel treatmentin1993 treatmentin1994 treatmentin1995 ///
    treatmentin1996 treatmentin1997 treatmentin1998 ///
    treatmentin1999 treatmentin2000 treatmentin2001 treatmentin2002 



**parallel trend test**
cap  program drop generate_parallel_dummies
program define generate_parallel_dummies
forvalues i=1993(1)2002{
	gen treatmentin`i' = laid_off_proxy if year==`i'
	replace  treatmentin`i' = 0 if year!=`i'
}
replace  treatmentin1997 = 0

end

cap  program drop Parallel_Trend
program define Parallel_Trend
syntax, var(varname)
	eststo : reghdfe `var' $parallel `cont' , absorb(${FE}) vce(cluster dcode)
	coefplot (est1, mcolor(dknavy) m(D) c(l) msize(medsmall) ///
	        lc(dknavy) lp(solid) ciopts(lc(blue)) ) ///
	    ,vertical baselevels keep($parallel) omitted yline(0,lp(dot) lc(orange)) ///
		ytitle("""Regression Coefficient") xtitle("""Year") ///
		xline(5,lp(dot) lc(orange)) ///
		xlabel(1 "1993" 2 "1994" 3 "1995" 4 "1996" 5 "1997" 6 "1998" ///
		    7 "1999" 8 "2000" 9 "2001" 10 "2002" ) 
	graph export "${outdir}\Parallel_Trend_`var'_n.pdf", as(pdf) name("Graph") replace
	eststo clear
end

**robustness check, individual level, table 8**

clear 
use "${workingdir}\workdataindi.dta" 
merge m:1 dcode using "${workingdir}/workdatarobustness.dta",nogen
ren laid_off_proxy_treatment mainvar
foreach var in $robust  {
	replace `var' = `var' * isafter_treatment
	local treatment `var' male eduy age age_2 is_not_host yearage
	foreach depvar in employment asinh_labor_income asinh_total_income {
		eststo : reghdfe `depvar' `treatment', absorb(${FE}) vce(cluster dcode)
	}
}

esttab using  ${outdir}/robustnessi.csv, ///
    replace label  compress star(* 0.1 ** 0.05 *** 0.01) ///
	nogaps mlabel(,none) ///
	keep($robust ) ///
	b(%8.3f) se(%8.3f) r2(%8.2f)  hlinechar(`=char(151)')
	
eststo clear

***robustness check with bartik IV, individual level, table 8***
log using "${workingdir}/bartik2.log",replace
foreach var in $robust_bartik  {
	replace `var' = `var' * isafter_treatment
	local treatment  male eduy age age_2 is_not_host yearage
	foreach depvar in employment asinh_labor_income asinh_total_income {
		eststo : ivreghdfe `depvar'  `treatment' (mainvar =`var'), absorb(${FE}) ///
		    r cluster(dcode)  
	}
}

esttab using  ${outdir}/robustnessiv.csv, ///
    replace label  compress star(* 0.1 ** 0.05 *** 0.01) ///
	nogaps mlabel(,none) ///
	keep( mainvar ) ///
	b(%8.3f) se(%8.3f) r2(%8.2f)  hlinechar(`=char(151)')
	
eststo clear

log close	

clear

use "${workingdir}\workdatahousehold.dta" 
merge m:1 dcode using "${workingdir}/workdatarobustness.dta",nogen

keep if age_of_host != .& log_size_of_f_abo_18!=.
ren laid_off_proxy_treatment mainvar

**robustness check, household level, table 8**

foreach var in $robust  {
	replace `var' = `var' * isafter_treatment
	local treatmenth `var'  is_female_host age_of_host eduy_of_host 
	foreach depvar in asinh_dis_income_pc asinh_cons_pc ///
	    asinh_lend_pc log_size_of_f_abo_18 {
		eststo : reghdfe `depvar' `treatmenth', absorb(${FE}) vce(cluster dcode)
	}
	eststo : reghdfe asinh_educ_budget_pstu `treatmenth' ///
	    if number_of_stu!=0 , absorb(${FE}) vce(cluster dcode) 
	eststo : reghdfe co_resid_with_adult_child `treatmenth' ///
	    if age_of_host >= 45, absorb(${FE}) vce(cluster dcode)
}

esttab using  ${outdir}/robustnessh.csv, ///
    replace label  compress star(* 0.1 ** 0.05 *** 0.01) ///
	nogaps mlabel(,none) ///
	keep($robust  ) ///
	b(%8.3f) se(%8.3f) r2(%8.2f)  hlinechar(`=char(151)')
	
eststo clear

***robustness check with bartik IV, household level, table 8***

log using "${workingdir}/bartik2.log",append

foreach var in $robust_bartik  {
	replace `var' = `var' * isafter_treatment
	local treatmenth   is_female_host age_of_host eduy_of_host 
	foreach depvar in asinh_dis_income_pc asinh_cons_pc ///
	    asinh_lend_pc log_size_of_f_abo_18 {
		eststo : ivreghdfe `depvar' `treatmenth' (mainvar =`var') , absorb(${FE}) r cluster(dcode) 
	}
	eststo : ivreghdfe asinh_educ_budget_pstu `treatmenth' (mainvar =`var')  ///
	    if number_of_stu!=0 , absorb(${FE})  r cluster(dcode)  
	eststo : ivreghdfe co_resid_with_adult_child `treatmenth' (mainvar =`var')  ///
	    if age_of_host >= 45, absorb(${FE}) r cluster(dcode) 
}

esttab using  ${outdir}/robustness1005.rtf, ///
    append label  compress star(* 0.1 ** 0.05 *** 0.01) ///
	nogaps mlabel(,none) ///
	keep( mainvar ) ///
	b(%8.3f) se(%8.3f) r2(%8.2f)  hlinechar(`=char(151)')
eststo clear
log close



****Figure 2 Pre-Trend****
clear
use "${workingdir}\workdataindi.dta" 

generate_parallel_dummies

local cont male eduy age age_2 is_not_host yearage

foreach var in employment asinh_labor_income asinh_total_income{
	Parallel_Trend, var(`var')
}
eststo clear
clear
use "${workingdir}\workdatahousehold.dta" 

generate_parallel_dummies

local cont is_female_host age_of_host eduy_of_host 

foreach var in log_size_of_f_abo_18 asinh_educ_budget_pstu ///
    asinh_lend_pc asinh_cons_pc asinh_dis_income_pc ///
    co_resid_with_adult_child {
	Parallel_Trend, var(`var')
}
	
eststo clear
clear  
