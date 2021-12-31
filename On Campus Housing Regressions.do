***************************************
***************************************
*****ON CAMPUS HOUSING REGRESSIONS*****
***************************************
***************************************

clear
set more off
set excelxlsxlargefile on


***********
***INTRO***
***********
*This script runs the OLS regressions for the on-campus housing data. This aims to identify
*the effect of living on campus on student retention and graduation rates.


************
***IMPORT***
************
/*HUD DATA*/
*import the fair market housing data. This data provides the safmr2br variable which provides
*the average rental price of a 2 bedroom apartment by zip code. We will use this as an
*instrument later on
import delimited "$intermed/HUD/Fair Market Housing Data.csv", varnames(1) case(lower) bindquotes(strict) clear

*add a year to the fiscal year so that the years line up with the academic years. This is because
*in a case like fy2003, this is actually 2003-2004, and the academic year would have started in fall 2004.
gen academic_year = fiscal_year + 1

*keep the vars we need for the merge
keep zip academic_year safmr2br

isid zip academic_year

rename safmr2br sa_fmr_2br

*save
compress
save "$intermed/HUD/Fair Market Housing Data.dta", replace

/*ON CAMPUS HOUSING REQUIREMENTS*/
import delimited "$intermed/On-Campus Housing Requirements.csv", case(lower) clear

rename year academic_year

drop if housing_req == .
display in red "**********DELETES 0 OBSERVATIONS**********"

compress
save "$intermed/Top 100 University On-Campus Housing Requirements.dta", replace

/*IPEDS DATA*/
*import the cleaned IPEDS data. This is output from R and should be in good shape
import delimited "$intermed/IPEDS/IPEDS Combined Data.csv", varnames(1) case(lower) clear

*cleanup from R output
foreach var in roomamt boardamt rmbrdamt efalevel1 hospital_expenses independent_expenses ///
	ret_pcf grtype8 grtype9 grad_rate_150 {
	replace `var' = "" if `var' == "NA"
	destring `var', replace
}

*rename for ease
rename efalevel2 total_undergrads

*create a room_board cost equal to the room amount plus the board amount
gen room_board_cost = rmbrdamt
replace room_board_cost = roomamt + boardamt if room_board_cost == .

*compute the bed-enrollment ratio. This is just the ratio of the number of beds available
*on campus (roomcap from HD data) to the total undergraduate enrollment (efalevel2 from EFA data).
*Notably, roomcap includes graduate housing. This implies that if a university has far more graduate
*students than undergrads and has additional housing for those graduate students, then this ratio
*could be skewed too high. I will test this with efalevel1 as well which includes all students.
gen bed_enrollment_ratio = roomcap/total_undergrads

*create a 5 digit zip code for each observation that we can use to merge in the next step
gen zip_cleaned = zip
replace zip_cleaned = ustrleft(zip, 5) if ustrlen(zip_cleaned) > 5

compress
save "$intermed/IPEDS/IPEDS Combined Data.dta", replace


***********
***MERGE***
***********
use "$intermed/IPEDS/IPEDS Combined Data.dta", clear

/*HUD DATA*/
*cleanup and keep the new zip code variable you created
drop zip
rename zip_cleaned zip

*merge with the housing data
merge m:1 zip academic_year using "$intermed/HUD/Fair Market Housing Data.dta"

drop if _merge == 2
display in red "**********DELETES 404,094 OBSERVATIONS**********"

*cleanup
drop _merge

/*ON-CAMPUS HOUSING REQUIREMENTS*/
*merge in the housing requirements of the top 100 universities to
*determine how the housing requirement affects students
merge m:1 unitid academic_year using "$intermed/Top 100 University On-Campus Housing Requirements.dta"

keep if _merge == 3
display in red "**********DELETES 26,193 OBSERVATIONS**********"

drop _merge

*create a private indicator
gen private = 0
replace private = 1 if school_type == "private"


*********************
***OLS REGRESSIONS***
*********************
/*PREPARE FOR REGRESSIONS*/
*Create spending variables. These are rolling 4-year expenses for each cohort
sort unitid academic_year

gen stu_spending_roll = (student_services_expenses[_n-3] + student_services_expenses[_n-2] + student_services_expenses[_n-1] + ///
	student_services_expenses) if unitid == unitid[_n-3] & unitid == unitid[_n-2] & unitid == unitid[_n-1] & ///
	academic_year == (academic_year[_n-3] + 3) & (academic_year == academic_year[_n-2] + 2) & (academic_year == academic_year[_n-1] + 1)

gen aux_spending_roll = (auxiliary_expenses[_n-3] + auxiliary_expenses[_n-2] + auxiliary_expenses[_n-1] + ///
	auxiliary_expenses) if unitid == unitid[_n-3] & unitid == unitid[_n-2] & unitid == unitid[_n-1] & ///
	academic_year == (academic_year[_n-3] + 3) & (academic_year == academic_year[_n-2] + 2) & (academic_year == academic_year[_n-1] + 1)
	
gen ed_spending_roll = (instruction_expenses[_n-3] + instruction_expenses[_n-2] + instruction_expenses[_n-1] + ///
	instruction_expenses) if unitid == unitid[_n-3] & unitid == unitid[_n-2] & unitid == unitid[_n-1] & ///
	academic_year == (academic_year[_n-3] + 3) & (academic_year == academic_year[_n-2] + 2) & (academic_year == academic_year[_n-1] + 1)

*take the log of the spending variables that you just created, as well as other variables
*of interest
gen ln_stu_spending_roll = ln(stu_spending_roll)
gen ln_aux_spending_roll = ln(aux_spending_roll)
gen ln_ed_spending_roll = ln(ed_spending_roll)
gen ln_grad_rate = ln(grad_rate_150)
gen ln_retention_rate = ln(ret_pcf)
gen ln_total_undergrads = ln(total_undergrads)
gen ln_safmr2br = ln(sa_fmr_2br)

*keep the vars we need for the regressions
keep unitid academic_year ln_grad_rate ln_retention_rate housing_req private bed_enrollment_ratio ln_aux_spending_roll ln_stu_spending_roll ln_ed_spending_roll ln_total_undergrads ln_safmr2br

*export the university-year pairs so that you can import these and use them when creating summary
*statistics for the right subset of observations
preserve
	keep unitid academic_year
	duplicates drop
	display in red "**********DELETES 0 OBSERVATIONS**********"
	
	gen flag = 1
	
	compress
	export delimited "$intermed/IPEDS/IPEDS On-Campus University-Year Pairs.csv", replace
restore


/*RUN REGRESSIONS*/
*retention rate regression 1: nothing
regress ln_retention_rate housing_req, robust
outreg2 using "$output/IPEDS Regressions/IPEDS Retention-Housing Req Regressions", keep(housing_req) tex replace

*retention rate regression 2: expenditure vars
regress ln_retention_rate housing_req ln_aux_spending_roll ln_stu_spending_roll ln_ed_spending_roll, robust
outreg2 using "$output/IPEDS Regressions/IPEDS Retention-Housing Req Regressions", keep(housing_req ln_aux_spending_roll ln_stu_spending_roll ln_ed_spending_roll) tex

*retention rate regression 3: university chars
regress ln_retention_rate housing_req ln_aux_spending_roll ln_stu_spending_roll ln_ed_spending_roll private bed_enrollment_ratio ln_total_undergrads ln_safmr2br, robust
outreg2 using "$output/IPEDS Regressions/IPEDS Retention-Housing Req Regressions", keep(housing_req ln_aux_spending_roll ln_stu_spending_roll ln_ed_spending_roll private bed_enrollment_ratio ln_total_undergrads ln_safmr2br) tex

*retention rate regression 4: university fe
xi: regress ln_retention_rate housing_req ln_aux_spending_roll ln_stu_spending_roll ln_ed_spending_roll private bed_enrollment_ratio ln_total_undergrads ln_safmr2br i.unitid, robust
outreg2 using "$output/IPEDS Regressions/IPEDS Retention-Housing Req Regressions", keep(housing_req ln_aux_spending_roll ln_stu_spending_roll ln_ed_spending_roll private bed_enrollment_ratio ln_total_undergrads ln_safmr2br) tex

*retention rate regression 5: university and year fe
xi: regress ln_retention_rate housing_req ln_aux_spending_roll ln_stu_spending_roll ln_ed_spending_roll private bed_enrollment_ratio ln_total_undergrads ln_safmr2br i.unitid i.academic_year, robust
outreg2 using "$output/IPEDS Regressions/IPEDS Retention-Housing Req Regressions", keep(housing_req ln_aux_spending_roll ln_stu_spending_roll ln_ed_spending_roll private bed_enrollment_ratio ln_total_undergrads ln_safmr2br) tex


*grad rate regression 1: nothing
regress ln_grad_rate housing_req, robust
outreg2 using "$output/IPEDS Regressions/IPEDS Graduation-Housing Req Regressions", keep(housing_req) tex replace

*grad rate regression 2: expenditure vars
regress ln_grad_rate housing_req ln_aux_spending_roll ln_stu_spending_roll ln_ed_spending_roll, robust
outreg2 using "$output/IPEDS Regressions/IPEDS Graduation-Housing Req Regressions", keep(housing_req ln_aux_spending_roll ln_stu_spending_roll ln_ed_spending_roll) tex

*grad rate regression 3: university chars
regress ln_grad_rate housing_req ln_aux_spending_roll ln_stu_spending_roll ln_ed_spending_roll private bed_enrollment_ratio ln_total_undergrads ln_safmr2br, robust
outreg2 using "$output/IPEDS Regressions/IPEDS Graduation-Housing Req Regressions", keep(housing_req ln_aux_spending_roll ln_stu_spending_roll ln_ed_spending_roll private bed_enrollment_ratio ln_total_undergrads ln_safmr2br) tex

*grad rate regression 4: university fe
xi: regress ln_grad_rate housing_req ln_aux_spending_roll ln_stu_spending_roll ln_ed_spending_roll private bed_enrollment_ratio ln_total_undergrads ln_safmr2br i.unitid, robust
outreg2 using "$output/IPEDS Regressions/IPEDS Graduation-Housing Req Regressions", keep(housing_req ln_aux_spending_roll ln_stu_spending_roll ln_ed_spending_roll private bed_enrollment_ratio ln_total_undergrads ln_safmr2br) tex

*grad rate regression 5: university and year fe
xi: regress ln_grad_rate housing_req ln_aux_spending_roll ln_stu_spending_roll ln_ed_spending_roll private bed_enrollment_ratio ln_total_undergrads ln_safmr2br i.unitid i.academic_year, robust
outreg2 using "$output/IPEDS Regressions/IPEDS Graduation-Housing Req Regressions", keep(housing_req ln_aux_spending_roll ln_stu_spending_roll ln_ed_spending_roll private bed_enrollment_ratio ln_total_undergrads ln_safmr2br) tex


