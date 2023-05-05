/*STATISTICAL ANALYSIS FOR 

GPs estimations of duration that would be required to deliver BMJ's `ten minute consultations' 

*/

************************************
*1. NUMBER OF TASKS OVER TIME 
************************************

*Plots of tasks per year 
use "C:\~consultations.dta", clear
keep condition pubyear task*
rename task_tot task1
rename task_ess task2
rename task_op task3
reshape long task, i(condition pubyear) j(cat)
label define catlab 1 "All" 2 "Essential" 3 "Additional"
label values cat catlab
twoway scatter task pubyear, by(cat, holes(2) graphregion(c(white)) note(""))  ylabel(0(5)50, labsize(vsmall) angle(0)) ///
ytitle("Number of tasks")   xtitle("Publication Year") 

*TASKS OVER TIME - 5-year periods
use "C:\~consultations.dta", clear
*create 5 year time periods
gen period=1 if pubyear>=2000 & pubyear<=2004
replace period=2 if pubyear>=2005 & pubyear<=2009
replace period=3 if pubyear>=2010 & pubyear<=2014
replace period=4 if pubyear>=2015 & pubyear<=2019
replace period=5 if pubyear>=2020 & pubyear<=2022
label define periodlab 1 "2000-2004" 2 "2005-2009" 3 "2010-2014" 4 "2015-2019" 5 "2020-2022"
label values period periodlab
tab pubyear period

*median and IQR of tasks per period 
bysort period: summ task_tot, d 
bysort period: summ task_ess, d
bysort period: summ task_op, d

*box plot over period
*all tasks 
graph box task_tot, over(period, label(labsize(small))) graphregion(c(white)) ytitle("Number of tasks") ///
 ylabel(0(5)50, labsize(vsmall)) title("Total number of tasks per consultation")
*Essential tasks 
graph box task_ess, over(period, label(labsize(small))) graphregion(c(white)) ytitle("Number of tasks") ///
 ylabel(0(5)50, labsize(vsmall)) title("Total number of essential tasks per consultation")
*Optional tasks 
graph box task_op, over(period, label(labsize(small))) graphregion(c(white)) ytitle("Number of tasks") ///
 ylabel(0(5)50, labsize(vsmall)) title("Total number of additional tasks per consultation")
 

***********************************
*2. INTER- RATER RELIABILITY 
***********************************

use "C:\~consultations.dta", clear
keep condition GP1total GP2total GP3total GP4total GP1_ess GP1_add GP2_ess GP2_add GP3_ess GP3_add GP4_ess GP4_add
rename GP*total tottime*
rename GP*_ess esstime*
rename GP*_add addtime*
reshape long tottime esstime addtime, i(condition) j(GP)

* calcuate ICCs for consultation data 
*total time 
icc tottime condition GP
*essential time
icc esstime condition GP
*additional time 
icc addtime condition GP

*Plots of time for each GP - reshape long to plot
use "C:\~consultations.dta", clear
keep condition pubdate pubyear GP*
rename GP*total tottime*
rename tottime_ tottime5
rename GP*_ess esstime*
rename esstime esstime5
rename GP*_add addtime*
rename addtime addtime5
order condition tottime* esstime* addtime*
reshape long tottime esstime addtime, i(condition  pubdate pubyear) j(GP)
sencode condition, gen(consultation) gsort(pubdate)
label define raterlab 1 "GP 1" 2 "GP 2" 3 "GP 3" 4 "GP 4" 5 "Combined (median)"
label values GP raterlab

*Box plot of distribution by GP 
*total time
graph box tottime, over(GP, label(labsize(small))) graphregion(c(white)) ytitle("Estimated time (mins)") ///
 ylabel(0(20)100) title("Distribution of estimated total time by GP") yline(10)
*essential time 
graph box esstime, over(GP, label(labsize(small))) graphregion(c(white)) ytitle("Estimated time (mins)") ///
 ylabel(0(20)100) title("Distribution of estimated essential time by GP") yline(10)
*additional time
graph box addtime, over(GP, label(labsize(small))) graphregion(c(white)) ytitle("Estimated time (mins)") ///
 ylabel(0(20)100) title("Distribution of estimated additional time by GP") yline(10) 
 
*For each consultaion time by GP and median time    
*total time 
twoway (scatter consultation tottime if GP==1) (scatter consultation tottime if GP==2) (scatter consultation tottime if GP==3) ///
 (scatter consultation tottime if GP==4) (scatter consultation tottime if GP==5), ///
 legend(order(1 "GP 1" 2 "GP 2" 3 "GP 3" 4 "GP 4" 5 "Combined (median)") size(small) rows(1)) ///
 graphregion(c(white)) ytitle("Consultation") xtitle("Estimated time (mins)") ///
 xlabel(0(20)100) title("Estimated total time by consultation") xline(10) ///
 ylabel(1(1)44,valuelabel angle(0) labsize(tiny))

*essential time  
twoway (scatter consultation esstime if GP==1) (scatter consultation esstime if GP==2) (scatter consultation esstime if GP==3) ///
 (scatter consultation esstime if GP==4) (scatter consultation esstime if GP==5), ///
 legend(order(1 "GP 1" 2 "GP 2" 3 "GP 3" 4 "GP 4" 5 "Combined (median)") size(small) rows(1)) ///
 graphregion(c(white)) ytitle("Consultation") xtitle("Estimated time (mins)") ///
 xlabel(0(20)100) title("Estimated essential time by consultation") xline(10) ///
 ylabel(1(1)44,valuelabel angle(0) labsize(tiny))

*additional time  
twoway (scatter consultation addtime if GP==1) (scatter consultation addtime if GP==2) (scatter consultation addtime if GP==3) ///
 (scatter consultation addtime if GP==4) (scatter consultation addtime if GP==5), ///
 legend(order(1 "GP 1" 2 "GP 2" 3 "GP 3" 4 "GP 4" 5 "Combined (median)") size(small) rows(1)) ///
 graphregion(c(white)) ytitle("Consultation") xtitle("Estimated time (mins)") ///
 xlabel(0(20)100) title("Estimated additional time by consultation") xline(10) ///
 ylabel(1(1)44,valuelabel angle(0) labsize(tiny))
 

**********************************************
*3. TRENDS  IN CONSULTATION TIME OVER TIME
**********************************************

use "C:\~consultations.dta", clear
*create 5 year time periods
gen period=1 if pubyear>=2000 & pubyear<=2004
replace period=2 if pubyear>=2005 & pubyear<=2009
replace period=3 if pubyear>=2010 & pubyear<=2014
replace period=4 if pubyear>=2015 & pubyear<=2019
replace period=5 if pubyear>=2020 & pubyear<=2022
label define periodlab 1 "2000-2004" 2 "2005-2009" 3 "2010-2014" 4 "2015-2019" 5 "2020-2022"
label values period periodlab

/*add additional time for rotine tasks 
•       Preparing to see patients 1 minute 17 seconds 
•       Calling in patients 44 seconds
•       Documentation in electronic health record 1 minute 40 seconds 
Total of 221 seconds = 3.7 mins*/
gen cons_total= GP_total+3.7
gen cons_ess = GP_ess + 3.7
gen cons_add = GP_add

*median time per period 
bysort period: summ cons_total, d 
bysort period: summ cons_ess, d
bysort period: summ cons_add, d

*box plot over period
*total time 
graph box cons_total, over(period, label(labsize(small))) graphregion(c(white)) ytitle("Consultation time") ///
 ylabel(0(10)60, labsize(vsmall)) title("Total consultation time by period")  yline(10)
 
*Essential tasks time
graph box cons_ess, over(period, label(labsize(small))) graphregion(c(white)) ytitle("Consultation time") ///
 ylabel(0(10)60, labsize(vsmall)) title("Consultation time by period" "Essential tasks")  yline(10)

*Additional tasks time
graph box cons_add, over(period, label(labsize(small))) graphregion(c(white)) ytitle("Consultation time") ///
 ylabel(0(10)60, labsize(vsmall)) title("Consultation time by period" "Additional tasks")  yline(10)

*Plots of time per year - includes regression line 
twoway (scatter cons_total pubyear) (lfit cons_total pubyear, lc(green)),  ///
 graphregion(c(white)) ylabel(0(5)60, labsize(vsmall) angle(0)) ///
ytitle("Consultation time")   xtitle("Publication Year") yline(10) ///
 legend(order(1 "Median time" 2 "Fitted line") size(small) rows(1)) 

*Plots of time per year - no regression line
twoway (scatter cons_total pubyear) ,  ///
 graphregion(c(white)) ylabel(0(5)60, labsize(vsmall) angle(0)) ///
ytitle("Consultation time")   xtitle("Publication Year") yline(10) ///
 legend(order(1 "Median time" 2 "Fitted line") size(small) rows(1)) 

 
*regression model 
*recode year so 2000 is baseline (=1)
gen year=pubyear-1999

*total time
*linear regression - year
regress cons_total year
*regression - period
regress cons_total i.period

*essential time
* regression - year
regress cons_ess year
* regression - period categorical 
regress cons_ess i.period

*additional time
regress cons_add year
* regression - period categorical 
regress cons_add i.period

* excluding the added on times for not incuded tasks (3.7 mins - preparing for patients etc)
*median time per period (total and essential tasks only)
bysort period: summ GP_total, d 
bysort period: summ GP_ess, d

*box plot over period
*total time 
graph box GP_total, over(period, label(labsize(small))) graphregion(c(white)) ytitle("Consultation time") ///
 ylabel(0(10)60, labsize(vsmall)) title("Total consultation time by period")  yline(10)
 
*Essential tasks time
graph box GP_ess, over(period, label(labsize(small))) graphregion(c(white)) ytitle("Consultation time") ///
 ylabel(0(10)60, labsize(vsmall)) title("Consultation time by period" "Essential tasks")  yline(10)

*Plots of time per year 
twoway (scatter GP_total pubyear) ,  ///
 graphregion(c(white)) ylabel(0(5)60, labsize(vsmall) angle(0)) ///
ytitle("Consultation time")   xtitle("Publication Year") yline(10) ///
 legend(order(1 "Median time" 2 "Fitted line") size(small) rows(1)) 
