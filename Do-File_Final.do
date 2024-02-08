
clear all


*** Load the data


insheet using "C:\Users\wmb815\Desktop\BDZV_Liste_Finale_November_29_VerlagsID.txt", tab



*****************************************************************************
*** DATA PREPARATION ********************************************************
*****************************************************************************



*** Collapse the data


keep if version != .  // Drops all lines that do not contain any further information
					  // Note that the file still contains observations on newspapers
					  // that we have dropped in the main analysis due to "attrition".
					  // We need these observations later on for robustness checks!



*** Clean the data


// Drop all variables not needed in the analysis
// to make the dataset more clearly arranged and better readable. 

drop plz kommentar zusage absage warten nichterschienengefolgert nichtaufhomepage 
drop anrufseitensderredaktion realittscheck inhaltlichenachfrage anrufeseitensannamarierichter 
drop artikelangegeben gesendetanwelchelokalausgabe kommentarzuauflage
drop auflageintsdbdzv verlag erscheinungsort
drop v38 v39 v40 v41 v39


// Give the variables more convenient names.

rename (nationalnewspaper jamitid jaohneid neinkondz neinidcheck neinpur auflageintsdfinal bundesland) (national y_id yes n_cond n_id no circulation state)



// Further cleaning

destring circulation, replace dpcomma	// Attention: we have the "German" comma in the Excel-Spreadsheet
destring score, replace dpcomma			// and must replace it with the English comma!

replace national = 0 if missing(national)



*** Generate further variables  


gen print = 1 if y_id ==1 | yes ==1		// Generates a dummy for being printed.
replace print = 0 if missing(print)

gen no_print = 1 if n_cond == 1 | n_id==1 | no == 1		// Generates a dummy for not being printed
replace no_print = 0 if missing(no_print)

gen left = 1 if version == 3 | version == 4		// Generates a dummy for a left letter, i.e.,
replace left = 0 if missing(left)				// generates a dummy for version 3 and 4.

gen neg = 1 if version == 2 | version == 4		// Generates a dummy for a negative letter, i.e.,
replace neg = 0 if missing(neg)					// generates a dummy for version 2 and 4.

gen merkel = 1 if version == 1 | version == 4	// Generates a dummy for a Merkel letter, i.e.,
replace merkel = 0 if missing(merkel)			// generates a dummy for version 1 and 4.


foreach x in 1 2 3 4 {

g v`x' = 1 if version==`x'
replace v`x' = 0 if missing(v`x')

}
// Generates a dummy variable for each version.


// Generate a dummy for each state

tab state, gen(state)
gen land = state == "state"
drop land





label var v1 "Version 1"
label var v2 "Version 2"
label var v3 "Version 3"
label var v4 "Version 4"

label var print "Letter was printed"
label var no_print "Letter was not printed"

label var neg "Negative Letter"
label var left "Left-leaning Letter"
label var merkel "Incumbent Letter"


replace email = 0 if missing(email)


*** Save data

save "C:\Users\wmb815\Desktop\Data_Final.dta", replace



****************************************************************************************************************************************************************
****************************************************************************************************************************************************************
**********************************              ****************************************************************************************************************
**********************************   ANALYSIS   ****************************************************************************************************************
**********************************              ****************************************************************************************************************
****************************************************************************************************************************************************************
****************************************************************************************************************************************************************




********************************************************************************
*** FISHER EXACT TESTS *********************************************************
********************************************************************************


*** Load the data

// If starting from here, load the prepared dataset.


use "C:\Users\wmb815\Desktop\Data_Final.dta", clear




*** Non-parametric tests


* Overview 

tab version 

tab version if print == 1 




* Incumbency Dominance 

tabi 52 55 \ 37 70 		// Left to backslash: Merkel print, Merkel notprint
						// Right to backslash: NotMerkel print, NotMerkel notprint
						// Merkel: Versions 1 and 4,
						// NotMerkel: Versions 2 and 3.


* Negativity Bias

tabi 49 60 \ 40 65		// Left to backslash: Negative print, Negative notprint
						// Right to backslash: NotNegative print, NotNegative notprint
						// Negative: Versions 2 and 4,
						// NotNegative: Versions 1 and 3.

* Political Bias

tabi 44 66 \ 45 59		// Left to backslash: Left print, Left notprint
						// Right to backslash: NotLeft print, NotLeft notprint
						// Left: Versions 3 and 4,
						// NotLeft: Versions 1 and 2.




********************************************************************************
*** REGRESSIONS ****************************************************************
********************************************************************************



*** Load the data

// If starting from here, load the prepared dataset.


use "C:\Users\wmb815\Desktop\Data_Final.dta", clear





***************************
*** Randomization Check ***
***************************


*** Summary Statistics


* Summary Statistics of Circulatiom

su circulation , detail

su circulation if v1 == 1 , detail
su circulation if v2 == 1 , detail
su circulation if v3 == 1 , detail
su circulation if v4 == 1 , detail


* Summary Statistics of Score

su score if , detail

su score if v1 == 1 , detail
su score if v2 == 1 , detail
su score if v3 == 1 , detail
su score if v4 == 1 , detail



* Shares of versions per state

tab version if state == "Baden-Württemberg" 
tab version if state == "Bayern" 
tab version if state == "Berlin / Brandenburg" 
tab version if state == "Bremen / Niedersachsen" 
tab version if state == "Hamburg / Schleswig-Holstein" 
tab version if state == "Hessen" 
tab version if state == "Mecklenburg-Vorpommern" 
tab version if state == "NRW" 
tab version if state == "Rheinland-Pfalz" 
tab version if state == "Saarland" 
tab version if state == "Sachsen" 
tab version if state == "Sachsen-Anhalt" 
tab version if state == "Thüringen" 




*** Regressions


eststo clear



reg circulation left neg merkel, r
eststo

test left = neg = merkel			// F-Test


reg score left neg merkel, r
eststo

test left = neg = merkel			// F-Test


esttab using "RevisionRandomization.tex", r2 se star(* 0.1 ** 0.05 *** 0.01)






***************************
*** Main Regressions*******
***************************


eststo clear



reg print left neg merkel, r				// Main 
eststo

reg print left neg merkel circulation, r	// Control for circulation
eststo

reg print left neg merkel national, r		// Control for national newspapers
eststo



* Put state dummies in and redo  


reg print left neg merkel state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, r
eststo

reg print left neg merkel circulation state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, r
eststo

reg print left neg merkel national state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, r
eststo





esttab, r2 se star(* 0.1 ** 0.05 *** 0.01)

esttab using "RegressionTableMain.tex", r2 se star(* 0.1 ** 0.05 *** 0.01) addn("The dependent variable is a dummy variable equal to 1 if newspaper i has printed the letter. Left Letter is an indicator variable for left-leaning letters, Negative Letter an indicator for negative letters, and Incumbent Letter an indicator for letters on Merkel. Control variables include the quarterly circulation of a newspaper in thousands, a dummy variable equal to 1 if newspaper i is a national newspaper, and state dummies.")





***************************
*** Further Results ******* 
***************************


eststo clear



gen leftscore = score * left		// Generate an Interaction Term between Left and Score



reg print left score leftscore, r
eststo

reg print left neg merkel score leftscore, r
eststo

reg print left neg merkel circulation score leftscore, r
eststo

reg print left neg merkel national score leftscore, r
eststo



esttab, r2 se star(* 0.1 ** 0.05 *** 0.01)

esttab using "RegressionTableScore.tex", r2 se star(* 0.1 ** 0.05 *** 0.01)








************************************************
*** ROBUSTNESS CHECKS **************************
************************************************



*** Load the data

// If starting from here, load the prepared dataset.


use "C:\Users\wmb815\Desktop\Data_Final.dta", clear





***************************
*** Exclude Nationals ***** 
***************************


*** BILD, SZ, FAZ, Welt, taz, Neues Deutschland



* Overview 

tab version if national != 1 & attrition != 1

tab version if print == 1 & national != 1 & attrition != 1



** Fisher Exact tests

// See descriptions above


* Incumbency Dominance 

tabi 51 53 \ 37 67


* Negativity Bias

tabi 49 57 \ 39 63


* Political Bias

tabi 44 64 \ 44 56





***************************
*** Attrition ************* 
***************************




* Overview 

tab version if dead != 1 & other != 1				// Keep observations that were forwarded or
													// contaminated as no_prints. 
													// Dead newspapers and other reasons such as that
													// the website was temporarily not available are 
													// still dropped.

tab version if print == 1 & dead != 1 & other != 1



*** Fisher exact tests including contaminated / forwarded

// See descriptions above



* Incumbency Dominance 

tabi 52 69 \ 37 80


* Negativity Bias

tabi 40 77 \ 49 72


* Political Bias

tabi 44 76 \ 45 73




*** Main Regression including contaminated / forwarded


eststo clear


reg print left neg merkel if dead != 1 & other != 1, r			 
eststo



esttab, r2 se star(* 0.1 ** 0.05 *** 0.01)





**********************************************************************************************************************************************************
*** REVISION *********************************************************************************************************************************************
**********************************************************************************************************************************************************



***************************
*** Separate Regs *********
***************************



eststo clear



qui reg print left,r  				
eststo

qui reg print left circulation, r				
eststo

qui reg print left national, r				
eststo

qui reg print left state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, r
eststo 

qui reg print left circulation state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, r
eststo 

qui reg print left national state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, r
eststo 

esttab using "RevisionSeparateLeft.tex", r2 se star(* 0.1 ** 0.05 *** 0.01) addn("XXX.")



eststo clear



qui reg print neg, r 
eststo

qui reg print neg circulation, r
eststo

qui reg print neg national, r
eststo

qui reg print neg state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, r
eststo 

qui reg print neg circulation state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, r
eststo 

qui reg print neg national state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, r
eststo 


esttab using "RevisionSeparateNegative.tex", r2 se star(* 0.1 ** 0.05 *** 0.01) addn("XXX.")




eststo clear 


qui reg print merkel, r
eststo

qui reg print merkel circulation, r
eststo

qui reg print merkel national, r
eststo

qui reg print merkel state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12,
eststo 

qui reg print merkel circulation state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12,
eststo 

qui reg print merkel national state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12,
eststo 



esttab using "RevisionSeparateMerkel.tex", r2 se star(* 0.1 ** 0.05 *** 0.01) addn("XXX.")




***************************
*** Use classical SEs *****
***************************



eststo clear



qui reg print left neg merkel 				// Main 
eststo

qui reg print left neg merkel circulation	// Control for circulation
eststo

qui reg print left neg merkel national		// Control for national newspapers
eststo


qui reg print left neg merkel state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12 
eststo

qui reg print left neg merkel circulation state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12 
eststo

qui reg print left neg merkel national state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12 
eststo



esttab using "RevisionStandardErrors.tex", r2 se star(* 0.1 ** 0.05 *** 0.01) addn("XXX")




***************************
*** Bootstrap SEs *********
***************************


set seed 28052018


eststo clear



qui reg print left neg merkel, vce(bootstrap) 				// Main 
eststo

qui reg print left neg merkel circulation, vce(bootstrap)	// Control for circulation
eststo

qui reg print left neg merkel national, vce(bootstrap)		// Control for national newspapers
eststo



qui reg print left neg merkel state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, vce(bootstrap) 
eststo

qui reg print left neg merkel circulation state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, vce(bootstrap) 
eststo

qui reg print left neg merkel national state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, vce(bootstrap) 
eststo



esttab, r2 se star(* 0.1 ** 0.05 *** 0.01)


* 100 Reps

eststo clear



qui reg print left neg merkel, vce(bootstrap, reps(100)) 				// Main 
eststo

qui reg print left neg merkel circulation, vce(bootstrap, reps(100))	// Control for circulation
eststo

qui reg print left neg merkel national, vce(bootstrap, reps(100))		// Control for national newspapers
eststo
 


qui reg print left neg merkel state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, vce(bootstrap, reps(100)) 
eststo

qui reg print left neg merkel circulation state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, vce(bootstrap, reps(100)) 
eststo

qui reg print left neg merkel national state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, vce(bootstrap, reps(100)) 
eststo



esttab, r2 se star(* 0.1 ** 0.05 *** 0.01)



* 200 Reps

eststo clear



qui reg print left neg merkel, vce(bootstrap, reps(200)) 				// Main 
eststo

qui reg print left neg merkel circulation, vce(bootstrap, reps(200))	// Control for circulation
eststo

qui reg print left neg merkel national, vce(bootstrap, reps(200))		// Control for national newspapers
eststo




qui reg print left neg merkel state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, vce(bootstrap, reps(200)) 
eststo

qui reg print left neg merkel circulation state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, vce(bootstrap, reps(200)) 
eststo

qui reg print left neg merkel national state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, vce(bootstrap, reps(200)) 
eststo



esttab, r2 se star(* 0.1 ** 0.05 *** 0.01)




***************************
*** Logit Probit **********
***************************


***** Logit

* Main


eststo clear



qui logit print i.left i.neg i.merkel, r							// Main 
eststo
 
margins, dydx(*) atmeans


qui logit print i.left i.neg i.merkel c.circulation, r				// Control for circulation
eststo

margins, dydx(*) atmeans


qui logit print i.left i.neg i.merkel i.national, r					// Control for national newspapers
eststo

margins, dydx(*) atmeans


// Perfect Predictors of failure rausnehmen: State7, State10, State12: Saarland (1obs), Sachsen-Anhalt (2obs), MeckPomm (1 obs)

qui logit print i.left i.neg i.merkel state1 state2 state3 state4 state5 state6 state8 state9 state11 if state7==0 & state10==0 & state12==0, r 
eststo

margins, dydx(*) atmeans


qui logit print i.left i.neg i.merkel c.circulation state1 state2 state3 state4 state5 state6 state8 state9 state11 if state7==0 & state10==0 & state12==0, r 
eststo
 
margins, dydx(*) atmeans


qui logit print i.left i.neg i.merkel i.national state1 state2 state3 state4 state5 state6 state8 state9 state11 if state7==0 & state10==0 & state12==0, r 
eststo

margins, dydx(*) atmeans


esttab using "RevisionLogitProbitMain.tex", r2 se star(* 0.1 ** 0.05 *** 0.01) 




* Further



eststo clear

logit print i.left##c.score , r
eststo
 


logit print i.left##c.score i.neg i.merkel, r
eststo



logit print i.left##c.score i.neg i.merkel c.circulation, r
eststo
 


logit print i.left##c.score i.neg i.merkel i.national, r
eststo
 



esttab using "RevisionLogitProbitFurther.tex", r2 se star(* 0.1 ** 0.05 *** 0.01) addn("XXX.")



*** Compute Margins

su score, detail

egen mean_score = mean(score)
gen mean_score_l = mean_score - 0.007 		// One half SD to the left of mean(score)
gen mean_score_r = mean_score + 0.007		// One half SD


logit print i.left##c.score , r


margins, at(left=(1) score=(-.0069058)) 
margins, at(left=(0) score=(-.0069058)) atmeans
margins, at(left=(1) score=(.0070942)) atmeans
margins, at(left=(0) score=(.0070942)) atmeans





***** Probit // (we don't need this, coded just in case ...)


eststo clear



qui probit print left neg merkel, r								// Main 
eststo

qui probit print left neg merkel circulation, r					// Control for circulation
eststo

qui probit print left neg merkel national, r					// Control for national newspapers
eststo
  


qui probit print left neg merkel state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, r 
eststo

qui probit print left neg merkel circulation state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, r 
eststo

qui probit print left neg merkel national state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, r
eststo



esttab, r2 se star(* 0.1 ** 0.05 *** 0.01)



* Further


gen leftscore = score * left		// Generate an Interaction Term between Left and Score



eststo clear

qui probit print left score leftscore, r
eststo

qui probit print left neg merkel score leftscore, r
eststo

qui probit print left neg merkel circulation score leftscore, r
eststo

qui probit print left neg merkel national score leftscore, r
eststo



esttab, r2 se star(* 0.1 ** 0.05 *** 0.01)



***************************
*** Cluster Verlage *******
***************************



eststo clear



qui reg print left neg merkel, cl(verlagsid)				// Main 
eststo

qui reg print left neg merkel circulation, cl(verlagsid)	// Control for circulation
eststo

qui reg print left neg merkel national, cl(verlagsid)		// Control for national newspapers
eststo



qui reg print left neg merkel state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, cl(verlagsid) 
eststo

qui reg print left neg merkel circulation state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, cl(verlagsid) 
eststo

qui reg print left neg merkel national state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, cl(verlagsid)
eststo


esttab using "RevisionClusterVerlage.tex", r2 se star(* 0.1 ** 0.05 *** 0.01) addn("XXX.")




***************************
*** POWER ANALYSIS ********
***************************

*** Political Bias


// Given the observed publishing proportion in the control group, 
// and given different levels of power, 
// how large would be the minimal detectable ES?


power twoproportions 0.433, test(fisher) n1(104) n2(110) diff(0.00 (0.01) 0.2) alpha(0.05) graph
power twoproportions 0.433, test(fisher) n1(104) n2(110) diff(0.00 (0.01) 0.2) alpha(0.1) graph


// Given the effect size that we observe, 
// and given different levels of power,
// how many observations would we have needed to make it significant?


power twoproportions 0.433 0.4, test(fisher) n(200(5)800) graph
power twoproportions 0.433 0.4, test(fisher) n(200(5)800) alpha(0.1) graph





*** Negativity Bias


// Given the observed publishing proportion in the control group, 
// and given different levels of power, 
// how large would be the minimal detectable ES?

power twoproportions 0.381, test(fisher) n1(109) n2(105) diff(0.00 (0.01) 0.2) graph onesided
power twoproportions 0.381, test(fisher) n1(109) n2(105) diff(0.00 (0.01) 0.2) alpha(0.1) graph onesided



// Given the effect size that we observe, 
// and given different levels of power,
// how many observations would we have needed to make it significant?


power twoproportions 0.381 0.449, test(fisher) n(200(5)800) graph onesided
power twoproportions 0.381 0.449, test(fisher) n(200(5)800) alpha(0.1) graph onesided



*** Incumbency Dominance


// Given the observed publishing proportion in the control group, 
// and given different levels of power, 
// how large would be the minimal detectable ES?

power twoproportions 0.346, test(fisher) n1(107) n2(107) diff(0.00 (0.01) 0.2) graph onesided
power twoproportions 0.346, test(fisher) n1(107) n2(107) diff(0.00 (0.01) 0.2) alpha(0.1) graph onesided



***************************
*** WEIGHT OBSERVATIONS ***
***************************



egen totalcirculation = total(circulation)

gen printcirc = print * circulation
gen printcirc_w = printcirc / totalcirculation



eststo clear


reg printcirc_w left neg merkel, r
eststo

qui reg printcirc_w left neg merkel circulation, r
eststo

qui reg printcirc_w left neg merkel national, r
eststo



qui reg printcirc_w left neg merkel state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, r
eststo

qui reg printcirc_w left neg merkel circulation state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, r 
eststo

qui reg printcirc_w left neg merkel national state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12, r
eststo


esttab using "RevisionWeighted.tex", r2 se star(* 0.1 ** 0.05 *** 0.01) 






*** Weighted Least Squares




eststo clear



qui reg print left neg merkel [aweight = circulation]				// Main 
eststo

qui reg print left neg merkel circulation [aweight = circulation]	// Control for circulation
eststo

qui reg print left neg merkel national [aweight = circulation]		// Control for national newspapers
eststo



qui reg print left neg merkel state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12 [aweight = circulation]
eststo

qui reg print left neg merkel circulation state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12 [aweight = circulation]
eststo

qui reg print left neg merkel national state1 state2 state3 state4 state5 state6 state7 state8 state9 state10 state11 state12 [aweight = circulation] 
eststo



esttab, r2 se star(* 0.1 ** 0.05 *** 0.01)








***************************
*** EMAILs ****************
***************************


su email, detail

tab email print







							
							
							


