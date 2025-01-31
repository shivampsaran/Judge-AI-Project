* if not already done: cd to the folder where this script is stored, which should have a "data" subfolder with the data and another subfolder "output" to store the results
* requires user-written packages cibar, grc1leg2, grstyle, sadi, sq, colorpalette 

clear all

/****************************************************************************
******************************************************************************
	I. DOCUMENT VIEWS
******************************************************************************
****************************************************************************/

use data\studentjudge_sequence.dta, clear
drop if precedent==3 // Besic/REVERSE was only used with students
drop precedent
egen id = group(ID type) // some IDs were used for some judge AND for some student
sqset doc id eventtime

/***********************************************************
	Fig. 1
************************************************************/

grstyle init
grstyle set plain, nogrid
grstyle set color cblind
forvalues t=1/2 { // NB: I tried using a different ordering of sequences (edit distance clustered via single or Wards linkage, as Halpin recommends): it's inintelligble
	preserve
	keep if type==`t'
	assert inrange(doctime,3,55) // 3 was the cutoff used in the ingestion script. Two obs between 3 and 5 actually did consult multiple documents ...
	sqindexplot, scheme(_GRSTYLE_) rbar plotregion(margin(zero)) nodraw ///
		title("`: label type `t''s") xtitle("Normalized Time") xlabel(25 "start" 470 "finish", labsize(*1) notick) ytitle("Participant") ylabel(minmax) name(p`t', replace) ///
		legend(rows(1) size(*.73) symxsize(*.5)) // order(2 "facts" 1 "briefs" 6 "trial judgment" 5 "statute" 4 "precedent") doesn't work after upgrade to Stata 17 ...
	restore
}
grc1leg2 p1 p2, scheme(_GRSTYLE_) xcommon rows(1) imargin(tiny) graphregion(margin(r+8)) xtob1title ytol1title ring(1) ///
	title("Fig. 1: Document View Paths by Participant Type") ///
	note("By participant type (plot), the graph shows for each participant (vertical axis) the documents live on that participant's screen from start to finish (horizontal axis).", size(*.63))
graph save output\sequences, replace


/***********************************************************
	Type Comparisons Using Edit Distance
***********************************************************/

preserve
reshape wide doc, i(type id doctime) j(eventtime)
rename doctime time // to avoid conflict with "doc" stem names used by oma
sort id

* unrestricted -- all obs
foreach subscost in 1 1.5 { // 1 is Levenshtein
	di _n "*** Substitution cost `subscost' ***"
	matrix scost = `subscost'*(J(6,6,1) - I(6))
	oma doc*, subsmat(scost) indel(1) length(500) pwdist(ED) dups
	sddiscrep type, dist(ED) id(id) niter(100000)
}

* restricted -- only obs with >= 5 minutes in the documents (as in graph)
keep if inrange(time,5,55) // there are no obs with doctime>55)
drop time
foreach subscost in 1 1.5 { // 1 is Levenshtein
	di _n "*** Substitution cost `subscost' ***"
	matrix scost = `subscost'*(J(6,6,1) - I(6))
	oma doc*, subsmat(scost) indel(1) length(500) pwdist(ED) dups
	sddiscrep type, dist(ED) id(id) niter(100000)
}

* sanity check -- random data
di "sanity check -- random data"
gen random = .
forvalues i=1/10 {
	replace random = runiform()
	sort random
	replace random = _n<33 // creates binary indicator for first 32 random obs (same N as judges) and rest (students)
	sort id
	matrix scost = (J(6,6,1) - I(6))
	oma doc*, subsmat(scost) indel(1) length(500) pwdist(ED) dups
	sddiscrep random, dist(ED) id(id) niter(1000)
}

restore

/***********************************************************
	Summary statistics, simple comparisons
************************************************************/
contract type id doctime doc, freq(t_)
replace t_=t_/500
decode doc, gen(document)
drop doc
reshape wide t_, i(type id doctime) j(document) string
egen total = rowtotal(t_*)
assert round(total,.000001)==1
recode t_* (. = 0)

table () type, statistic(mean t_trial t_precedent t_statute) nformat(%4.2f) nototals
manova t_briefs t_facts t_precedent t_statute t_trial = type

keep if inrange(doctime,5,55)
table () type, statistic(mean t_trial t_precedent t_statute) nformat(%4.2f) nototals
manova t_briefs t_facts t_precedent t_statute t_trial = type