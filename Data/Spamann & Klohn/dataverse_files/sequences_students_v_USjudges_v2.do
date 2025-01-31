* if not already done: cd to the folder where this script is stored, which should have a "data" subfolder with the data and another subfolder "output" to store the results
* requires user-written packages cibar, grc1leg2, grstyle, sadi, sq, colorpalette 

clear all

/*****************************************************************************
	CONTENTS
	0. Helper programs
	I. Summary statistics and simple comparisons
	II. Figure 1
	III. Formal tests of group differences between sequences (Levenshtein pseudo R^2)
	IV. Treatment effects for students matched to judges based on sequence
****************************************************************************/

/****************************************************************************
******************************************************************************
	0. Helper programs defined
******************************************************************************
****************************************************************************/

program prepseq // loads the sequence data and does minimal cleaning
	use data\studentjudge_sequence.dta, clear
	drop if precedent==3 // Besic/REVERSE was only used with students
	drop precedent
	egen id = group(ID type) // some IDs were used for some judge AND for some student
	sqset doc id eventtime
	end
	
program wide // reshapes wide
	reshape wide doc, i(type ID id doctime) j(eventtime)
	rename doctime time // to avoid conflict with "doc" stem names used by oma
	sort id
	end

	
/****************************************************************************
******************************************************************************
	I. Summary statistics, simple comparisons
******************************************************************************
****************************************************************************/

use data\studentjudge_sequence.dta, clear
contract type ID doctime doc, freq(t_)
replace t_=t_/500
decode doc, gen(document)
drop doc
reshape wide t_, i(type ID doctime) j(document) string
egen total = rowtotal(t_*)
assert round(total,.000001)==1
recode t_* (. = 0)

table () type, statistic(mean t_trial t_precedent t_statute) nformat(%4.2f) nototals
manova t_briefs t_facts t_precedent t_statute t_trial = type

keep if inrange(doctime,5,55)
table () type, statistic(mean t_trial t_precedent t_statute) nformat(%4.2f) nototals
manova t_briefs t_facts t_precedent t_statute t_trial = type


/****************************************************************************
******************************************************************************
	II. Fig. 1
******************************************************************************
****************************************************************************/

prepseq

grstyle init
grstyle set plain, nogrid
grstyle set color cblind
local common "scheme(_GRSTYLE_) plotregion(margin(zero)) ylabel(minmax) legend(rows(1) size(*.67) symxsize(*.3) symysize(*.8))"

sqindexplot, by(type, yrescale title("a. Individual Sequences", size(*.8)) note("")) ///
	rbar xtitle("") ytitle("Participant") xlabel(none) `common' ///
	name(individual, replace)

wide
sdchronogram doc1-doc500, by(type, imargin(medium) title("b. State Distribution", size(*.8)) note("") noixtick) subtitle("") ///
	proportional xtitle("Normalized Time") ytitle("Proportion")  xlabel(25 "start" 470 "finish", labsize(*1)) `common' ///
	name(proportion, replace)
	
grc1leg2 individual proportion, scheme(_GRSTYLE_) ysize(6) xcommon cols(1) imargin(tiny) graphregion(margin(r+8)) ///
	title("Fig. 1: Document View Paths by Participant Type", size(*.85)) ///
	note("By participant type, the graph shows (a) individual sequences (stacked vertically) and (b) the" "state distribution of documents live on participants' screen from start to finish (horizontal axis).", size(*.7))

graph save output\sequences, replace



/****************************************************************************
******************************************************************************
	III. Formal tests for group differences between sequences (edit distance pseudo R^2)
******************************************************************************
****************************************************************************/

prepseq
wide

* unrestricted -- all obs
foreach subscost in 1 1.5 { // 1 is Levenshtein
	di _n "*** Substitution cost `subscost' ***"
	matrix scost = `subscost'*(J(6,6,1) - I(6))
	oma doc*, subsmat(scost) indel(1) length(500) pwdist(ED) dups
	sddiscrep type, dist(ED) id(id) niter(100000)
}

* restricted -- only obs with >= 5 minutes in the documents (as in graph)
list type if time<5 // two students
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


/****************************************************************************
******************************************************************************
	IV. Are treatment effects more similar for students matched to judges on sequence?
******************************************************************************
****************************************************************************/

prepseq
wide
	
sort type // "oma" below uses the sort order, and I want to distinguish judges from students -- judges will be at top
gen match_id = _n // I will need this to recover the student's participant ID from the sort order
preserve // save the students' participant IDs for later use
	tempfile student_ids
	keep match_id ID type
	keep if type=="student":type
	save `student_ids'
restore

matrix scost = (J(6,6,1) - I(6)) // Levenshtein substitution matrix
oma doc*, subsmat(scost) indel(1) length(500) pwdist(ED) dups // calculates the pairwise Levenshtein distances
svmat ED // appends the distance matrix to the dataset

count if type=="judge":type
local judges = r(N)
local total = _N
drop ED1-ED`judges' // drops distances from judges
drop if type=="student":type // drop students -- we are trying to find matches for judges, not the other way around
preserve // store judge_ids to append to ids of matching students later
	tempfile judge_ids
	keep type ID
	save `judge_ids'
restore

egen mindist = rowmin(ED*) // finds the minimum distance
forvalues j=`=`judges'+1'/`total' { // finds the student match_id belonging to that minimum distance
	replace match_id = `j' if mindist==ED`j'
	}
	
keep match_id
assert match_id<.
merge m:1 match_id using `student_ids', keep(1 3) nogenerate
drop match_id
append using `judge_ids'
decode ID, gen(randomID)
merge m:1 type randomID using data\studentjudge_maindata.dta, keep(1 3) assert(2 3)
codebook randomID if type=="student":type // 21 different students used


bys type: tab guilty precedent, exact
bys type: tab guilty nationality, exact

gen student = type=="student":type
gen unsympathetic=nat=="unsympathetic":nationality
gen studentXunsympa = student*unsympa
gen reverse = precedent==2
gen studentXreverse = student*reverse
eststo exact: exlogistic guilty student studentXunsympa studentXreverse, condvars(unsympa reverse) memory(2g) nolog level(95) terms(t1= studentXunsympa studentXreverse)
estimates replay, test(score)