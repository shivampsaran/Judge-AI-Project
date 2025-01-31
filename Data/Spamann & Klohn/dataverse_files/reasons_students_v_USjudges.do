* if not already done: cd to the folder where this script is stored, which should have a "data" subfolder with the data and another subfolder "output" to store the results
* requires user-written packages cibar and grstyle


******************************************
*** judgment reasons *********************

use mentioned_* judgmentreasons type using data\studentjudge_maindata.dta, clear
keep if !mi(judgmentreasons)
rename mentioned_* *
local elements "Statute Policy Precedent" 
rename (statute policy precedent) (`elements')
gen words = wordcount(judgmentreasons)
bys type: egen cwords = mean(words)
foreach element in `elements' {
	gen `element'_sc = `element'/cwords // this scales down a mention by the average length of reasons for that type (student or judge)
	local elements_sc "`elements_sc' `element'_sc"
	}

* importance of scaling
sum words
tabstat words, by(type) format(%6.2f)

/***********************************************************
	Figure
************************************************************/
grstyle init
grstyle set plain, nogrid
grstyle set color cblind
local emdash = ustrunescape("\u2E3B")
label list type // to check if next line is correct!
local typelabels `"xlabel(1 "judge" 2 "student" , labsize(*3))"'
local emptylabels `"xlabel(1 " " 2 " ")"' // this is a hack to prevent the width of the bars to differ in top and bottom panel -- didn't happen in Stata 16
local cibaropts "over(type) vce(bootstrap) ciopts(lwidth(*8))"
local gphopts "scheme(_GRSTYLE_) ytitle("") xtitle("") nodraw legend(off)"
cibar Statute,		`cibaropts' graphopts(`gphopts' ylabel(, labsize(*1.5))	`emptylabels' name(Statute	 , replace) title("Statute",   size(*2.5)))
cibar Policy,		`cibaropts' graphopts(`gphopts' ylabel(,notick nolabel)	`emptylabels' name(Policy	 , replace) title("Policy",	   size(*2.5)))
cibar Precedent,	`cibaropts' graphopts(`gphopts' ylabel(,notick nolabel)	`emptylabels' name(Precedent   , replace) title("Precedent", size(*2.5)))
cibar Statute_sc,	`cibaropts' graphopts(`gphopts' ylabel(, labsize(*1.5))	`typelabels' name(Statute_sc  , replace) title(""))
cibar Policy_sc,	`cibaropts' graphopts(`gphopts' ylabel(,notick nolabel)	`typelabels' name(Policy_sc   , replace) title(""))
cibar Precedent_sc,	`cibaropts' graphopts(`gphopts' ylabel(,notick nolabel)	`typelabels' name(Precedent_sc, replace) title(""))
graph combine Statute Policy Precedent,			 scheme(_GRSTYLE_) ycommon xcommon altshrink rows(1) name(raw, replace) 
graph combine Statute_sc Policy_sc Precedent_sc, scheme(_GRSTYLE_) ycommon xcommon altshrink rows(1) name(sc, replace)  title("{it:`emdash'Scaled by Average Length (Words)`emdash'}", size(*2))
graph combine raw sc, scheme(_GRSTYLE_) xcommon altshrink rows(2) title("Fig. 2: Prevalence of Reasons by Type") ///
	note("Means and 95% bootstrap confidence intervals of mentions of specified reasons by individual participants." ///
	"In the upper panel, the mean is taken over indicators whether an individual participant mentioned the feature" ///
	"in the reasons. In the lower panel, the type mean is divided by the average number of words in reasons by" ///
	"that type.")
graph save output\reasons.gph, replace


/***********************************************************
	Tests
************************************************************/
* element by element
foreach element in `elements' { 
	tab type `element' if inlist(`element',0,1), exact // classic Fisher: need to exclude the obs with coder disagreement; doesn't work for non-0/1 _sc
	tab type `element', exact // generalized Fisher does work with three options
	ttest `element', by(type) // to get the difference in means
	ttest `element'_sc, by(type) unequal
}

manova `elements' = type // to get the value of Pillai's trace etc.
permute type (e(stat_m)[2,1]), reps(10000) dots(100): manova `elements'	  = type // type test: permute Pillai's trace
manova `elements_sc' = type
permute type (e(stat_m)[2,1]), reps(10000) dots(100): manova `elements_sc' = type