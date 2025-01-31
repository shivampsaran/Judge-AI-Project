* if not already done: cd to the folder where this script is stored, which should have a "data" subfolder with the data and another subfolder "output" to store the results

local personal "off" // if you have access to judges' personal info under NDA with us, switch this to "on" -- otherwise, don't do it or else you will overwite the summary demographics provided!

clear all
tempfile participations transcriptions judgedemographics endtime sequence docviews


/********************************************************************
	I. MAIN PARTICIPATION DATA -- Ingest and clean
*********************************************************************/

* NB: judges' main data already exist in clean form from Spamann & Kloehn (2016) (FJC_April2015_nopersonal.dta, provided as part of this replication package) (minus coded judgment reasons) but recreated here to verify/document that judges' and students' data were identically processed for this paper -- see long footnote in paper for explanation

***** ingestion *****

* judges' handwritten reasons
import delimited data\raw\FJC_20150414_transcribed_judgmentreasons_all.csv, bindquote(strict) encoding("utf-8") varnames(1) clear
drop if mi(judgmentpapermatchingid)
save `transcriptions'

* judges & 1st student round
import delimited data\raw\participations_2015-04-26T03-24-48+00-00_decrypted_noprof.csv, bindquote(strict) encoding("utf-8") varnames(1) clear // students 1; same file as without _noprof suffix (i.e., raw file produced by experimental software) but removed exit question on which crim prof they had to abide by school's request not to be identified
drop exitquestionsquestion5-exitquestionsquestion8 // 7 would be professor (which would render school identifiable), 5-6 and 8 are judges only
rename (exitquestionsquestion9 exitquestionsquestion10) (gender_students tech_difficulties)
save `participations'

if "`personal'"!="on" local suffix "_nopers" // _nopers is same as original but with entries for age, gender, and prior experience as prosecutor or public defender deleted
import delimited data\raw\Judges_participations_2015-04-15T06-56-03+00-00_decrypted`suffix'.csv, bindquote(strict) encoding(UTF-8) delimiters(",") varnames(1) clear  // original file
drop exitquestionsquestion7 exitquestionsquestion10 reasonfor // not asked to judges
drop if mi(timestampstep4finished) & mi(judgmentpapermatchingid) // the 2nd var is necessary for merge in next line; 1st var is necessary for obs, see below
merge 1:1 judgmentpapermatchingid using `transcriptions', update nogenerate assert(1 3 4)
rename (exitquestionsquestion5 exitquestionsquestion6 exitquestionsquestion8 exitquestionsquestion9) (ever_prosecutor ever_defender age gender)
preserve // next four lines will be pointless if personal data isn't available: "judgedemographics" will only contain missing values
	keep random ever_prosecutor ever_defender age gender
	rename random randomID
	compress
	save `judgedemographics'
restore
drop ever_prosecutor ever_defender age gender

append using `participations', gen(type)
recode type (0=1) (1=2)
label define type 1 "judge" 2 "student"
label values type type
label var type "judge or student"
rename exitquestionsquestion1-exitquestionsquestion4 (confidence sentence know_ICL recognize_ )
save `participations', replace
	
* students: 2nd round -- separate because we changed sentence question (see below) and data field names after first judges' and students' rounds
import delimited data\raw\participations_2016-02-15T11-56-42+00-00_decrypted.csv, bindquote(strict) encoding("utf-8") varnames(1) clear
drop exitquestion5everaprosecutorjudg exitquestion6everacriminaldefens exitquestion8agejudgeonly
rename exitquestion* (confidence sentence_string know_ICL recognize_ gender_students classyear)

* combine
append using `participations', gen(studentround)
recode studentround (0 = 2)
label var studentround "student round"
replace studentround = . if studentround==1 & type=="judge":type
assert mi(type) if studentround==2
replace type="student":type if studentround==2

* clean up
rename (random nationality precedent judgmentguilty primings anchor) (randomID nat_ prec_  guilty prime anchor) // NB: some vars like randomID have longer names, hence renaming


****** encode and generate variables ******

* encode treatment variables
label define nationality 1 "croatian" 2 "serbian"
label define precedent 1 "sainovic" 2 "vasiljevic" 3 "besic"
encode nat_, gen(def_nationality) label(nationality)
encode prec_, gen(precedent) label(precedent)
drop nat_ prec_
label define nationality 1 "sympathetic" 2 "unsympathetic", modify

* time
local t=1
foreach step in start consent "instructions read" "documents finished" "judgment rendered" exit {
	gen double time`t' = clock(timestampstep`t',"YMD#hms#") // if type=="student":type 
	format time`t' %tc
	label var time`t++' "time when clicked: `step'"
	}
drop timestampstep*

forvalues t=1/5 {
	count if mi(time`t') & !mi(time`=`t'+1')
	assert r(N)<3
	assert mi(guilty) if mi(time`t') & !mi(time`=`t'+1')
	}

* sentence
* student round 2: there we asked to write out "years" or "months"
drop if regexm(sentence_string,"To avoid misunderstandings, please write the time units \(years or months\) explicitly. ") // one student copied (?) the instructions ... -- sign of ...?
gen years  = ustrregexs(1) if ustrregexm(sentence_string,"([0-9]+(\.[0-9])?)[ ]*(year)", 1)
destring years, replace
gen months = ustrregexs(1) if ustrregexm(sentence_string,"([0-9]+(\.[0-9])?)[ ]*(month)", 1)
destring months, replace
assert months*years==. | months==12*years
replace years = months/12 if mi(years)
replace years = 3 if sentence_string=="three years" // 2 obs.
assert sentence_string=="" if mi(years)
list years sentence_string if !mi(years) // two obs with "5 to 10 years" counted as 10 //  "Taylor: 50 years.  Delic: 30 years" coutned as 50
replace years = 7.5 if sentence_string=="5 to 10 years"
replace years = . if sentence_string=="Taylor: 50 years.  Delic: 30 years" // seems to have misunderstood the task, i.e., re-sentenced the examples, not our defendant?
* student round 1 and judges: there we only asked for a number (in all rounds, examples were phrased in years)
assert sentence==. if years!=. 
replace sentence = years if sentence==.
drop years months sentence_string

* other
label define yesno 0 "no" 1 "yes"
encode know_ICL, generate(knowICL) label(yesno)
drop know_ICL

encode recognize_, generate(recognize) label(yesno)
drop recognize_

assert inrange(confidence,1,100) if !mi(confidence)

* convenience variables
gen duration=minutes(time5-time2)
gen doctime =minutes(time4-time3)
gen conflict = ((precedent==1)+(def_nationality=="Sympathetic":nationality))!=1 // either prec=Affirm-->affirm AND def=Croat=nice, or neither
label var conflict "precedent and emotions push in opposite directions"


***** dropping unuseable / incomplete participations *******

drop if datausagewithdrawal==0 // IRB-mandated withdrawal right for students round 2 (0 is withdrawn)

count if !inlist(tech_diff,".","","no","No","None","No.","N/A")
assert r(N)==9
list studentround tech_diff if !inlist(tech_diff,"","no","No","None","No.","N/A") // a few freezes/sluggishness, plus reports that they couldn't open other tabs (which was intentionally programmed this way)

replace reasonforc="" if reasonforc=="."
drop if regexm(reasonforc,"Holger") | regexm(reasonforc,"ramthun")
list reasonforc if !mi(reasonforc) // 5 students where something else came up (plus one test)

assert judgmentreasons=="Test" if regexm(lower(judgmentreasons),"test") & !regexm(judgmentreasons,"testimony")
drop if judgmentreasons=="Test"
drop if regexm(lower(judgmentreasons),"holger") | regexm(lower(judgmentreasons),"ramthun") | regexm(lower(judgmentreasons),"denise neary") // Ramthun was our programmer, Neary the point person / organizer at the FJC

drop if mi(guilty) // problem: in judge and student round 1, missing judgment was recorded as 0 (as was "not guilty"), so we need more elaborate filter:
drop if mi(time4) // 1. step 4 is entering judgment stage -- no judgment without time4!
drop if mi(time5) & (studentround==1 | type=="judge":type) & guilty==0 & mi(judgmentreasons) // 2. step 5 is exiting judgment stage -- if judge or student round 1 participant didn't exit, 0 could be missing value, but not if judgment reasons were given
assert type=="judge":type & random=="bh6pa8rafkh81urgcd0fsgnd00" & guilty==0 if mi(time5) // 3. Anyway, this all concerns only one judge

assert random=="b2oopuj03i3u5bv6tjfkav6de2" & !mi(judgmentreasons) if canceled==1 | canceledexplicitorauto==1 | autocancelledinactivity==1 // so this was probably "cancelled" in error

assert random=="o8s56qc94rh5vtln3ec06rhbu4" & doctime<.12 & time1>tc("14apr2015 16:48:00") if type=="judge":type & doctime<10 & time1>tc("14apr2015 16:24:00")
drop if random=="o8s56qc94rh5vtln3ec06rhbu4"  // late start --> ipad problem, -- excluded in Spamann et al. 2021 and in robustness checks of Spamann & Kloehn 2016
count if doctime<3 | duration<5
assert r(N)==4
drop if doctime<3 | duration<5

assert random=="vcqjdqoa30p309cidlmlu9ua41" & guilty==0 & mi(sentence) & type=="judge":type if trim(judgmentreasons)=="Not sufficient time to form a judgment." // recall that guilty==0 could be missing value for judges and student round 1
drop if trim(judgmentreasons)=="Not sufficient time to form a judgment." 

***** finish *****
drop reasonforc canceled canceledexp autocancel datausagewithdrawal judgmentpapermatch amazon* tech_diff
compress
save `participations', replace


/********************************************************************
	II. CORRECTIONS AND ADDITIONS BASED ON REVIEW OF JUDGMENT REASONS
*********************************************************************/
* old RA work from Spamann et al. paper
import excel data\raw\RA\guilty_inferred_from_reasons_Johne.xlsx, sheet("Sheet1") firstrow clear // RA JohneHenre coded judgment reasons to infer affirmance/reversal
drop if mi(ID)
rename ID randomID
destring Affirmed, gen(affirmed) ignore("?")
merge 1:1 randomID using `participations', keep(2 3) // RA had coded judges from all countries (-->_merge==1) & had not coded students (--> _merge==2)
assert (type=="student":type | inlist(random,"bh6pa8rafkh81urgcd0fsgnd00","c2jeqoc8ph1ghbbrg9cle6h506")) if _merge==2 // bh6p... is the partipant missing time5 that we "rescued" above but hadn't given to JohneHenre
local RA_IDs `"inlist(random,"50rct5b73gu6f9sv7pi3rnrq51","ckhn152vfaelsdiibgr1e1h832","m6ihiipf2s0u9jsj206hc2og43")"'
assert `RA_IDs' & guilty==0 if guilty!=affirmed & !mi(Reason) & Affirmed!="?" & _merge==3 // re guilty==0: recall from above that 0 was also recorded for missing entries in judge round -- so judge could have erroneously left it like that
gen rawguilty = guilty // for possible use later
replace guilty = affirmed if `RA_IDs'
drop _merge Affirmed affirmed Reason_Text Note 

* additional participant "rescued" when assembling data from scratch: see above -- bh6p... had mi(time5) but did give reasons for affirming
list guilty judgmentrea if random=="bh6pa8rafkh81urgcd0fsgnd00", notrim 
replace guilty=1 if random=="bh6pa8rafkh81urgcd0fsgnd00"
save `participations', replace

*** RA work for this paper -- they were given the data up to this point, so their coding of ERROR, MISUNDERSTANDING, and REFUSAL verifies that we handled everything correctly thus far
import excel data\raw\RA\Case_coding_Julian_students.xlsx, sheet("Sheet1") firstrow clear
tempfile Julian
save `Julian'
import excel data\raw\RA\Case_coding_Oliver_students.xlsx, sheet("Sheet1") firstrow clear
rename (REF-POL) =_O
merge 1:1 random-guilty using `Julian', assert(3) nogenerate
drop nationality precedent judgmentreasons guilty
merge 1:1 random using `participations', assert(2 3)
assert precedent=="besic":precedent if _merge==2 // we didn't have these coded because we can't compare them with judges and didn't want to waste RA time
drop if _merge==2
drop _merge 

assert (ERROR==0 & ERROR_O==0 & MISUNDERSTANDING==0 & MISUNDERSTANDING_O==0 & REFUSAL==REFUSAL_O)
qui count if REFUSAL!= 0
assert r(N) == 2
list random type judgmentreason if REFUSAL!= 0, noobs notrim // 2 reasons that start with statement that time was too short but that nevertheless give judgment reasons

foreach var of varlist PRECEDENT STATUTE POLICY {
	count if `var'!=`var'_O 
	assert r(N)<=3
	replace `var' = (`var' + `var'_O)/2
	}
	
drop *_O ERROR MISUNDERSTANDING
rename (PRECEDENT STATUTE POLICY) mentioned_=
rename mentioned_*, lower
save `participations', replace

/********************************************************************
	III. DOCUMENT VIEWS -- ingest & clean
*********************************************************************/
*** how much time (minutes) a participant spent with each of the documents; plus sequence data for path analysis (to be completed at end of script)
* NB: judges' time data are already in Spamann & Kloehn (2016) and *most* judges' sequences in Spamann et al. (2021) but not all because the latter applied more stringent filters (29 instead of 32 obs.) 

gen double time = time4 // time when document views end (participant proceeds to judgment) -- this will serve to calculate length of last document view
gen double starttime = time3 // time when document views start (participant enters main document area) -- this will serve to check data integrity
keep randomID time starttime
gen document = "end"
save `endtime'

import delimited data\raw\document_requests_2015-04-15T20-15-33+00-00.csv, varnames(1) clear
save `docviews'
import delimited data\raw\document_requests_2015-07-01T11-44-43+00-00.csv, varnames(1) clear
append using `docviews'
save `docviews', replace
import delimited data\raw\document_requests_2016-02-15T11-56-44+00-00.csv, varnames(1) clear
append using `docviews'
	
rename participationid randomID
replace document = subinstr(document,"/en_US","",1)
replace document = subinstr(document,"/documents/","",1)
replace document = "toc" if doc=="/documents"
replace document = "precedent" if regexm(doc,"precedent") // there are versions /2 and /3 in the data as well because of splitting of the long docs
replace document = "trialjudgment" if regexm(doc,"trial_judgment") // id.

gen double time = clock(timestamp,"YMD#hms#")
format time %tc
duplicates drop randomID time doc, force
duplicates report random time
isid random time idof
bys random time (idof): keep if _n==_N // in case of duplicates by time, keep the higher ID of the document request = later click (this only concerns the 3 duplicates listed above)
drop idof timestamp

append using `endtime'
save `sequence' // this will be saved as sequence.dta at end of the script after dropping unuseable observations & checking consistency


/*************************************************
	IV. Main Data: add document view times & finish
*************************************************/

bys random (time): gen t_ = time[_n+1]-time
assert t_==. if document=="end" // should be because nothing comes after "end" so time[_n+1] should be empty
drop if document=="end"
collapse (sum) t_, by(randomID document)
replace t_ = minutes(t_)
reshape wide t_, i(randomID) j(document) string
recode t_* (.=0) // in judge data, some participants actually did not look at some documents
egen t_total = rowtotal(t_*)

merge 1:1 randomID using `participations', assert(1 3) keep(3) nogenerate
order randomID type studentround def_nationality precedent prime anchor time* t_* duration doctime guilty rawguilty sentence judgmentreasons
rename def_nat nationality // this could have been done earlier but I didn't want to risk breaking anything -- analysis code uses "nationality"
compress
save `participations', replace
save data\studentjudge_maindata.dta, replace

*** verify consistency with / compare differences to Spamann & Kloehn (2016) (cf. paper section 3.1)
keep if type=="judge":type
keep random nationality precedent guilty
rename (precedent nationality guilty) =_new
merge 1:1 random using data\FJC_April2015_nopersonal.dta, assert(2 3) // these are the cleaned data used in Spamann & Kloehn 2016
drop if mi(guilty) & mi(guilty_new) // those are useless
assert precedent==precedent_new if _merge==3
assert nationality==nationality_new if _merge==3
* 1 add'l obs:
count if mi(guilty) & !mi(guilty_new)
assert r(N)==1
assert random=="bh6pa8rafkh81urgcd0fsgnd00" & _merge==3 if mi(guilty) & !mi(guilty_new) // this participant did not proceed to the post-judgment, sentencing stage but did write reasons. The system recorded guilty==0 but this was corrected above to 1 in function of the clear reasons. So this is not a confusion of missing value for 0 and thus not an incomplete participation. It should not have been dropped in S&K 2016
* 3 corrected obs:
count if !mi(guilty,guilty_new) & guilty!=guilty_new & _merge==3
assert r(N)==3
assert inlist(random,"50rct5b73gu6f9sv7pi3rnrq51","ckhn152vfaelsdiibgr1e1h832","m6ihiipf2s0u9jsj206hc2og43") if !mi(guilty,guilty_new) & guilty!=guilty_new & _merge==3 // we corrected these under II above in function of judgment reasons (--> cf. SK analysis script lines 173 et seq.)
* 2 omitted obs
count if _merge==2
assert r(N)==2
assert inlist(random,"o8s56qc94rh5vtln3ec06rhbu4","vcqjdqoa30p309cidlmlu9ua41") if _merge==2 // see above on o8s56qc94rh5vtln3ec06rhbu4 (iPad freeze issue) (--> cf. SK analysis script line 146, 148) and vcqjdqoa30p309cidlmlu9ua41 (no time to give judgment) (--> cf. SK analysis script line 179)


/********************************************************************
	V. Sequence data: drop help docs, shift to event time, and finish
********************************************************************/
use `sequence', clear
merge m:1 randomID using `participations', keepusing(doctime precedent type) assert(1 3) keep(3) nogenerate // get rid of unuseable participations, add doctime (seriousness!), precedent (to filter out Besic), type
assert time-starttime<(1000*60*60) if !mi(starttime) // since time is in ms, 1000ms * 60s * 60 = 60 min -- no participation should be longer given 55 min cap
drop starttime

isid random time // time is tc calendar time (ms)
replace document = "briefs" if regexm(document,"brief") // treating appellant's and respondent's briefs as interchangeable
replace document = "facts" if document == "statement_of_facts" // for easier display in graphs

* checking if there are participants with long stretches of viewing toc -- if not, we can drop them to make computation etc. easier
bys random (time): gen pos = _n // in which order did the click occur
by  random (time): gen length = time[_n+1] - time // how long did it stay live
assert document=="toc" if pos == 1
drop if pos==1
list length pos if document=="toc"
count if document=="toc"
assert r(N)==2
sum length if document=="toc"
assert r(max)<30000 // 30s
sum pos if document=="toc"
assert r(max)<=3
drop if document=="toc"

* instruction views: chop off at the start
tab document [iw=length] // 1.89% instructions
bys random (time): drop if _n==1 & document=="instructions" // chop off instruction views in the beginning ...
while r(N_drop)>0 {
	bys random (time): drop if _n==1 & document=="instructions" // ... and keep doing this until there are none left
	}
tab document [iw=length] // 1.16% instructions

* eventtime (including more chopping off of instructions)
scalar totalclicks = _N
gen int eventtime = . // starting off with missing values makes it possible to do the loop below
local drop = 1
while `drop'!=0 { // chop off instructions at eventtime==1 until there aren't any anymore even after dropping duplicates within that event window 1
	bys random (time): replace eventtime = round(500*(time - time[1])/(time[_N]-time[1])) + 1 //  normalized & discretized to 500 periods (oma crashes with >500 sequence elements) (NB: t=501 is "end", see below)
	bys random eventtime (time): drop if eventtime==1 & _n<_N // keep later click in the first event window
	drop if eventtime==1 & document=="instructions" // chop off initial view if it is instructions (we already did this but rounding & keeping last could have re-created)
	local drop = r(N_drop)
	}
bys random eventtime (time): keep if _n==_N // now keep later observation in any event window (NB: these are click data and hence sparse, so it is important to preserve the later click, which will probably stay live for a while)
assert _N>0.94*totalclicks // confirm we have only removed a small fraction of clicks during cleaning

* dropping "end" markers
assert document=="end" if eventtime==501
drop if eventtime == 501

* encoding
encode random, gen(ID)
encode document, gen(doc)
drop random document time pos length

* fill the time space with document then still live
xtset ID eventtime
tsfill, full
bys ID: replace doc = doc[_n-1] if mi(doc) // same as above - since these are click data, the right approach is to fill with earlier doc until next click comes
foreach IDvar in doctime precedent type {
	bys ID (`IDvar'): replace `IDvar' = `IDvar'[1]
}

compress
save data\studentjudge_sequence.dta, replace

/********************************************************************
	APPENDIX: Creating summary statistics of US demographics -- only possible with access to US data before de-personalization, which requires confidentiality agreement
*********************************************************************/
if "`personal'"=="on" {
use `judgedemographics', clear
merge 1:1 random using data\studentjudge_maindata.dta, keepusing(random) keep(3) nogenerate // this retains only the useable participations
cap label define yesno 0 "no" 1 "yes"
encode ever_prosecutor, generate(everprosecutor) label(yesno)
encode ever_defender, generate(everdefender) label(yesno)
tab age, gen(age)
tab gender, gen(gender)

drop age gender
foreach v of varlist age* gender* {
	local l`v' : variable label `v'
	}
collapse (mean) age* gender* everp everd
foreach v of varlist age* gender* {
	label var `v' "fraction `l`v''"
	}
save data\USdemographics, replace
}