*  if not already done: cd to the folder where this script is stored, which should have a "data" subfolder with the data and another subfolder "output" to store the results
* the called scripts use user-written packages cibar, grc1leg2, grstyle, sadi, sq, colorpalette, eststo

cap mkdir output // to store the results; you don't need to create one for the intermediate data because that should already have been created in downloading the replication files

log using output\SpamannKloehn2023, replace

do ingestion_student_and_judge_data.do // creates the two datasets (decisions incl. reasons, and paths) used in the analyses, and, if you have access to the full data under an NDA with us, one helper demographic file

do main_analysis_students_v_USjudges.do // produces summary statistics (T.1) and tests of experimental treatment effects separately for judges and students (cf. T.2), and tests of their difference (T.3 etc.)
do sequences_students_v_USjudges_v2.do // sequence analysis -- the "document paths" shown in the paper's first figure
do reasons_students_v_USjudges.do // comparing reasons given by judges and students -- paper's second figure

log close