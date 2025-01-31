# Set your user_file_path
user_file_path <- "~/Desktop/Univ. of Chicago/AI Judges"  # CHANGE THIS TO YOUR ACTUAL FILE PATH WHERE THE REPLICATION PACKAGE IS STORED

# Installs the necessary packages if not already installed
packages <- c("tidyverse", "stringi", "haven", "boot", "pwr", "knitr", 
              "Exact", "stats", "elrm", "stargazer", "car")

install_if_missing <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
    message(paste("Installing package:", pkg))
  } else {
    message(paste("Package already installed:", pkg))
  }
}

invisible(lapply(packages, install_if_missing))

# Run each script
source(file.path(user_file_path, "Replication Package", "Code", "01 - Posner & Saran Replication", "01 - Assessing GPT Results.R"))
source(file.path(user_file_path, "Replication Package", "Code", "01 - Posner & Saran Replication", "02 - Affirm Plots (Figures 1, 2, and 3a).R"))
source(file.path(user_file_path, "Replication Package", "Code", "01 - Posner & Saran Replication", "03 - Statistical Tests (and Table 1 Output).R"))
source(file.path(user_file_path, "Replication Package", "Code", "01 - Posner & Saran Replication", "04 - Regressions (and Wald, Score Tests).R"))
source(file.path(user_file_path, "Replication Package", "Code", "01 - Posner & Saran Replication", "05 - Mention Plot (Figure 4).R"))
source(file.path(user_file_path, "Replication Package", "Code", "01 - Posner & Saran Replication", "06 - Prompt Engineering (Figures 5 and 6)"))
