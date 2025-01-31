# Replication Package for Posner & Saran (2025)
# Judge AI: Assessing Large Language Models in Judicial Decision-Making

This folder contains the necessary code and data to replicate all results, figures, and outputs in Posner & Saran (2025). Note that because large language models are not entirely deterministic, running the same exact code (same parameters, prompts, etc.) does not guarantee replicability. In fact, in rerunning the experiment with GPT, we found slightly different results. Because of this, we have provided all of our original results from our experiment in the Results folder. You may choose to rerun the GPT experiment by running the code in the *00 - Posner & Saran GPT Generation* folder. If you do this, all of the existing code will still work (producing figures, regressions, etc.). Just be aware that the new results will overwrite the old ones (i.e., Posner & Saran's).

## What this replication package contains

There are five folders in the replication folder: 
(1) Code
(2) Data
(3) Experiment Materials
(4) Graphs
(5) Results

(1) The Code folder stores all the relevant scripts. This includes...
    (a) Master File.R
        - runs scripts b-g
    (b) Assessing GPT Results.R 
        - determines whether in each iteration, GPT affirmed or reversed
        - tracks mentions of key metrics (policy, precedent, and statute)
        - creates the final dataframe
    (c) Affirm Plots (Figures 1, 2, and 3a).R
        - produces the frequency of affirming plots (as seen in paper)
    (d) Statistical Tests (and Table 1 Output).R
        - calculates statistical significance of precedent and sympathy effects for GPT
        - calcualtes statistical significance of differences between GPT and Student/Judge results
        - produces the output for Table 1 (as seen in the paper)
    (e) Regressions (and Wald, Score Tests).R
        - runs all regressions (ols, logit, and exact logistic)
        - produces the regression table (as seen in the paper)
        - calculates the joint p-value between GPT interaction terms in all models
        - calculates the p-value of difference in coefficients between GPT and Student coefficients in all models
    (f) Mention Plot (Figure 4).R
        - produces the mention of key term plots (as seen in paper)
    (g) Prompt Engineering.R
        - produces figures for prompt engineering (as seen in paper)
        
Note: As mentioned earlier, the *00 - Posner & Saran GPT Generation* folder will rerun the experiment, including the cleaning done to get the necessary input materials. If you choose to run these files, you will not get exact replicates of our results, figures, etc. Note also that these scripts will also overwrite our original experiment results. The following scripts are included in this folder:
    (a) Case Scraping & Cleaning.ipynb 
        - converts Spamann & Klöhn's HTML files into .txt files readable for GPT
    (b) Simulating Decisions.ipynb
        - runs the experiment of GPT deciding the case in 100 iterations across four conditions
    (c) Prompt Engineering.ipynb
        - runs the various prompt engineering experiments

*NOTE*: If you choose to run Prompt Engineering.ipynb, DO NOT RUN ALL AT ONCE. Each code block is a various prompt engineering attempt and it will be very expensive to run all. 
        
(2) The Data folder contains Spamann & Klöhn's results

(3) The Experiment Materials folder contains all the input material used in the experiment; it also includes Spamann & Klöhn's original HTML files (which are converted to .txt files in our experiment using the the Case Scraping & Cleaning.ipynb script)

(4) The Graphs folder stores the graphs

(5) The Results folder stores the results

## How to use this replication package

### Dependencies

While the experiment was conducted in Python, the primary analysis and production of outputs (tables, graphs, etc.) was done in R. The following packages are used:
- tidyverse
- stringi
- haven
- boot
- pwr
- knitr
- Exact
- stats
- elrm
- stargazer
- car

The Master File.R script installs these packages prior to running the scripts. However, if you choose to run scripts individually, you can install the relevant packages for that script by running the following code in your R console:
```
install.packages("[PACKAGE_NAME]")
```

If you choose to rerun the experiment, you will need the following Python packages:
- bs4
- os
- re
- tiktoken
- openai
- time
- pandas
- dotenv

To install a package in Python, replace [PACKAGE_NAME] with the name of the desired package and run the following code in your terminal:
```
pip install [PACKAGE_NAME]
```
Note: An API-KEY from OpenAI and credit is required to run GPT prompts (i.e., Simulating Decisions.ipynb and Prompt Engineering.ipynb)

### Executing the code

To produce our outputs, run the *Master File.R* script. If you choose to run the scripts individually, they are named numerically. Run them in that order.

Also, make sure to replace "directory" at the top of Master File with the actual directory where you have the replication package stored on your computer. If you would like to run any of the scripts individually, you will similarly have to assign the directory at the top of each file in the object "directory_name"

## Help

You may run into several issues while running this code. Here's what you should do:

If you face problems with token limits, make sure that you are using the correct GPT model. Older models have lower token limits. You can view token limits of models here: https://platform.openai.com/docs/models. 

If you face problems with exceeding your OpenAI quota, this may be due to your organization's usage tier. See here: https://platform.openai.com/docs/guides/rate-limits/usage-tiers?context=tier-one#usage-tiers. If you are on a lower tier, you may have to increase the amount of time in the time.sleep command in the Simulating Decisions script (e.g., increasing from time.sleep(10) to time.sleep(60)).

Either of these issues may also arise from inadequate funds in your account. Go to your OpenAI API billing account to check the amount of credits you have.

If you face other problems, reach out to OpenAI or if the issues are internal, reach out to Saran (see below).

## Authors

Shivam Saran, shivamsaran@uchicago.edu
Eric Posner, eposner@uchicago.edu