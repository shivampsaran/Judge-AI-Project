# IF YOU CHOOSE TO RUN THIS SCRIPT INDIVIDUALLY, FIRST ASSIGN THE FILE PATH TO THE REPLICATION PACKAGE TO "user_file_path"
# user_file_path <-

library(tidyverse)
library(haven)
library(stats)
library(elrm)
library(stargazer)
library(car)

set.seed(2000)

# Reading in Spamann & Klohn's data
jsi <- read_dta(file.path(user_file_path, "Replication Package", "Data", "Spamann & Klohn", "dataverse_files", "data", "studentjudge_maindata.dta"))
jsi <- jsi %>% select(type, nationality, precedent, guilty)

# Renaming columns to match our data structure
jsi <- jsi %>% rename(Affirm = guilty, Precedent = precedent, Sympathy = nationality, Group = type)

# Converting categorical variables to numerical variables
jsi <- jsi %>%
  mutate(
    Sympathy = if_else(Sympathy == 1, 1, 0),
    Precedent = if_else(Precedent == 1, 1, 0),
    Group = if_else(Group == 1, "Judge", "Student")
  )

# NOTE: 
# SYMPATHETIC = 1, UNSYMPATHETIC = 0
# PRECEDENT AFFIRM = 1, PRECEDENT REVERSE = 0

# Reading in GPT experiment results
gpti <- read_csv(file.path(user_file_path, "Replication Package", "Results", "GPT Individual Results.csv"))
gpti <- gpti %>% select(c(Filename, Affirm))

# Converting categorical variables to numerical variables
gpti <- gpti %>% mutate(Group = "GPT-4o") %>%
  mutate(Sympathy = ifelse(str_detect(Filename, "Horvat"), 1, 0)) %>%
  mutate(Precedent = ifelse(str_detect(Filename, "Sainovic"), 1, 0))
gpti <- gpti %>% select(!Filename) %>% relocate(Affirm, .after = last_col())

# Binding results
final_df <- rbind(jsi, gpti)
final_df$Group <- as.factor(final_df$Group)

# Setting Judge, Affirm, Sympathetic as the baseline for regressions
final_df <- final_df %>%
  mutate(
    Group = relevel(as.factor(Group), ref = "Judge"),
    Precedent = relevel(as.factor(Precedent), ref = "1"),  # "Affirm" as baseline
    Sympathy = relevel(as.factor(Sympathy), ref = "1")     # "Sympathetic" as baseline
  )

# Fit the OLS model
ols_model <- lm(Affirm ~ Precedent + Sympathy + Group + Group:Precedent + Group:Sympathy, data = na.omit(final_df))

# Fit the logistic model
logit_model <- glm(Affirm ~ Precedent + Sympathy + Group + Group:Precedent + Group:Sympathy,
                   family = binomial(link = "logit"), data = final_df)

# Extract coefficients and confidence intervals for the OLS model
ols_coef <- coef(ols_model)
ols_ci <- confint(ols_model)
ols_ci <- round(ols_ci, 2)
ols_pvals <- summary(ols_model)$coefficients[,4]

# Extract odds ratios and confidence intervals for the logistic model
logit_or <- exp(coef(logit_model))       # Odds ratios
logit_ci <- exp(confint(logit_model))    # Confidence intervals for odds ratios
logit_ci <- round(logit_ci, 2)
logit_pvals <- summary(logit_model)$coefficients[,4]

# Temporarily place placeholder model to hold the results for exact logistic model
placeholder_model <- ols_model 

# Filtering individual datasets for student and GPT to isolate their effects in the exact logistic model.
# Note: the elrm package requires that you calculate odds ratios for variables one at a time
student <- final_df %>% filter(Group != "GPT-4o")
student <- student %>%
  mutate(Group = ifelse(Group == "Judge", 0, 1))

GPT <- final_df %>% filter(Group != "Student")
GPT <- GPT %>%
  mutate(Group = ifelse(Group == "Judge", 0, 1))

collapsed_data_student <- student %>%
  group_by(Group, Sympathy, Precedent) %>%
  summarise(Successes = sum(Affirm), Trials = n(), .groups = 'drop')

collapsed_data_GPT <- GPT %>%
  group_by(Group, Sympathy, Precedent) %>%
  summarise(Successes = sum(Affirm), Trials = n(), .groups = 'drop')

# Computing Odds Ratio for Group: Student
m1_student <- elrm(
  formula = Successes/Trials ~ Group + Sympathy + Precedent + Group:Precedent + Group:Sympathy,
  interest = ~Group, 
  iter = 100000,  
  burnIn = 10000, 
  data = collapsed_data_student
)

# Computing Odds Ratio for Precedent: Reverse
m2_student <- elrm(
  formula = Successes/Trials ~ Group + Sympathy + Precedent + Group:Precedent + Group:Sympathy,
  interest = ~Group:Precedent, 
  iter = 100000,  
  burnIn = 10000, 
  data = collapsed_data_student
)

# Computing Odds Ratio for Defendant: Unsympathetic
m3_student <- elrm(
  formula = Successes/Trials ~ Group + Sympathy + Precedent + Group:Precedent + Group:Sympathy,
  interest = ~Group:Sympathy, 
  iter = 100000,  
  burnIn = 10000, 
  data = collapsed_data_student
)

# JOINT P-VALUE FOR STUDENT INTERACTION TERMS
# Note: Not reported in our paper; purely for Spamann/Klöhn replication; See Table 3 on Page 6 (Spamann & Klohn, 2021)
# m4_student <- elrm(Successes/Trials ~ Group + Sympathy + Precedent + Group:Precedent + Group:Sympathy,
#                    interest = ~Group:Precedent + Group:Sympathy, 
#                    iter = 100000, burnIn = 10000, data = collapsed_data_student)

# Extracting Odds Ratio
m1OR_student <- exp(m1_student$coeffs[[1]])
# Extracting Confidence Interval
m1CI_student <- exp(m1_student$coeffs.ci)
# Extracting P-value
m1pval_student <- m1_student$p.values[[1]]

m2OR_student <- exp(m2_student$coeffs[[1]]) 
m2CI_student <- exp(m2_student$coeffs.ci)
m2pval_student <- m2_student$p.values[[1]]

m3OR_student <- exp(m3_student$coeffs[[1]]) 
m3CI_student <- exp(m3_student$coeffs.ci)
m3pval_student <- m3_student$p.values[[1]]

# Not reported in our paper; purely for Spamann/Klöhn replication
# m4OR_student <- exp(m4_student$coeffs[[1]]) 
# m4CI_student <- exp(m4_student$coeffs.ci)
# m4pval_student <- m4_student$p.values[[1]]

# Computing Odds Ratio for Group: GPT
m1_gpt <- elrm(
  formula = Successes/Trials ~ Group + Sympathy + Precedent + Group:Precedent + Group:Sympathy,
  interest = ~Group, 
  iter = 100000,  
  burnIn = 10000, 
  data = collapsed_data_GPT
)

# Computing Odds Ratio for Precedent: Reverse
m2_gpt <- elrm(
  formula = Successes/Trials ~ Group + Sympathy + Precedent + Group:Precedent + Group:Sympathy,
  interest = ~Group:Precedent, 
  iter = 100000,  
  burnIn = 10000, 
  data = collapsed_data_GPT
)

# Computing Odds Ratio for Defendant: Unsympathetic
m3_gpt <- elrm(
  formula = Successes/Trials ~ Group + Sympathy + Precedent + Group:Precedent + Group:Sympathy,
  interest = ~Group:Sympathy, 
  iter = 100000,  
  burnIn = 10000, 
  data = collapsed_data_GPT
)

# Extracting Odds Ratio
m1OR_gpt <- exp(m1_gpt$coeffs[[1]])
# Extracting Confidence Interval
m1CI_gpt <- exp(m1_gpt$coeffs.ci)
# Extracting P-value
m1pval_gpt <- m1_gpt$p.values[[1]]

m2OR_gpt <- exp(m2_gpt$coeffs[[1]]) 
m2CI_gpt <- exp(m2_gpt$coeffs.ci)
m2pval_gpt <- m2_gpt$p.values[[1]]

m3OR_gpt <- exp(m3_gpt$coeffs[[1]]) 
m3CI_gpt <- exp(m3_gpt$coeffs.ci)
m3pval_gpt <- m3_gpt$p.values[[1]]

# Prepare lists with NA placeholders for non-numeric rows
exact_or <- c(NA, NA, NA, 
              m1OR_gpt, m1OR_student, 
              m2OR_gpt, m2OR_student, 
              m3OR_gpt, m3OR_student)
names(exact_or) <- names(logit_or)

exact_ci <- rbind(NA, NA, NA, 
                  m1CI_gpt, m1CI_student, 
                  m2CI_gpt, m2CI_student, 
                  m3CI_gpt, m3CI_student)

# Renaming columns & rownames
colnames(exact_ci) <- colnames(ols_ci)
rownames(exact_ci) <- rownames(ols_ci)

exact_ci <- as.matrix(exact_ci)
exact_ci <- round(exact_ci, 2)

exact_pvals <- c(NA, NA, NA, 
                 m1pval_gpt, m1pval_student, 
                 m2pval_gpt, m2pval_student, 
                 m3pval_gpt, m3pval_student)

# Generate table with rounded coefficients and CIs in latex
# Paste output in Overleaf or other latex editor; for exact formatting as seen in paper, reach out to authors for additional (manual) instructions
stargazer(ols_model, logit_model, placeholder_model, type = "latex", title = "Regression Models",
          dep.var.caption = "Dependent Variable: Affirmed",
          dep.var.labels = "",  
          covariate.labels = c("P-Reverse", "Defendant: Unsympathetic", "GPT-4o", "Student", 
                               "GPT-4o * P-Reverse", "Student * P-Reverse", 
                               "GPT-4o * Unsympathetic", "Student * Unsympathetic"),
          column.labels = c("OLS", "Logit", "Exact Logistic"),
          digits = 2,
          model.names = FALSE,
          coef = list(ols_coef, logit_or, exact_or),
          ci.custom = list(ols_ci, logit_ci, exact_ci),
          p = list(ols_pvals, logit_pvals, exact_pvals),
          star.cutoffs = c(0.05, 0.01))

# WALD/SCORE TEST TO TEST DIFFERENCE BETWEEN GPT AND STUDENT COEFFICIENTS
# These are reported in footnote 21

ols_wald <- linearHypothesis(ols_model, "GroupStudent = GroupGPT-4o")
logit_wald <- linearHypothesis(logit_model, "GroupStudent = GroupGPT-4o")

ols_wald_pval <- ols_wald$`Pr(>F)`
logit_wald_pval <- logit_wald$`Pr(>Chisq)`

score_data <- final_df %>% filter(Group != "Judge")
score_data <- score_data %>%
  mutate(Group = ifelse(Group == "Student", 0, 1))

collapsed_data_score <- score_data %>%
  group_by(Group, Sympathy, Precedent) %>%
  summarise(Successes = sum(Affirm), Trials = n(), .groups = 'drop')

score_model <- elrm(
  formula = Successes / Trials ~ Group + Sympathy + Precedent + Group:Precedent + Group:Sympathy,
  interest = ~ Group,
  iter = 100000,
  burnIn = 10000,
  data = collapsed_data_score
)

# Extract score test statistic and p-value
score_stat <- score_model$coeffs
score_pval <- score_model$p.values

# WALD/SCORE TEST FOR JOINT P-VALUE BETWEEN INTERACTION TERMS (OLS & LOGIT)
# These results are reported in footnote 22

ols_joint <- linearHypothesis(ols_model, c("Precedent0:GroupGPT-4o = 0", "Sympathy0:GroupGPT-4o = 0"))
logit_joint <- linearHypothesis(logit_model, c("Precedent0:GroupGPT-4o = 0", "Sympathy0:GroupGPT-4o = 0"))

ols_joint_pval <- ols_joint$`Pr(>F)`
logit_joint_pval <- logit_joint$`Pr(>Chisq)`

m4_gpt <- elrm(Successes/Trials ~ Group + Sympathy + Precedent + Group:Precedent + Group:Sympathy,
               interest = ~Group:Precedent + Group:Sympathy,
               iter = 100000, burnIn = 10000, data = collapsed_data_GPT)

exact_joint_pval <- m4_gpt$p.values[[1]]




