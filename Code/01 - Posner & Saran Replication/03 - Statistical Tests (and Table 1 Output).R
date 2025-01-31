# IF YOU CHOOSE TO RUN THIS SCRIPT INDIVIDUALLY, FIRST ASSIGN THE FILE PATH TO THE REPLICATION PACKAGE TO "user_file_path"
# user_file_path <-

library(tidyverse)
library(Exact)
library(pwr)
options(scipen=999)
library(knitr)

results <- read_csv(file.path(user_file_path, "Replication Package", "Results", "Group Comparison.csv"))
results <- results %>% filter(Group != 'Judges (Original)')
results <- results %>% mutate(Group = ifelse(Group == 'Judges (Adjusted)', 'Judges', Group))

# Checking statistical significance in difference within GPT results (Boschloo unconditional test)
# See https://search.r-project.org/CRAN/refmans/Exact/html/exact.test.html

# Subsetting dataframes to precedent, sympathy, and granular level of Unsympathetic_Reverse & Sympathetic_Affirm
GPT_precedent <- results %>% filter(Group == 'GPT-4o' & Condition_ %in% c('Affirm', 'Reverse'))
GPT_sympathy <- results %>% filter(Group == 'GPT-4o' & Condition_ %in% c('Sympathetic', 'Unsympathetic'))
GPT_granular <- results %>% filter(Group == 'GPT-4o' & Condition_ %in% c('Unsympathetic_Reverse', 'Sympathetic_Affirm'))

# Checking for precedent effect
data <- matrix(c(GPT_precedent$Affirm[1], GPT_precedent$Reverse[1], GPT_precedent$Affirm[2], GPT_precedent$Reverse[2]), 2, 2, byrow=TRUE)
GPT_precedent_boschloo <- exact.test(data, alternative="two.sided", method="boschloo",
           conf.int=TRUE, tsmethod = 'central', conf.level=0.95)

# Checking for sympathy effect
data <- matrix(c(GPT_sympathy$Affirm[1], GPT_sympathy$Reverse[1], GPT_sympathy$Affirm[2], GPT_sympathy$Reverse[2]), 2, 2, byrow=TRUE)
GPT_sympathy_boschloo <- exact.test(data, alternative="two.sided", method="boschloo",
                                     conf.int=TRUE, tsmethod = 'central', conf.level=0.95)

# Checking for effect of Unsympathetic_Reverse - Sympathetic_Affirm
# This is done purely in efforts to reproduce Spamann & Klohn's methods but does not make our paper.
# See Table 2 on page 269 (Spamann & Klohn, 2016)
data <- matrix(c(GPT_granular$Affirm[1], GPT_granular$Reverse[1], GPT_granular$Affirm[2], GPT_granular$Reverse[2]), 2, 2, byrow=TRUE)
GPT_granular_boschloo <- exact.test(data, alternative="two.sided", method="boschloo",
                                     conf.int=TRUE, tsmethod = 'central', conf.level=0.95)

GPT_differences <- data.frame(Difference = c("Precedent", "Sympathy", "UR-SA"), 
                              P_value = c(GPT_precedent_boschloo$p.value, GPT_sympathy_boschloo$p.value, 
                                          GPT_granular_boschloo$p.value))


# Checking statistical significance in terms of whether GPT is closer to students or judges (Boschloo)

# Define the conditions
conditions <- c("Affirm", "Reverse", "Sympathetic", "Unsympathetic", 
                "Sympathetic_Affirm", "Sympathetic_Reverse", "Unsympathetic_Affirm", "Unsympathetic_Reverse")

# Initialize an empty dataframe to store the results
results_df <- data.frame(Condition = character(),
                         Comparison = character(),
                         Proportion_Difference = numeric(),
                         Cohen_H = numeric(), 
                         P_value = numeric(),
                         stringsAsFactors = FALSE)

# Loop through each condition
for (condition in conditions) {
  # Filter the data for the current condition
  subset_data <- results %>% filter(Condition_ == condition)
  
  
  # GPT-Judges Proportion Difference
  gpt_j_prop_diff <- subset_data$percent_affirm[1] - subset_data$percent_affirm[2]
  # GPT-Judges Cohen's H value
  gpt_j_cohen_h <- ES.h(subset_data$percent_affirm[1], subset_data$percent_affirm[2])
  # GPT-Judges Boschloo test
  data <- matrix(c(subset_data$Affirm[1], subset_data$Reverse[1],
                   subset_data$Affirm[2], subset_data$Reverse[2]), 2, 2, byrow=TRUE)
  gpt_j_diff <- exact.test(data, alternative="two.sided", method="boschloo",
                           conf.int=TRUE, tsmethod = 'central', conf.level=0.95)
 
  # GPT-Student Proportion Difference
  gpt_s_prop_diff <- subset_data$percent_affirm[1] - subset_data$percent_affirm[3]
  # GPT-Students Cohen's H value
  gpt_s_cohen_h <- ES.h(subset_data$percent_affirm[1], subset_data$percent_affirm[3])
  # GPT-Students Boschloo test
  data <- matrix(c(subset_data$Affirm[1], subset_data$Reverse[1],
                   subset_data$Affirm[3], subset_data$Reverse[3]), 2, 2, byrow=TRUE)
  gpt_s_diff <- exact.test(data, alternative="two.sided", method="boschloo",
                           conf.int=TRUE, tsmethod = 'central', conf.level=0.95)
  
  # Judges-Students Proportion Difference
  j_s_prop_diff <- subset_data$percent_affirm[2] - subset_data$percent_affirm[3]
  # Judges-Students Cohen's H value
  j_s_cohen_h <- ES.h(subset_data$percent_affirm[2], subset_data$percent_affirm[3])
  # Judges-Students Boschloo test
  data <- matrix(c(subset_data$Affirm[2], subset_data$Reverse[2],
                   subset_data$Affirm[3], subset_data$Reverse[3]), 2, 2, byrow=TRUE)
  j_s_diff <- exact.test(data, alternative="two.sided", method="boschloo",
                           conf.int=TRUE, tsmethod = 'central', conf.level=0.95)
  
# Combine results
results_df <- rbind(results_df, 
                      data.frame(Condition = condition, Comparison = "GPT vs. Judges", Proportion_Difference = round(gpt_j_prop_diff,2), Cohen_H = round(gpt_j_cohen_h,2), P_value = round(gpt_j_diff$p.value,4)),
                      data.frame(Condition = condition, Comparison = "GPT vs. Students", Proportion_Difference = round(gpt_s_prop_diff,2), Cohen_H = round(gpt_s_cohen_h,2), P_value = round(gpt_s_diff$p.value,4)), 
                    data.frame(Condition = condition, Comparison = "Judges vs. Students", Proportion_Difference = round(j_s_prop_diff,2), Cohen_H = round(j_s_cohen_h,2), P_value = round(j_s_diff$p.value,4)))
}

# Excluding Judges vs. Students from the final table output
results_final <- results_df %>% filter(Comparison != "Judges vs. Students")

results_final$P_value <- round(results_final$P_value, 2)

results_final <- results_final %>%
  mutate(Condition = case_when(
    Condition == "Affirm" ~ "P-Affirm",
    Condition == "Reverse" ~ "P-Reverse",
    Condition == "Sympathetic_Affirm" ~ "Sympathetic/P-Affirm",
    Condition == "Sympathetic_Reverse" ~ "Sympathetic/P-Reverse",
    Condition == "Unsympathetic_Affirm" ~ "Unympathetic/P-Affirm",
    Condition == "Unsympathetic_Reverse" ~ "Unympathetic/P-Reverse",
    TRUE ~ as.character(Condition)  # Preserve other values if necessary
  )) %>%
  relocate(P_value, .after = 3) %>%
  rename("Proportion Difference" = Proportion_Difference, 
         "P-value" = P_value,
         "Cohen's h Effect Size" = Cohen_H)


table_output <- kable(results_final, format = "markdown") # This corresponds to Table 1
print(table_output)
