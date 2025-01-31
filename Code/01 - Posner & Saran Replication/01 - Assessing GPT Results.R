# IF YOU CHOOSE TO RUN THIS SCRIPT INDIVIDUALLY, FIRST ASSIGN THE FILE PATH TO THE REPLICATION PACKAGE TO "user_file_path"
# user_file_path <-

library(tidyverse)
library(stringi)

folder_path <- file.path(user_file_path, "Replication Package", "Results", "GPT Results")

# Get the .txt files (i.e. GPT's individual decisions)
file_list <- list.files(path = folder_path, pattern = "\\.txt$", full.names = TRUE)

# Initialize an empty dataframe
results_i <- data.frame(Filename = character(), ContainsAffirm = integer(), stringsAsFactors = FALSE)

# Loop through each file
for (file in file_list) {
  # Read the file content
  file_content <- readLines(file, warn = FALSE)
  file_content <- paste(file_content, collapse = " ")
  
  # Check if "Affirm" or "Reverse" is present
  contains_affirm <- ifelse(str_detect(file_content, regex("affirm", ignore_case = TRUE)), 1, 0)
  contains_reverse <- ifelse(str_detect(file_content, regex("reverse", ignore_case = TRUE)), 1, 0)
  
  # Add the result to the dataframe
  results_i <- rbind(results_i, data.frame(Filename = basename(file), Affirm = contains_affirm, Reverse = contains_reverse, Judgment = file_content))
}

# Check results which mention both 'reverse' and 'affirm'
check <- results_i %>% filter(Affirm + Reverse >= 2)

# Manual check of GPT's responses that mention both affirm and reverse reveal that they both were affirmances
# Updating those results to reflect that they were not reverses
results_i <- results_i %>% mutate(Reverse = ifelse(Filename %in% check$Filename, 0, Reverse))

# NOTE: If you decide to run your own experiment, make sure to manually check the whether the observations that 
# appear in the "check" dataframe are actually affirmances or reversals. Then, update the results_i dataframe accordingly.

# Creating a new column indicating condition
results_i <- results_i %>%
  mutate(Condition = case_when(
    str_detect(Filename, "Horvat_Sainovic") ~ "Horvat_Sainovic",
    str_detect(Filename, "Horvat_Vasiljevic") ~ "Horvat_Vasiljevic",
    str_detect(Filename, "Vukovic_Sainovic") ~ "Vukovic_Sainovic",
    str_detect(Filename, "Vukovic_Vasiljevic") ~ "Vukovic_Vasiljevic",
    TRUE ~ NA_character_
  ))

# Renaming conditions in a format easier to interpret (e.g., "Sympathetic_Affirm" instead of "Horvat_Sainovic")
results_i <- results_i %>% 
  mutate(Condition_ = case_when(
    str_detect(Condition, "Horvat_Sainovic") ~ "Sympathetic_Affirm",
    str_detect(Condition, "Horvat_Vasiljevic") ~ "Sympathetic_Reverse",
    str_detect(Condition, "Vukovic_Sainovic") ~ "Unsympathetic_Affirm",
    str_detect(Condition, "Vukovic_Vasiljevic") ~ "Unsympathetic_Reverse",
    TRUE ~ NA_character_
  ))

# Creating new dataframe with column which records proportion who affirm
results <- results_i %>%
  group_by(Condition, Condition_) %>%
  summarise(Affirm = sum(Affirm), 
            Total = n()) %>% 
  mutate(percent_affirm = Affirm/Total) %>% 
  mutate(Reverse = Total - Affirm) %>%
  relocate(Reverse, .after = 2)

# Assigning "GPT-4o" as group name
results <- results %>% 
  mutate(Group = "GPT-4o")

# Reading in Spamann/Klohn results (for judges and students) and binding our results to theirs
hs_results <- read_csv(file.path(user_file_path, "Replication Package", "Data", "Spamann & Klohn", "Spamann & Klohn Results.csv"))
results <- rbind(results, hs_results)

# Computing proportion affirmed for aggregated sympathy and aggregated precedent (i.e., sympathy irrespective of precedent and vice versa)
results <- results %>%
  mutate(temp_condition_s = case_when(
    str_detect(Condition_, "Sympathetic") ~ "Sympathetic",
    str_detect(Condition_, "Unsympathetic") ~ "Unsympathetic",
    TRUE ~ NA_character_
  )) %>%
  mutate(temp_condition_p = case_when(
    str_detect(Condition_, "Affirm") ~ "Affirm",
    str_detect(Condition_, "Reverse") ~ "Reverse",
    TRUE ~ NA_character_))

# Aggregated proportion affirmed for sympathy
summary_conditions_s <- results %>%
  group_by(Group, temp_condition_s) %>%
  summarize(
    Condition = unique(temp_condition_s),
    Condition_ = unique(temp_condition_s),
    Affirm = sum(Affirm),
    Total = sum(Total),
    percent_affirm = round(Affirm/Total,2),
  ) %>% 
  select(!temp_condition_s) %>%
  mutate(Reverse = Total - Affirm) %>%
  relocate(Reverse, .after = 4)

# Aggregated proportion affirmed for precedent
summary_conditions_p <- results %>%
  group_by(Group, temp_condition_p) %>%
  summarize(
    Condition = unique(temp_condition_p),
    Condition_ = unique(temp_condition_p),
    Affirm = sum(Affirm),
    Total = sum(Total),
    percent_affirm = round(Affirm/Total,2),
  ) %>% 
  select(!temp_condition_p) %>%
  mutate(Reverse = Total - Affirm) %>%
  relocate(Reverse, .after = 4)

# Total proportion affirmed
summary_total <- results %>%
  group_by(Group) %>%
  summarize(Condition = "Total", Condition_ = "Total", Affirm = sum(Affirm), Total = sum(Total), percent_affirm = round(Affirm/Total,2)) %>%
  mutate(Reverse = Total - Affirm) %>%
  relocate(Reverse, .after = 4)

results <- results %>% select(!c(temp_condition_s, temp_condition_p))

# Binding all the results together
results <- rbind(results, summary_total, summary_conditions_p, summary_conditions_s)

# ADDING IN MENTIONS OF KEY TERMS TO DATAFRAME (policy, precedent, statute)

normalize_text <- function(text) {
  text %>%
    stri_trans_general("Latin-ASCII") %>%
    tolower()
}

results_i <- results_i %>%
  mutate(mentioned_precedent = ifelse(str_detect(normalize_text(Judgment), 
                                                      regex("sainovic|vasiljevic|precedent", ignore_case = TRUE)), 1, 0)) %>%
  mutate(mentioned_statute = ifelse(str_detect(normalize_text(Judgment), 
                                                    regex("statute|article", ignore_case = TRUE)), 1, 0)) %>%
  mutate(mentioned_policy = ifelse(str_detect(normalize_text(Judgment), 
                                                   regex("policy", ignore_case = TRUE)), 1, 0))

write_csv(results, file.path(user_file_path, "Replication Package", "Results", "Group Comparison.csv"))
write_csv(results_i, file.path(user_file_path, "Replication Package", "Results", "GPT Individual Results.csv"))

# Checking if there was any reasonable difference between those prompts which had the law comment and those that didn't (there wasn't)
# lc <- results_i %>%
#   mutate(LC_Classification = ifelse(str_detect(Filename, "NLC"), "NLC", "LC"))
# lc_compare <- lc %>%
#   group_by(LC_Classification) %>%
#   summarise(Affirm = sum(Affirm), 
#             Total = n()) %>%
#   mutate(percent_affirm = Affirm/Total)
