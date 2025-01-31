# IF YOU CHOOSE TO RUN THIS SCRIPT INDIVIDUALLY, FIRST ASSIGN THE FILE PATH TO THE REPLICATION PACKAGE TO "user_file_path"
# user_file_path <-

library(tidyverse)
library(haven)
library(boot)

set.seed(2000)

# Reading in original judge, student, and GPT results
broad_sbs <- read_csv(file.path(user_file_path, "Replication Package", "Results", "broad_bootstrap_results_comparison.csv"))
granular_sbs <- read_csv(file.path(user_file_path, "Replication Package", "Results", "granular_bootstrap_results_comparison.csv"))

# CODE TO ASSESS PROMPT ENGINEERING RESULTS

# Creating a dataframe from values found in GPT Results (Panel) folder
GPT_panel <- data.frame(Group = "GPT-4o (Panel of Judges Method)",
                               Condition = c("Sympathetic_Affirm", "Sympathetic_Reverse", "Unsympathetic_Affirm", "Unsympathetic_Reverse"), 
                               Mean_Affirm = c(.72, .72, .72, .68), 
                               Lower_CI = NA, 
                               Upper_CI = NA)

# Renaming variables so they are appropriate text for the graph
GPT_panel <- GPT_panel %>% 
  mutate(Condition_ = case_when(
    str_detect(Condition, "Sympathetic_Affirm") ~ "When Defendant is \nSympathetic \nand Precedent Says \nto Affirm",
    str_detect(Condition, "Sympathetic_Reverse") ~ "When Defendant is \nSympathetic \nand Precedent Says \nto Reverse",
    str_detect(Condition, "Unsympathetic_Affirm") ~ "When Defendant is \nUnsympathetic \nand Precedent Says \nto Affirm",
    str_detect(Condition, "Unsympathetic_Reverse") ~ "When Defendant is \nUnsympathetic \nand Precedent Says \nto Reverse",
    str_detect(Condition, "Affirm") ~ "When Precedent \nSays to Affirm",
    str_detect(Condition, "Reverse") ~ "When Precedent \nSays to Reverse",
    str_detect(Condition, "Sympathetic") ~ "When Defendant \nis Sympathetic",
    str_detect(Condition, "Unsympathetic") ~ "When Defendant \nis Unsympathetic",
    TRUE ~ Condition
  ))

# Filtering to only select Judge and GPT results (not students)
gpt_judges_panel <- granular_sbs %>% filter(Group %in% c("Judge", "GPT-4o"))

# Renaming GPT-4o to "original" to indicate they were the results from our initial experiment
gpt_judges_panel <- gpt_judges_panel %>% mutate(Group=ifelse(Group == "GPT-4o", "GPT-4o (Original)", Group))
GPT_panel <- rbind(GPT_panel, gpt_judges_panel)

# Ordering the groups for graph appearance purposes
GPT_panel$Group <- factor(GPT_panel$Group, 
                            levels = c("GPT-4o (Original)", 
                                       "GPT-4o (Panel of Judges Method)",
                                       "Judge"))

# FIGURE 5. GPT vs. Judge Results Comparison (Panel of Judges Method)
GPT_panel_plot <- ggplot() + 
  geom_bar(data=GPT_panel, aes(x=Condition_, y=Mean_Affirm, fill=Group), stat = "identity", width = .6, position = position_dodge(width = 0.6)) + 
  geom_errorbar(data=GPT_panel, aes(x=Condition_, ymin = Lower_CI, ymax = Upper_CI, group = Group), width = 0.1, linewidth = .75, color = 'orange', position = position_dodge(width = 0.6)) +
  labs(title = '', 
       y = 'Proportion of Decisions Affirming Conviction',
       x = '',
       color = '') +
  scale_fill_manual(values = c("GPT-4o (Original)" = "black", "Judge" = "skyblue", "GPT-4o (Panel of Judges Method)" = "aquamarine4")) + 
  theme_light() + 
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 24),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18, margin = margin(t = 0, r = 30, b = 0, l = 0)),
        axis.title.y.right = element_text(size = 16, margin = margin(t = 0, r = 0, b = 0, l = 10)),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.caption = element_text(hjust = 0, size = 16),
        legend.title = element_blank(),
        panel.grid = element_blank()) +
  guides(color = guide_legend(reverse = TRUE))

ggsave(file.path(user_file_path, "Replication Package", "Graphs", "Prompt Engineering Panel of Judges Plot.png"), GPT_panel_plot, width = 12.5, height = 6.4)

# FIGURE 6. GPT vs. Judge Results Comparison (Explicit Instruction of Sympathy Method)

# Assessing Sympathy1 Prompt Results

folder_path <- file.path(user_file_path, "Replication Package", "Results", "Prompt Engineering", "GPT Results (Sympathy1)")

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

# Manual check of the GPT responses that mentions both affirm and reverse reveal that it was a reversal
# Updating that results to reflect it was not an affirmance
results_i <- results_i %>% mutate(AFfirm = ifelse(Filename %in% check$Filename, 0, Affirm))

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

# Reading in Spamann/Klöhn results (for judges and students) and binding our results to theirs
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

# Computing total proportion affirmed
summary_total <- results %>%
  group_by(Group) %>%
  summarize(Condition = "Total", Condition_ = "Total", Affirm = sum(Affirm), Total = sum(Total), percent_affirm = round(Affirm/Total,2)) %>%
  mutate(Reverse = Total - Affirm) %>%
  relocate(Reverse, .after = 4)

results <- results %>% select(!c(temp_condition_s, temp_condition_p))

# Binding all the results together
results <- rbind(results, summary_total, summary_conditions_p, summary_conditions_s)

write_csv(results, file.path(user_file_path, "Replication Package", "Results", "Prompt Engineering", "Group Comparison (Sympathy 1).csv"))
write_csv(results_i, file.path(user_file_path, "Replication Package", "Results", "Prompt Engineering", "GPT Individual Results (Sympathy1).csv"))

results_gpti_s <- results_i

results_gpti_s_sympathetic <- results_gpti_s %>% filter(str_detect(Condition_, "Sympathetic"))
results_gpti_s_unsympathetic <- results_gpti_s %>% filter(str_detect(Condition_, "Unsympathetic"))
results_gpti_s_p_affirm <- results_gpti_s %>% filter(str_detect(Condition_, "Affirm"))
results_gpti_s_p_reverse <- results_gpti_s %>% filter(str_detect(Condition_, "Reverse"))
results_gpti_s_sympathetic_p_affirm <- results_gpti_s %>% filter(Condition_ == "Sympathetic_Affirm")
results_gpti_s_sympathetic_p_reverse <- results_gpti_s %>% filter(Condition_ == "Sympathetic_Reverse")
results_gpti_s_unsympathetic_p_affirm <- results_gpti_s %>% filter(Condition_ == "Unsympathetic_Affirm")
results_gpti_s_unsympathetic_p_reverse <- results_gpti_s %>% filter(Condition_ == "Unsympathetic_Reverse")

# Function to calculate the mean affirmance 
bootstrap_affirm <- function(data, indices) {
  sample_data <- data[indices, ]  # Resample data with replacement
  return(mean(sample_data$Affirm))
}

perform_bootstrap <- function(data, group_name, condition_name) {
  set.seed(123)  # For reproducibility
  
  # Check if all values in the "Affirm" column are the same
  # Adding this in because GPT ALWAYS affirms when defendant is sympathetic and precedent says to affirm
  if (length(unique(data$Affirm)) == 1) {
    # If all values are the same (e.g., all Affirm = 1), set default values
    mean_affirm <- mean(data$Affirm)
    ci <- c(NA, NA)  # Confidence intervals can't be calculated
  } else {
    # Otherwise, run the bootstrap
    bootstrap_results <- boot(data = data, statistic = bootstrap_affirm, R = 1000)
    
    # Calculate the mean and 95% confidence intervals
    mean_affirm <- colMeans(bootstrap_results$t)
    ci <- boot.ci(bootstrap_results, type = "perc")$percent[4:5]  # 95% CI
  }
  
  # Return a data frame with results
  return(data.frame(
    Group = group_name,
    Condition = condition_name,
    Mean_Affirm = mean_affirm,
    Lower_CI = ci[1],
    Upper_CI = ci[2]
  ))
}

s_gpt_results_affirm <- perform_bootstrap(results_gpti_s_p_affirm, "GPT-4o", "Affirm")
s_gpt_results_reverse <- perform_bootstrap(results_gpti_s_p_reverse, "GPT-4o", "Reverse")
s_gpt_results_sympathetic <- perform_bootstrap(results_gpti_s_sympathetic, "GPT-4o", "Sympathetic")
s_gpt_results_unsympathetic <- perform_bootstrap(results_gpti_s_unsympathetic, "GPT-4o", "Unsympathetic")
s_gpt_results_sympathetic_affirm <- perform_bootstrap(results_gpti_s_sympathetic_p_affirm, "GPT-4o", "Sympathetic_Affirm")
s_gpt_results_unsympathetic_affirm <- perform_bootstrap(results_gpti_s_unsympathetic_p_affirm, "GPT-4o", "Unsympathetic_Affirm")
s_gpt_results_sympathetic_reverse <- perform_bootstrap(results_gpti_s_sympathetic_p_reverse, "GPT-4o", "Sympathetic_Reverse")
s_gpt_results_unsympathetic_reverse <- perform_bootstrap(results_gpti_s_unsympathetic_p_reverse, "GPT-4o", "Unsympathetic_Reverse")

# 4. Combine all the results into one data frame
s_gpt_bootstrap_results <- rbind(
  s_gpt_results_affirm, 
  s_gpt_results_reverse,
  s_gpt_results_sympathetic, 
  s_gpt_results_unsympathetic,
  s_gpt_results_sympathetic_affirm,
  s_gpt_results_unsympathetic_affirm, 
  s_gpt_results_sympathetic_reverse,
  s_gpt_results_unsympathetic_reverse
)

s_gpt_bootstrap_results <- s_gpt_bootstrap_results %>% 
  mutate(Condition_ = case_when(
    str_detect(Condition, "Sympathetic_Affirm") ~ "When Defendant is \nSympathetic \nand Precedent Says \nto Affirm",
    str_detect(Condition, "Sympathetic_Reverse") ~ "When Defendant is \nSympathetic \nand Precedent Says \nto Reverse",
    str_detect(Condition, "Unsympathetic_Affirm") ~ "When Defendant is \nUnsympathetic \nand Precedent Says \nto Affirm",
    str_detect(Condition, "Unsympathetic_Reverse") ~ "When Defendant is \nUnsympathetic \nand Precedent Says \nto Reverse",
    str_detect(Condition, "Affirm") ~ "When Precedent \nSays to Affirm",
    str_detect(Condition, "Reverse") ~ "When Precedent \nSays to Reverse",
    str_detect(Condition, "Sympathetic") ~ "When Defendant \nis Sympathetic",
    str_detect(Condition, "Unsympathetic") ~ "When Defendant \nis Unsympathetic",
    TRUE ~ Condition
  ))

s_gpt_bootstrap_results_broad <- s_gpt_bootstrap_results %>% filter(!Condition %in% c("Sympathetic_Affirm", "Sympathetic_Reverse", "Unsympathetic_Affirm", "Unsympathetic_Reverse"))
s_gpt_bootstrap_results_granular <- s_gpt_bootstrap_results %>% filter(Condition %in% c("Sympathetic_Affirm", "Sympathetic_Reverse", "Unsympathetic_Affirm", "Unsympathetic_Reverse"))

sympathy_df <- rbind(s_gpt_bootstrap_results_granular, gpt_judges_panel)
sympathy_df <- sympathy_df %>% mutate(Group = ifelse(Group == "GPT-4o", "GPT-4o (Instruction of Sympathy Method)", Group))
sympathy_df$Group <- factor(sympathy_df$Group, 
                            levels = c("GPT-4o (Original)", 
                                       "GPT-4o (Instruction of Sympathy Method)", 
                                       "Judge"))

sympathy1_plot <- ggplot() + 
  geom_bar(data=sympathy_df, aes(x=Condition_, y=Mean_Affirm, fill=Group), stat = "identity", width = .6, position = position_dodge(width = 0.6)) + 
  geom_errorbar(data=sympathy_df, aes(x=Condition_, ymin = Lower_CI, ymax = Upper_CI, group = Group), width = 0.1, linewidth = .75, color = 'orange', position = position_dodge(width = 0.6)) +
  labs(title = '', 
       y = 'Proportion of Decisions Affirming Conviction',
       x = '',
       color = '') +
  scale_fill_manual(values = c("GPT-4o (Original)" = "black", "Judge" = "skyblue", "GPT-4o (Instruction of Sympathy Method)" = "aquamarine4")) + 
  theme_light() + 
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 24),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18, margin = margin(t = 0, r = 30, b = 0, l = 0)),
        axis.title.y.right = element_text(size = 16, margin = margin(t = 0, r = 0, b = 0, l = 10)),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        plot.caption = element_text(hjust = 0, size = 16),
        legend.title = element_blank(),
        panel.grid = element_blank()) +
  guides(color = guide_legend(reverse = TRUE))

ggsave(file.path(user_file_path, "Replication Package", "Graphs", "Prompt Engineering Sympathy Plot.png"), sympathy1_plot, width = 12.5, height = 6.4)
