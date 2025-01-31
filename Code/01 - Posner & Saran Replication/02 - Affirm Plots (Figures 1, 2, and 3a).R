# IF YOU CHOOSE TO RUN THIS SCRIPT INDIVIDUALLY, FIRST ASSIGN THE FILE PATH TO THE REPLICATION PACKAGE TO "user_file_path"
# user_file_path <-

library(tidyverse)
library(haven)
library(boot)

set.seed(2000)

# Affirm Plots

# Reading in Spamann/Klohn results and GPT experiment results
results_jsi <- read_dta(file.path(user_file_path, "Replication Package", "Data", "Spamann & Klohn", "dataverse_files", "data", "studentjudge_maindata.dta"))
results_gpti <- read_csv(file.path(user_file_path, "Replication Package", "Results", "GPT Individual Results.csv"))
results_jsi <- results_jsi %>% rename(Affirm = guilty)

# Assigning categorical variables to nationality and precedent so that Spamann & Klohn's data structure matches with ours
results_jsi <- results_jsi %>% 
  mutate(Condition = case_when(
    nationality == 1 & precedent == 1 ~ "Horvat_Sainovic", 
    nationality == 1 & precedent == 2 ~ "Horvat_Vasiljevic", 
    nationality == 2 & precedent == 1 ~ "Vukovic_Sainovic", 
    nationality == 2 & precedent == 2 ~ "Vukovic_Vasiljevic", 
    TRUE ~ NA_character_
  ))

# Renaming conditions in a format easier to interpret (e.g., "Sympathetic_Affirm" instead of "Horvat_Sainovic")
results_jsi <- results_jsi %>% 
  mutate(Condition_ = case_when(
    str_detect(Condition, "Horvat_Sainovic") ~ "Sympathetic_Affirm",
    str_detect(Condition, "Horvat_Vasiljevic") ~ "Sympathetic_Reverse",
    str_detect(Condition, "Vukovic_Sainovic") ~ "Unsympathetic_Affirm",
    str_detect(Condition, "Vukovic_Vasiljevic") ~ "Unsympathetic_Reverse",
    TRUE ~ NA_character_
  ))

# Splitting Spamann & Klohn's data into a judge dataframe (results_ji) and student dataframe (results_si)
results_ji <- results_jsi %>% filter(type == 1)
results_si <- results_jsi %>% filter(type == 2)

# Filtering results for each group and each condition (so that we can bootstrap the means and get confidence intervals)
results_gpti_sympathetic <- results_gpti %>% filter(str_detect(Condition_, "Sympathetic"))
results_gpti_unsympathetic <- results_gpti %>% filter(str_detect(Condition_, "Unsympathetic"))
results_gpti_p_affirm <- results_gpti %>% filter(str_detect(Condition_, "Affirm"))
results_gpti_p_reverse <- results_gpti %>% filter(str_detect(Condition_, "Reverse"))
results_gpti_sympathetic_p_affirm <- results_gpti %>% filter(Condition_ == "Sympathetic_Affirm")
results_gpti_sympathetic_p_reverse <- results_gpti %>% filter(Condition_ == "Sympathetic_Reverse")
results_gpti_unsympathetic_p_affirm <- results_gpti %>% filter(Condition_ == "Unsympathetic_Affirm")
results_gpti_unsympathetic_p_reverse <- results_gpti %>% filter(Condition_ == "Unsympathetic_Reverse")

results_ji_sympathetic <- results_ji %>% filter(str_detect(Condition_, "Sympathetic"))
results_ji_unsympathetic <- results_ji %>% filter(str_detect(Condition_, "Unsympathetic"))
results_ji_p_affirm <- results_ji %>% filter(str_detect(Condition_, "Affirm"))
results_ji_p_reverse <- results_ji %>% filter(str_detect(Condition_, "Reverse"))
results_ji_sympathetic_p_affirm <- results_ji %>% filter(Condition_ == "Sympathetic_Affirm")
results_ji_sympathetic_p_reverse <- results_ji %>% filter(Condition_ == "Sympathetic_Reverse")
results_ji_unsympathetic_p_affirm <- results_ji %>% filter(Condition_ == "Unsympathetic_Affirm")
results_ji_unsympathetic_p_reverse <- results_ji %>% filter(Condition_ == "Unsympathetic_Reverse")

results_si_sympathetic <- results_si %>% filter(str_detect(Condition_, "Sympathetic"))
results_si_unsympathetic <- results_si %>% filter(str_detect(Condition_, "Unsympathetic"))
results_si_p_affirm <- results_si %>% filter(str_detect(Condition_, "Affirm"))
results_si_p_reverse <- results_si %>% filter(str_detect(Condition_, "Reverse"))
results_si_sympathetic_p_affirm <- results_si %>% filter(Condition_ == "Sympathetic_Affirm")
results_si_sympathetic_p_reverse <- results_si %>% filter(Condition_ == "Sympathetic_Reverse")
results_si_unsympathetic_p_affirm <- results_si %>% filter(Condition_ == "Unsympathetic_Affirm")
results_si_unsympathetic_p_reverse <- results_si %>% filter(Condition_ == "Unsympathetic_Reverse")

# Function to calculate the mean affirmance 
bootstrap_affirm <- function(data, indices) {
  sample_data <- data[indices, ]  # Resample data with replacement
  return(mean(sample_data$Affirm))
}

perform_bootstrap <- function(data, group_name, condition_name) {
  set.seed(123)
  
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

# Performing boostraps for each group and each condition
gpt_results_affirm <- perform_bootstrap(results_gpti_p_affirm, "GPT-4o", "Affirm")
gpt_results_reverse <- perform_bootstrap(results_gpti_p_reverse, "GPT-4o", "Reverse")
gpt_results_sympathetic <- perform_bootstrap(results_gpti_sympathetic, "GPT-4o", "Sympathetic")
gpt_results_unsympathetic <- perform_bootstrap(results_gpti_unsympathetic, "GPT-4o", "Unsympathetic")
gpt_results_sympathetic_affirm <- perform_bootstrap(results_gpti_sympathetic_p_affirm, "GPT-4o", "Sympathetic_Affirm")
gpt_results_unsympathetic_affirm <- perform_bootstrap(results_gpti_unsympathetic_p_affirm, "GPT-4o", "Unsympathetic_Affirm")
gpt_results_sympathetic_reverse <- perform_bootstrap(results_gpti_sympathetic_p_reverse, "GPT-4o", "Sympathetic_Reverse")
gpt_results_unsympathetic_reverse <- perform_bootstrap(results_gpti_unsympathetic_p_reverse, "GPT-4o", "Unsympathetic_Reverse")

j_results_affirm <- perform_bootstrap(results_ji_p_affirm, "Judge", "Affirm")
j_results_reverse <- perform_bootstrap(results_ji_p_reverse, "Judge", "Reverse")
j_results_sympathetic <- perform_bootstrap(results_ji_sympathetic, "Judge", "Sympathetic")
j_results_unsympathetic <- perform_bootstrap(results_ji_unsympathetic, "Judge", "Unsympathetic")
j_results_sympathetic_affirm <- perform_bootstrap(results_ji_sympathetic_p_affirm, "Judge", "Sympathetic_Affirm")
j_results_unsympathetic_affirm <- perform_bootstrap(results_ji_unsympathetic_p_affirm, "Judge", "Unsympathetic_Affirm")
j_results_sympathetic_reverse <- perform_bootstrap(results_ji_sympathetic_p_reverse, "Judge", "Sympathetic_Reverse")
j_results_unsympathetic_reverse <- perform_bootstrap(results_ji_unsympathetic_p_reverse, "Judge", "Unsympathetic_Reverse")

s_results_affirm <- perform_bootstrap(results_si_p_affirm, "Student", "Affirm")
s_results_reverse <- perform_bootstrap(results_si_p_reverse, "Student", "Reverse")
s_results_sympathetic <- perform_bootstrap(results_si_sympathetic, "Student", "Sympathetic")
s_results_unsympathetic <- perform_bootstrap(results_si_unsympathetic, "Student", "Unsympathetic")
s_results_sympathetic_affirm <- perform_bootstrap(results_si_sympathetic_p_affirm, "Student", "Sympathetic_Affirm")
s_results_unsympathetic_affirm <- perform_bootstrap(results_si_unsympathetic_p_affirm, "Student", "Unsympathetic_Affirm")
s_results_sympathetic_reverse <- perform_bootstrap(results_si_sympathetic_p_reverse, "Student", "Sympathetic_Reverse")
s_results_unsympathetic_reverse <- perform_bootstrap(results_si_unsympathetic_p_reverse, "Student", "Unsympathetic_Reverse")

# Combining all the results into one data frame for each group
gpt_bootstrap_results <- rbind(
  gpt_results_affirm, 
  gpt_results_reverse,
  gpt_results_sympathetic, 
  gpt_results_unsympathetic,
  gpt_results_sympathetic_affirm,
  gpt_results_unsympathetic_affirm, 
  gpt_results_sympathetic_reverse,
  gpt_results_unsympathetic_reverse
)

j_bootstrap_results <- rbind(
  j_results_affirm, 
  j_results_reverse,
  j_results_sympathetic, 
  j_results_unsympathetic,
  j_results_sympathetic_affirm,
  j_results_unsympathetic_affirm, 
  j_results_sympathetic_reverse,
  j_results_unsympathetic_reverse
)

s_bootstrap_results <- rbind(
  s_results_affirm, 
  s_results_reverse,
  s_results_sympathetic, 
  s_results_unsympathetic,
  s_results_sympathetic_affirm,
  s_results_unsympathetic_affirm, 
  s_results_sympathetic_reverse,
  s_results_unsympathetic_reverse
)

# Binding together all the results
bootstrap_results <- rbind(gpt_bootstrap_results, j_bootstrap_results, s_bootstrap_results)

# Changing the string text for the final plots
bootstrap_results <- bootstrap_results %>% 
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

# Creating broad (e.g. "Sympathetic) and granular (e.g. "Sympathetic/P-Affirm") subsets of the results
gpt_bootstrap_results <- bootstrap_results %>% filter(Group == "GPT-4o")
gpt_bootstrap_results_broad <- gpt_bootstrap_results %>% filter(!Condition %in% c("Sympathetic_Affirm", "Sympathetic_Reverse", "Unsympathetic_Affirm", "Unsympathetic_Reverse"))
gpt_bootstrap_results_granular <- gpt_bootstrap_results %>% filter(Condition %in% c("Sympathetic_Affirm", "Sympathetic_Reverse", "Unsympathetic_Affirm", "Unsympathetic_Reverse"))

broad_bootstrap_results_comparison <- bootstrap_results %>% filter(!Condition %in% c("Sympathetic_Affirm", "Sympathetic_Reverse", "Unsympathetic_Affirm", "Unsympathetic_Reverse"))
granular_bootstrap_results_comparison <- bootstrap_results %>% filter(Condition %in% c("Sympathetic_Affirm", "Sympathetic_Reverse", "Unsympathetic_Affirm", "Unsympathetic_Reverse"))

# Writing results out so they can be used in further plots comparing to original results (i.e., Prompt engineering attempts)
write_csv(broad_bootstrap_results_comparison, file.path(user_file_path, "Replication Package", "Results", "broad_bootstrap_results_comparison.csv"))
write_csv(granular_bootstrap_results_comparison, file.path(user_file_path, "Replication Package", "Results", "granular_bootstrap_results_comparison.csv"))



# Creating Plots for GPT results

# FIGURE 1. Frequency of Affirming Convictions: GPT Results
GPT_affirm_plot_broad <- ggplot() + 
  geom_bar(data=gpt_bootstrap_results_broad, aes(x=Condition_, y=Mean_Affirm), fill = "black", stat = "identity", width = .6, position = position_dodge(width = 0.6)) + 
  geom_errorbar(data=gpt_bootstrap_results_broad, aes(x=Condition_, ymin = Lower_CI, ymax = Upper_CI), width = 0.1, linewidth = .75, color = 'orange') +
  labs(title = '', 
       y = 'Proportion of Decisions Affirming Conviction',
       x = '',
       color = '') +
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

ggsave(file.path(user_file_path, "Replication Package", "Graphs", "GPT Affirm Plot.png"), GPT_affirm_plot_broad, width = 12.5, height = 6.4)

# FIGURE 2. Frequency of Affirming Conviction: Comparison of GPT, Judge, and Student Results
bootstrap_broad_comparison_plot <- ggplot() + 
  geom_bar(data=broad_bootstrap_results_comparison, aes(x=Condition_, y=Mean_Affirm, fill=Group), stat = "identity", width = .6, position = position_dodge(width = 0.6)) + 
  geom_errorbar(data=broad_bootstrap_results_comparison, aes(x=Condition_, ymin = Lower_CI, ymax = Upper_CI, group = Group), width = 0.1, linewidth = .75, color = 'orange', position = position_dodge(width = 0.6)) +
  labs(title = '', 
       y = 'Proportion of Decisions Affirming Conviction',
       x = '',
       color = '') +
  scale_fill_manual(values = c("GPT-4o" = "black", "Judge" = "skyblue", "Student" = "darkgray")) + 
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

ggsave(file.path(user_file_path, "Replication Package", "Graphs", "Broad Comparison Plot.png"), bootstrap_broad_comparison_plot, width = 12.5, height = 6.4)

# FIGURE 3a. Comparative Results by Scenario
bootstrap_granular_comparison_plot <- ggplot() + 
  geom_bar(data=granular_bootstrap_results_comparison, aes(x=Condition_, y=Mean_Affirm, fill=Group), stat = "identity", width = .6, position = position_dodge(width = 0.6)) + 
  geom_errorbar(data=granular_bootstrap_results_comparison, aes(x=Condition_, ymin = Lower_CI, ymax = Upper_CI, group = Group), width = 0.1, linewidth = .75, color = 'orange', position = position_dodge(width = 0.6)) +
  labs(title = '', 
       y = 'Proportion of Decisions Affirming Conviction',
       x = '',
       color = '') +
  scale_fill_manual(values = c("GPT-4o" = "black", "Judge" = "skyblue", "Student" = "darkgray")) + 
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

ggsave(file.path(user_file_path, "Replication Package", "Graphs", "Granular Comparison Plot.png"), bootstrap_granular_comparison_plot, width = 12.5, height = 6.4)