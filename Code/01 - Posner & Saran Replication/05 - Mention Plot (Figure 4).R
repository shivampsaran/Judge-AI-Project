# IF YOU CHOOSE TO RUN THIS SCRIPT INDIVIDUALLY, FIRST ASSIGN THE FILE PATH TO THE REPLICATION PACKAGE TO "user_file_path"
# user_file_path <-

library(tidyverse)
library(haven)
library(stringi)
library(stringr)
library(boot)

# Mention Plot

# Reading in Spamann & Klohn results and GPT experiment results
mention_jsi <- read_dta(file.path(user_file_path, "Replication Package", "Data", "Spamann & Klohn", "dataverse_files", "data", "studentjudge_maindata.dta"))
mention_gpti <- read_csv(file.path(user_file_path, "Replication Package", "Results", "GPT Individual Results.csv"))
mention_ji <- mention_jsi %>% filter(type == 1)
mention_si <- mention_jsi %>% filter(type == 2)

# Function to calculate the means of "Precedent", "Statute", and "Policy"
bootstrap_means <- function(data, indices) {
  sample_data <- data[indices, ]  # Resample data with replacement
  return(c(mean(sample_data$mentioned_precedent, na.rm = TRUE), 
           mean(sample_data$mentioned_statute, na.rm = TRUE), 
           mean(sample_data$mentioned_policy, na.rm = TRUE)))  # Calculate means
}

# Function to perform bootstrap for each group
perform_bootstrap <- function(data, group_name) {
  set.seed(123)  # For reproducibility
  bootstrap_results <- boot(data = data, statistic = bootstrap_means, R = 1000)
  
  # Calculate means and 95% confidence intervals
  means <- colMeans(bootstrap_results$t)
  ci_precedent <- boot.ci(bootstrap_results, index = 1, type = "perc")$percent[4:5]
  ci_statute <- boot.ci(bootstrap_results, index = 2, type = "perc")$percent[4:5]
  ci_policy <- boot.ci(bootstrap_results, index = 3, type = "perc")$percent[4:5]
  
  if (means[3] == 0) {
    means[3] <- 0.005  # Set to a small value above 0 so it appears on graphs
  }
  
  
  # Create a data frame with results
  data.frame(
    Group = group_name,
    Category = c("Precedent", "Statute", "Policy"),
    Mean = means,
    Lower_CI = c(ci_precedent[1], ci_statute[1], ci_policy[1]),
    Upper_CI = c(ci_precedent[2], ci_statute[2], ci_policy[2])
  )
}

# Bootstrap for each group
results_gpti <- perform_bootstrap(mention_gpti, "GPT-4o")
results_ji <- perform_bootstrap(mention_ji, "Judge")
results_si <- perform_bootstrap(mention_si, "Student")

combined_results <- rbind(results_gpti, results_ji, results_si)
combined_results <- combined_results %>% mutate(scaled = 0)

# Adding number of words for each judgment

mention_gpti <- mention_gpti %>% 
  mutate(num_words = sapply(strsplit(Judgment, "\\s+"), length))
mention_ji <- mention_ji %>% 
  mutate(num_words = sapply(strsplit(judgmentreasons, "\\s+"), length))
mention_si <- mention_si %>% 
  mutate(num_words = sapply(strsplit(judgmentreasons, "\\s+"), length))

mention_gpti <- mention_gpti %>% 
  mutate(mean_num_words = mean(num_words))
mention_ji <- mention_ji %>% 
  mutate(mean_num_words = mean(num_words))
mention_si <- mention_si %>% 
  mutate(mean_num_words = mean(num_words))

# Creating new mention variable which is sclaed to number of words
mention_gpti <- mention_gpti %>%
  mutate(mentioned_statute_sc = mentioned_statute / mean_num_words,
         mentioned_policy_sc = mentioned_policy / mean_num_words,
         mentioned_precedent_sc = mentioned_precedent / mean_num_words)

mention_ji <- mention_ji %>%
  mutate(mentioned_statute_sc = mentioned_statute / mean_num_words,
         mentioned_policy_sc = mentioned_policy / mean_num_words,
         mentioned_precedent_sc = mentioned_precedent / mean_num_words)

mention_si <- mention_si %>%
  mutate(mentioned_statute_sc = mentioned_statute / mean_num_words,
         mentioned_policy_sc = mentioned_policy / mean_num_words,
         mentioned_precedent_sc = mentioned_precedent / mean_num_words)


# Function to calculate the means of "Precedent", "Statute", and "Policy" for when scaled to words
bootstrap_means_sc <- function(data, indices) {
  sample_data <- data[indices, ]  # Resample data with replacement
  return(c(mean(sample_data$mentioned_precedent_sc, na.rm = TRUE), 
           mean(sample_data$mentioned_statute_sc, na.rm = TRUE), 
           mean(sample_data$mentioned_policy_sc, na.rm = TRUE)))  # Calculate means
}

# Function to perform bootstrap for each group
perform_bootstrap_sc <- function(data, group_name) {
  set.seed(123)  # For reproducibility
  bootstrap_results <- boot(data = data, statistic = bootstrap_means_sc, R = 1000)
  
  # Calculate means and 95% confidence intervals
  means <- colMeans(bootstrap_results$t)
  ci_precedent <- boot.ci(bootstrap_results, index = 1, type = "perc")$percent[4:5]
  ci_statute <- boot.ci(bootstrap_results, index = 2, type = "perc")$percent[4:5]
  ci_policy <- boot.ci(bootstrap_results, index = 3, type = "perc")$percent[4:5]
  
  if (means[3] == 0) {
    means[3] <- 0.00005  # Set to a small value above 0
  }
  
  
  # Create a data frame with results
  data.frame(
    Group = group_name,
    Category = c("Precedent", "Statute", "Policy"),
    Mean = means,
    Lower_CI = c(ci_precedent[1], ci_statute[1], ci_policy[1]),
    Upper_CI = c(ci_precedent[2], ci_statute[2], ci_policy[2])
  )
}

# Bootstrap for each group
results_gpti_sc <- perform_bootstrap_sc(mention_gpti, "GPT-4o")
results_ji_sc <- perform_bootstrap_sc(mention_ji, "Judge")
results_si_sc <- perform_bootstrap_sc(mention_si, "Student")

combined_results_sc <- rbind(results_gpti_sc, results_ji_sc, results_si_sc)
combined_results_sc <- combined_results_sc %>% mutate(scaled = 1)

combined_results_final <- rbind(combined_results, combined_results_sc)

# Setting confidence intervals that are 0 to NA so they don't mess up the graph
combined_results_final <- combined_results_final %>% mutate(Lower_CI = ifelse(Lower_CI == 0, NA, Lower_CI))
combined_results_final <- combined_results_final %>% mutate(Upper_CI = ifelse(Upper_CI == 0, NA, Upper_CI))

combined_results_unsc <- combined_results_final %>% filter(scaled == 0)

# MENTION PLOTS

# FIGURE 4. Proportion of Decisions Mentioning Policy/Precedent/Statue
mention_bootstrap <- ggplot(combined_results_final, aes(x = Group, y = Mean, fill = Group)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.1, linewidth = .75, color = 'orange') +
  facet_grid(scaled ~ Category, scales = "free_y", labeller = labeller(scaled = c("0" = "Unscaled", "1" = "Scaled"))) +
  labs(title = '',
       y = 'Proportion Mentioned',
       x = '',
       color = '') +
  scale_fill_manual(values = c("GPT-4o" = "black", "Judge" = "skyblue", "Student" = "darkgray")) +
  theme_light() + 
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 24),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18, margin = margin(t = 0, r = 30, b = 0, l = 0)),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.position = 'none',
        plot.caption = element_text(hjust = 0, size = 16),
        panel.grid = element_blank(), 
        strip.background = element_blank(),  # Remove background color
        strip.text = element_text(color = "black", size = 18)) +
  guides(color = guide_legend(reverse = TRUE))

ggsave(file.path(user_file_path, "Replication Package", "Graphs", "Mentions Plot.png"), mention_bootstrap, width = 10, height = 6.4)

# FIGURE 4. Proportion of Decisions Mentioning Policy/Precedent/Statue (JUST UNSCALED VERSION)
mention_bootstrap_unscaled <- ggplot(combined_results_unsc, aes(x = Group, y = Mean, fill = Group)) + 
  geom_bar(stat = "identity", width = 0.5) + 
  geom_errorbar(aes(ymin = Lower_CI, ymax = Upper_CI), width = 0.1, linewidth = .75, color = 'orange') +
  facet_grid(~ Category) +
  labs(title = '',
       y = 'Proportion Mentioned',
       x = '',
       color = '') +
  scale_fill_manual(values = c("GPT-4o" = "black", "Judge" = "skyblue", "Student" = "darkgray")) +
  theme_light() + 
  theme(text = element_text(family = "Times New Roman"),
        plot.title = element_text(size = 24),
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18, margin = margin(t = 0, r = 30, b = 0, l = 0)),
        axis.text.x = element_blank(),
        axis.text.y = element_text(size = 18),
        plot.caption = element_text(hjust = 0, size = 16),
        panel.grid = element_blank(), 
        strip.background = element_blank(),  # Remove background color
        strip.text = element_text(color = "black", size = 18)) +
  guides(color = guide_legend(reverse = TRUE))

ggsave(file.path(user_file_path, "Replication Package", "Graphs", "Mentions Plot (Unscaled).png"), mention_bootstrap_unscaled, width = 10, height = 6.4)

