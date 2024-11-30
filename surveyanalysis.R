# __________________
# surveyanalysis.R
# created by Peace Gwam on 18Nov24
# analyzes and creates visualizations for self-service survey
# see appendix for more details
# __________________

#### libraries ####
# remember to install packages first if not already installed
library(tidyverse)
library(janitor)
library(showtext)
library(scales)

#### step 1: import cleaned surveys####
all_cohorts <- read_csv("all_cohorts.csv")
all_cohorts_broker <- read_csv("all_cohorts_broker.csv")
all_cohorts_acct <- read_csv("all_cohorts_acct.csv")
all_cohorts_ga <- read_csv("all_cohorts_ga.csv")

#change fonts of graphs
font_add_google("Lato", "lato")
showtext_auto()

#### step 2: produce brokerage (q9) and broker (q10) tables ####
# q9: generate table of collapsed variables and export them
brokerage_table <- all_cohorts %>% 
  filter(question == "q9") %>% 
  mutate(value = case_when(
    question == "q9" & value == 1 ~ "National and Regional Brokerages",
    question == "q9" & value == 2 ~ "Local Brokerages and Individual Agents",
    question == "q9" & value == 3 ~ "General Agencies",
  ))  

brokerage_table <- brokerage_table %>% 
  tabyl(value) %>% 
  adorn_totals() %>% 
  adorn_pct_formatting() %>% 
  mutate(n = round(n)) %>% 
  rename(Brokerages = value)

# export out as .csv
write_csv(brokerage_table, "brokerage_table.csv")


# collapse broker type (q10) 
broker_table <- all_cohorts %>% 
  filter(question == "q10") %>% 
  mutate(value = case_when(
    question == "q10" & value == 1 ~ "Brokers",
    question == "q10" & value == 2 ~ "Account Managers",
    question == "q10" & value == 3 ~ "General Agents",
  ))  

broker_table <- broker_table %>% 
  tabyl(value) %>% 
  adorn_totals() %>% 
  adorn_pct_formatting() %>% 
  mutate(n = round(n)) %>% 
  rename(Users = value)

# export out as .csv
write_csv(broker_table, "broker_table.csv")

#### step 3: calculate csat (q1) scores####
# that is, the % of top two-box scores

# create a list to loop through cohorts
csat <- list (
  csat = all_cohorts,
  csat_broker = all_cohorts_broker,
  csat_acct = all_cohorts_acct,
  csat_ga = all_cohorts_ga
)

filtered_csat <- lapply(csat, function(csat) {
  csat %>% 
    filter(question == "q1")
})

list2env(filtered_csat, envir = .GlobalEnv)

# calculate csat score overall and by user group
csat_score <- lapply(filtered_csat, function(csat) {
  if (is.data.frame(csat)) {  # Ensure each item is a dataframe
    csat %>%
      summarize(
        count = sum(value %in% c(4.0, 4.3, 5.0), na.rm = TRUE),  # handle NA values
        total = n()
      ) %>%
      mutate(csat = count / total)
  } else {
    NULL  # skip non-dataframe elements
  }
})

#### step 4: generate csat (q1) graphs ####
# create csat_hist
# first, just round down obs originally on 7 pt scale
csat <- csat %>%
  mutate(value = as.numeric(as.character(value))) %>%  # numeric
  mutate(value = round(value)) 

# update the factor levels to include all values, even those with no observations
csat <- csat %>%
  mutate(value = factor(value, levels = c(1, 2, 3, 4, 5)))  # ensure all levels are included

# include zero counts
csat_summary <- csat %>%
  group_by(value) %>%
  summarize(count = n(), .groups = "drop") %>%
  complete(value = factor(c(1, 2, 3, 4, 5), levels = c(1, 2, 3, 4, 5)), fill = list(count = 0))

limits <- c(1, 5)

# create the plot
csat_hist <- ggplot() +
  geom_bar(
    data = csat_summary,
    aes(x = value, y = count, fill = as.factor(value)),
    stat = "identity",
    width = 1  
  ) +
  geom_text(
    data = csat_summary,
    aes(x = value, y = count, label = count),
    vjust = -0.5,  # Position the text slightly above the bars
    size = 4,  # Adjust label size
    family = "lato"
  ) +
  scale_fill_manual(
    name = "Score",
    values = c(
      "1" = "#B22222",  # very unsatisfied
      "2" = "#FF4500",  # unsatisfied
      "3" = "#FFD700",  # neutral
      "4" = "#90EE90",  # satisfied
      "5" = "#4CBB17"   # very satisfied
    ),
    breaks = c(1, 2, 3, 4, 5),
    labels = c(
      "1 = very unsatisfied",
      "2 = unsatisfied",
      "3 = neutral",
      "4 = satisfied",
      "5 = very satisfied"
    )
  ) +
  coord_cartesian(xlim = limits) +
  theme(legend.position = "right") +
  labs(
    x = "Self-Service CSAT Score",
    y = "Observations",
    title = "Reported CSAT Score"
  ) +
  theme_light(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    text = element_text(family = "lato"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12)
  )

# save the plot
ggsave("csat_histogram.svg", plot = csat_hist, width = 16, height = 12, units = "cm", dpi = 300)


# for brokers
csat_broker <- csat_broker %>%
  mutate(value = as.numeric(as.character(value))) %>%  # numeric
  mutate(value = round(value)) 

# update the factor levels to include all values, even those with no observations
csat_broker <- csat_broker %>%
  mutate(value = factor(value, levels = c(1, 2, 3, 4, 5)))  # Ensure all levels are included

# include zero counts
csat_summary_broker <- csat_broker %>%
  group_by(value) %>%
  summarize(count = n(), .groups = "drop") %>%
  complete(value = factor(c(1, 2, 3, 4, 5), levels = c(1, 2, 3, 4, 5)), fill = list(count = 0))

limits <- c(1, 5) 
# create the plot
csat_hist_broker <- ggplot() +
  geom_bar(
    data = csat_summary_broker,
    aes(x = value, y = count, fill = as.factor(value)),
    stat = "identity",
    width = 1  
  ) +
  geom_text(
    data = csat_summary_broker,
    aes(x = value, y = count, label = count),
    vjust = -0.5,  # Position the text slightly above the bars
    size = 4,  # Adjust label size
    family = "lato"
  ) +
  scale_fill_manual(
    name = "Score",
    values = c(
      "1" = "#B22222",  # very unsatisfied
      "2" = "#FF4500",  # unsatisfied
      "3" = "#FFD700",  # neutral
      "4" = "#90EE90",  # satisfied
      "5" = "#4CBB17"   # very satisfied
    ),
    breaks = c(1, 2, 3, 4, 5),
    labels = c(
      "1 = very unsatisfied",
      "2 = unsatisfied",
      "3 = neutral",
      "4 = satisfied",
      "5 = very satisfied"
    )
  ) +
  coord_cartesian(xlim = limits) +
  theme(legend.position = "right") +
  labs(
    x = "Self-Service CSAT Score",
    y = "Observations",
    title = "Reported CSAT Score"
  ) +
  theme_light(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    text = element_text(family = "lato"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12)
  )

# save the plot
ggsave("csat_histogram_broker.svg", plot = csat_hist_broker, width = 16, height = 12, units = "cm", dpi = 300)


# for account managers
csat_acct <- csat_acct %>%
  mutate(value = as.numeric(as.character(value))) %>%  # numeric
  mutate(value = round(value)) 

# update the factor levels to include all values, even those with no observations
csat_acct <- csat_acct %>%
  mutate(value = factor(value, levels = c(1, 2, 3, 4, 5)))  # ensure all levels are included

# include zero counts
csat_summary_acct <- csat_acct %>%
  group_by(value) %>%
  summarize(count = n(), .groups = "drop") %>%
  complete(value = factor(c(1, 2, 3, 4, 5), levels = c(1, 2, 3, 4, 5)), fill = list(count = 0))

# create the plot
csat_hist_acct <- ggplot() +
  geom_bar(
    data = csat_summary_acct,
    aes(x = value, y = count, fill = as.factor(value)),
    stat = "identity",
    width = 1  
  ) +
  geom_text(
    data = csat_summary_acct,
    aes(x = value, y = count, label = count),
    vjust = -0.5,  # Position the text slightly above the bars
    size = 4,  # Adjust label size
    family = "lato"
  ) +
  scale_fill_manual(
    name = "Score",
    values = c(
      "1" = "#B22222",  # very unsatisfied
      "2" = "#FF4500",  # unsatisfied
      "3" = "#FFD700",  # neutral
      "4" = "#90EE90",  # satisfied
      "5" = "#4CBB17"   # very satisfied
    ),
    breaks = c(1, 2, 3, 4, 5),
    labels = c(
      "1 = very unsatisfied",
      "2 = unsatisfied",
      "3 = neutral",
      "4 = satisfied",
      "5 = very satisfied"
    )
  ) +
  coord_cartesian(xlim = limits) +
  theme(legend.position = "right") +
  labs(
    x = "Self-Service CSAT Score",
    y = "Observations",
    title = "Reported CSAT Score"
  ) +
  theme_light(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    text = element_text(family = "lato"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12)
  )

# save the plot
ggsave("csat_histogram_acct.svg", plot = csat_hist_acct, width = 16, height = 12, units = "cm", dpi = 300)

# for gas
csat_ga <- csat_ga %>%
  mutate(value = as.numeric(as.character(value))) %>%  # numeric
  mutate(value = round(value)) 

# update the factor levels to include all values, even those with no observations
csat_ga <- csat_ga %>%
  mutate(value = factor(value, levels = c(1, 2, 3, 4, 5)))  # ensure all levels are included

# include zero counts
csat_summary_ga <- csat_ga %>%
  group_by(value) %>%
  summarize(count = n(), .groups = "drop") %>%
  complete(value = factor(c(1, 2, 3, 4, 5), levels = c(1, 2, 3, 4, 5)), fill = list(count = 0))

limits <- c(1, 5)
# create the plot
csat_hist_ga <- ggplot() +
  geom_bar(
    data = csat_summary_ga,
    aes(x = value, y = count, fill = as.factor(value)),
    stat = "identity",
    width = 1  
  ) +
  geom_text(
    data = csat_summary_ga,
    aes(x = value, y = count, label = count),
    vjust = -0.5,  # Position the text slightly above the bars
    size = 4,  # Adjust label size
    family = "lato"
  ) +
  scale_fill_manual(
    name = "Score",
    values = c(
      "1" = "#B22222",  # very unsatisfied
      "2" = "#FF4500",  # unsatisfied
      "3" = "#FFD700",  # neutral
      "4" = "#90EE90",  # satisfied
      "5" = "#4CBB17"   # very satisfied
    ),
    breaks = c(1, 2, 3, 4, 5),
    labels = c(
      "1 = very unsatisfied",
      "2 = unsatisfied",
      "3 = neutral",
      "4 = satisfied",
      "5 = very satisfied"
    )
  ) +
  coord_cartesian(xlim = limits) +
  theme(legend.position = "right") +
  labs(
    x = "Self-Service CSAT Score",
    y = "Observations",
    title = "Reported CSAT Score"
  ) +
  theme_light(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    text = element_text(family = "lato"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12)
  )

# save the plot
ggsave("csat_histogram_ga.svg", plot = csat_hist_ga, width = 16, height = 12, units = "cm", dpi = 300)

#### step 5: generate csat (q1) tables and print output####
csat_table <- csat_summary %>% 
  mutate(value = case_when(
    value == 1 ~ "Very unsatisfied",
    value == 2 ~ "Unsatisfied",
    value == 3 ~ "Neutral",
    value == 4 ~ "Satisfied",
    value == 5 ~ "Very satisfied"
  )) %>%
  mutate(
    percentage = percent(count/sum(count))
  ) %>% 
  adorn_totals("row") %>% 
  mutate(
    percentage = ifelse(value == "Total", "100.0%", percentage)
  )
write_csv(csat_table, "csat_table.csv")
total_csat_score <- sum(csat_summary$count[csat_summary$value %in% c(4, 5)]) / sum(csat_summary$count)
print(paste("Total CSAT Score:", round(total_csat_score * 100, 1),"%"))


csat_table_acct <- csat_summary_acct %>% 
  mutate(value = case_when(
    value == 1 ~ "Very unsatisfied",
    value == 2 ~ "Unsatisfied",
    value == 3 ~ "Neutral",
    value == 4 ~ "Satisfied",
    value == 5 ~ "Very satisfied"
  )) %>%
  mutate(
    percentage = percent(count/sum(count))
  ) %>% 
  adorn_totals("row") %>% 
  mutate(
    percentage = ifelse(value == "Total", "100.0%", percentage)
  )
write_csv(csat_table_acct, "csat_table_acct.csv")
total_csat_score_acct <- sum(csat_summary_acct$count[csat_summary_acct$value %in% c(4, 5)]) / sum(csat_summary_acct$count)
print(paste("Total CSAT Score for Account Managers:", round(total_csat_score_acct * 100, 1),"%"))

csat_table_broker <- csat_summary_broker %>% 
  mutate(value = case_when(
    value == 1 ~ "Very unsatisfied",
    value == 2 ~ "Unsatisfied",
    value == 3 ~ "Neutral",
    value == 4 ~ "Satisfied",
    value == 5 ~ "Very satisfied"
  )) %>%
  mutate(
    percentage = percent(count/sum(count))
  ) %>% 
  adorn_totals("row") %>% 
  mutate(
    percentage = ifelse(value == "Total", "100.0%", percentage)
  )
write_csv(csat_table_broker, "csat_table_broker.csv")
total_csat_score_broker <- sum(csat_summary_broker$count[csat_summary_broker$value %in% c(4, 5)]) / sum(csat_summary_broker$count)
print(paste("Total CSAT Score for brokers:", round(total_csat_score_broker * 100, 1),"%"))

csat_table_ga <- csat_summary_ga %>% 
  mutate(value = case_when(
    value == 1 ~ "Very unsatisfied",
    value == 2 ~ "Unsatisfied",
    value == 3 ~ "Neutral",
    value == 4 ~ "Satisfied",
    value == 5 ~ "Very satisfied"
  )) %>%
  mutate(
    percentage = percent(count/sum(count))
  ) %>% 
  adorn_totals("row") %>% 
  mutate(
    percentage = ifelse(value == "Total", "100.0%", percentage)
  )
write_csv(csat_table_ga, "csat_table_ga.csv")
total_csat_score_ga <- sum(csat_summary_ga$count[csat_summary_ga$value %in% c(4, 5)]) / sum(csat_summary_ga$count)
print(paste("Total CSAT Score for GAs:", round(total_csat_score_ga * 100, 1),"%"))


#### step 6: calculate usability (q3) scores####
# using https://measuringu.com/umux-lite/ for this. 
# for more on this: https://uxpajournal.org/sus-umux-lite-ueq-s/. 
# code (mostly) stolen from: https://rpubs.com/paulamat/umux
# create a list of cohorts
uxum <- list(
  uxum = all_cohorts,
  uxum_broker = all_cohorts_broker,
  uxum_acct = all_cohorts_acct,
  uxum_ga = all_cohorts_ga
)

# filter for relevant questions
filtered_uxum <- lapply(uxum, function(uxum_score) {
  uxum_score %>%
    filter(question %in% c("q5_a", "q5_b")) %>%
    distinct()  # remove duplicates from individual dataframes
})

# perform transformations: mutate, calculate score, and compute final_score
processed_uxum <- lapply(filtered_uxum, function(uxum_score) {
  # ensure numeric conversion and rounding
  uxum_score <- uxum_score %>%
    mutate(
      value = as.numeric(as.character(value)),
      value = round(value)
    )
  
  # subtract 1 from numeric columns
  uxum_score[, sapply(uxum_score, is.numeric)] <- uxum_score[, sapply(uxum_score, is.numeric)] - 1
  
  # initialize and calculate score
  uxum_score$Score <- apply(uxum_score[, sapply(uxum_score, is.numeric)], 1, function(row) {
    (sum(row, na.rm = TRUE) / 8) * 100
  })
  
  # calculate final_score by summing q5_a and q5_b for each email_address and brokerage
  final_scores <- uxum_score %>%
    group_by(email_address, brokerage) %>%
    summarize(
      first_name = first(first_name),
      last_name = first(last_name),
      final_score = sum(Score, na.rm = TRUE),
      .groups = "drop"
    )
  
  final_scores
})

# combine all cohorts into a single dataframe
final_scores_combined <- bind_rows(processed_uxum)

# remove duplicates across all cohorts
final_scores_combined <- final_scores_combined %>%
  distinct()

list2env(processed_uxum, envir = .GlobalEnv)
#### step 7: generate usability (q3) graphs #### 

limits <- c(0, 100)

# create the histogram
uxum_hist <- ggplot() +
  geom_histogram(data = uxum, aes(final_score, fill = ..x..), binwidth = 11) +
  coord_cartesian(xlim = limits) +
  scale_fill_gradient2(name = "Score", low = "#B22222", mid = "#FFD700", high = "#4CBB17", midpoint = 63, limits = limits) +
  theme(legend.position = "right") +
  labs(x = "Usability Scores", y = "Observations", title = "Distribution of Usability Scores") +
  theme_light(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    text = element_text(family = "lato")
  )

# save the plot
ggsave("uxum_histogram.svg", plot = uxum_hist, width = 16, height = 12, units = "cm", dpi = 300)

uxum_hist_brokers <- ggplot() +
  geom_histogram(data = uxum_broker, aes(final_score, fill = ..x..), binwidth = 11) +
  coord_cartesian(xlim = limits) +
  scale_fill_gradient2(name = "Score", low = "#B22222", mid = "#FFD700", high = "#4CBB17", midpoint = 63, limits = limits) +
  theme(legend.position = "right") +
  labs(x = "Usability Scores", y = "Observations", title = "Distribution of Usability Scores") +
  theme_light(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    text = element_text(family = "lato")
  )

# save the plot
ggsave("uxum_histogram_brokers.svg", plot = uxum_hist_brokers, width = 16, height = 12, units = "cm", dpi = 300)

uxum_hist_acct <- ggplot() +
  geom_histogram(data = uxum_acct, aes(final_score, fill = ..x..), binwidth = 11) +
  coord_cartesian(xlim = limits) +
  scale_fill_gradient2(name = "Score", low = "#B22222", mid = "#FFD700", high = "#4CBB17", midpoint = 63, limits = limits) +
  theme(legend.position = "right") +
  labs(x = "Usability Scores", y = "Observations", title = "Distribution of Usability Scores") +
  theme_light(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    text = element_text(family = "lato")
  )

# save the plot
ggsave("uxum_histogram_acct.svg", plot = uxum_hist_acct, width = 16, height = 12, units = "cm", dpi = 300)

uxum_hist_ga<- ggplot() +
  geom_histogram(data = uxum_ga, aes(final_score, fill = ..x..), binwidth = 11) +
  coord_cartesian(xlim = limits) +
  scale_fill_gradient2(name = "Score", low = "#B22222", mid = "#FFD700", high = "#4CBB17", midpoint = 63, limits = limits) +
  theme(legend.position = "right") +
  labs(x = "Usability Scores", y = "Observations", title = "Distribution of Usability Scores") +
  theme_light(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    text = element_text(family = "lato")
  )

# save the plot
ggsave("uxum_histogram_ga.svg", plot = uxum_hist_ga, width = 16, height = 12, units = "cm", dpi = 300)
#### step 8: generate usability (q3) tables and print output ####
# create tables for shinydashboard 
# first, create buckets of observations
uxum <- uxum %>% 
  mutate(score_bin_all = cut(final_score, breaks = c(0, 70, 80, 90, 100)))

uxum_table <- uxum %>%
  count(score_bin_all) %>%
  mutate(percent = n / sum(n) * 100) %>%
  mutate(percent = sprintf("%.1f%%", percent)) 

uxum_table <- uxum_table %>%
  mutate(
    score_bin_all = case_when(
      score_bin_all == "(0,70]" ~ "0 - 70%",
      score_bin_all == "(70,80]" ~ "71 - 80%",
      score_bin_all == "(80,90]" ~ "81 - 90%",
      score_bin_all == "(90,100]" ~ "91 - 100%",
      TRUE ~ as.character(score_bin_all)
    )
  ) %>%
  rename(Observations = score_bin_all)


uxum_table <- uxum_table %>%
  bind_rows(
    tibble(
      Observations = "Total",
      n = sum(uxum_table$n),
      percent = "100.0%"
    )
  )

write_csv(uxum_table, "uxum_table.csv")

#for brokers
uxum_broker <- uxum_broker %>% 
  mutate(score_bin_all = cut(final_score, breaks = c(0, 70, 80, 90, 100)))

uxum_table_brokers <- uxum_broker %>%
  count(score_bin_all) %>%
  mutate(percent = n / sum(n) * 100) %>%
  mutate(percent = sprintf("%.1f%%", percent)) 

uxum_table_brokers <- uxum_table_brokers %>%
  mutate(
    score_bin_all = case_when(
      score_bin_all == "(0,70]" ~ "0 - 70%",
      score_bin_all == "(70,80]" ~ "71 - 80%",
      score_bin_all == "(80,90]" ~ "81 - 90%",
      score_bin_all == "(90,100]" ~ "91 - 100%",
      TRUE ~ as.character(score_bin_all)
    )
  ) %>%
  rename(Observations = score_bin_all)


uxum_table_brokers <- uxum_table_brokers %>%
  bind_rows(
    tibble(
      Observations = "Total",
      n = sum(uxum_table_brokers$n),
      percent = "100.0%"
    )
  )

write_csv(uxum_table_brokers, "uxum_table_brokers.csv")


#for account managers
uxum_acct <- uxum_acct %>% 
  mutate(score_bin_all = cut(final_score, breaks = c(0, 70, 80, 90, 100)))

uxum_table_acct <- uxum_acct %>%
  count(score_bin_all) %>%
  mutate(percent = n / sum(n) * 100) %>%
  mutate(percent = sprintf("%.1f%%", percent)) 

uxum_table_acct <- uxum_table_acct %>%
  mutate(
    score_bin_all = case_when(
      score_bin_all == "(0,70]" ~ "0 - 70%",
      score_bin_all == "(70,80]" ~ "71 - 80%",
      score_bin_all == "(80,90]" ~ "81 - 90%",
      score_bin_all == "(90,100]" ~ "91 - 100%",
      TRUE ~ as.character(score_bin_all)
    )
  ) %>%
  rename(Observations = score_bin_all)


uxum_table_acct <- uxum_table_acct %>%
  bind_rows(
    tibble(
      Observations = "Total",
      n = sum(uxum_table_acct$n),
      percent = "100.0%"
    )
  )

write_csv(uxum_table_acct, "uxum_table_acct.csv")


#for gas
uxum_ga <- uxum_ga %>% 
  mutate(score_bin_all = cut(final_score, breaks = c(0, 70, 80, 90, 100)))

uxum_table_ga <- uxum_ga %>%
  count(score_bin_all) %>%
  mutate(percent = n / sum(n) * 100) %>%
  mutate(percent = sprintf("%.1f%%", percent)) 

uxum_table_ga <- uxum_table_ga %>%
  mutate(
    score_bin_all = case_when(
      score_bin_all == "(0,70]" ~ "0 - 70%",
      score_bin_all == "(70,80]" ~ "71 - 80%",
      score_bin_all == "(80,90]" ~ "81 - 90%",
      score_bin_all == "(90,100]" ~ "91 - 100%",
      TRUE ~ as.character(score_bin_all)
    )
  ) %>%
  rename(Observations = score_bin_all)


uxum_table_ga <- uxum_table_ga %>%
  bind_rows(
    tibble(
      Observations = "Total",
      n = sum(uxum_table_ga$n),
      percent = "100.0%"
    )
  )

write_csv(uxum_table_ga, "uxum_table_ga.csv")
#### step 9: calculate likert (q7) scores####
# that is, the % of top two-box scores

# create a list to loop through cohorts
likert <- list (
  likert = all_cohorts,
  likert_broker = all_cohorts_broker,
  likert_acct = all_cohorts_acct,
  likert_ga = all_cohorts_ga
)

filtered_likert <- lapply(likert, function(likert) {
  likert %>% 
    filter(question == "q7")
})

list2env(filtered_likert, envir = .GlobalEnv)

# calculate likert score overall and by user group
likert_score <- lapply(filtered_likert, function(likert) {
  if (is.data.frame(likert)) {  # Ensure each item is a dataframe
    likert %>%
      summarize(
        count = sum(value %in% c(4.0, 4.3, 5.0), na.rm = TRUE),  # handle NA values
        total = n()
      ) %>%
      mutate(likert = count / total)
  } else {
    NULL  # skip non-dataframe elements
  }
})

#### step 10: generate likert (q7) graphs #### 
# create likert_hist
# first, just round down obs originally on 7 pt scale
likert <- likert %>%
  mutate(value = as.numeric(as.character(value))) %>%  # numeric
  mutate(value = round(value)) 

# update the factor levels to include all values, even those with no observations
likert <- likert %>%
  mutate(value = factor(value, levels = c(1, 2, 3, 4, 5)))  # ensure all levels are included

# include zero counts
likert_summary <- likert %>%
  group_by(value) %>%
  summarize(count = n(), .groups = "drop") %>%
  complete(value = factor(c(1, 2, 3, 4, 5), levels = c(1, 2, 3, 4, 5)), fill = list(count = 0))

limits <- c(1, 5)

# create the plot
likert_hist <- ggplot() +
  geom_bar(
    data = likert_summary,
    aes(x = value, y = count, fill = as.factor(value)),
    stat = "identity",
    width = 1  
  ) +
  geom_text(
    data = likert_summary,
    aes(x = value, y = count, label = count),
    vjust = -0.5,  # Position the text slightly above the bars
    size = 4,  # Adjust label size
    family = "lato"
  ) +
  scale_fill_manual(
    name = "Score",
    values = c(
      "1" = "#B22222",  # very unlikely
      "2" = "#FF4500",  # unlikely
      "3" = "#FFD700",  # neutral
      "4" = "#90EE90",  # likely
      "5" = "#4CBB17"   # very likely
    ),
    breaks = c(1, 2, 3, 4, 5),
    labels = c(
      "1 = very unlikely",
      "2 = unlikely",
      "3 = neutral",
      "4 = likely",
      "5 = very likely"
    )
  ) +
  coord_cartesian(xlim = limits) +
  theme(legend.position = "right") +
  labs(
    x = "Likelihood of Adoption",
    y = "Observations",
    title = "Reported Likelihood of Adoption"
  ) +
  theme_light(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    text = element_text(family = "lato"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12)
  )

# save the plot
ggsave("likert_histogram.svg", plot = likert_hist, width = 16, height = 12, units = "cm", dpi = 300)


# for brokers
likert_broker <- likert_broker %>%
  mutate(value = as.numeric(as.character(value))) %>%  # numeric
  mutate(value = round(value)) 

# update the factor levels to include all values, even those with no observations
likert_broker <- likert_broker %>%
  mutate(value = factor(value, levels = c(1, 2, 3, 4, 5)))  # Ensure all levels are included

# include zero counts
likert_summary_broker <- likert_broker %>%
  group_by(value) %>%
  summarize(count = n(), .groups = "drop") %>%
  complete(value = factor(c(1, 2, 3, 4, 5), levels = c(1, 2, 3, 4, 5)), fill = list(count = 0))

limits <- c(1, 5) 
# create the plot
likert_hist_broker <- ggplot() +
  geom_bar(
    data = likert_summary_broker,
    aes(x = value, y = count, fill = as.factor(value)),
    stat = "identity",
    width = 1  
  ) +
  geom_text(
    data = likert_summary_broker,
    aes(x = value, y = count, label = count),
    vjust = -0.5,  # Position the text slightly above the bars
    size = 4,  # Adjust label size
    family = "lato"
  ) +
  scale_fill_manual(
    name = "Score",
    values = c(
      "1" = "#B22222",  # very unlikely
      "2" = "#FF4500",  # unlikely
      "3" = "#FFD700",  # neutral
      "4" = "#90EE90",  # likely
      "5" = "#4CBB17"   # very likely
    ),
    breaks = c(1, 2, 3, 4, 5),
    labels = c(
      "1 = very unlikely",
      "2 = unlikely",
      "3 = neutral",
      "4 = likely",
      "5 = very likely"
    )
  ) +
  coord_cartesian(xlim = limits) +
  theme(legend.position = "right") +
  labs(
    x = "Likelihood of Adoption",
    y = "Observations",
    title = "Reported Likelihood of Adoption"
  ) +
  theme_light(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    text = element_text(family = "lato"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12)
  )

# save the plot
ggsave("likert_histogram_broker.svg", plot = likert_hist_broker, width = 16, height = 12, units = "cm", dpi = 300)


# for account managers
likert_acct <- likert_acct %>%
  mutate(value = as.numeric(as.character(value))) %>%  # numeric
  mutate(value = round(value)) 

# update the factor levels to include all values, even those with no observations
likert_acct <- likert_acct %>%
  mutate(value = factor(value, levels = c(1, 2, 3, 4, 5)))  # Ensure all levels are included

# include zero counts
likert_summary_acct <- likert_acct %>%
  group_by(value) %>%
  summarize(count = n(), .groups = "drop") %>%
  complete(value = factor(c(1, 2, 3, 4, 5), levels = c(1, 2, 3, 4, 5)), fill = list(count = 0))

# create the plot
likert_hist_acct <- ggplot() +
  geom_bar(
    data = likert_summary_acct,
    aes(x = value, y = count, fill = as.factor(value)),
    stat = "identity",
    width = 1  
  ) +
  geom_text(
    data = likert_summary_acct,
    aes(x = value, y = count, label = count),
    vjust = -0.5,  # Position the text slightly above the bars
    size = 4,  # Adjust label size
    family = "lato"
  ) +
  scale_fill_manual(
    name = "Score",
    values = c(
      "1" = "#B22222",  # very unlikely
      "2" = "#FF4500",  # unlikely
      "3" = "#FFD700",  # neutral
      "4" = "#90EE90",  # likely
      "5" = "#4CBB17"   # very likely
    ),
    breaks = c(1, 2, 3, 4, 5),
    labels = c(
      "1 = very unlikely",
      "2 = unlikely",
      "3 = neutral",
      "4 = likely",
      "5 = very likely"
    )
  ) +
  coord_cartesian(xlim = limits) +
  theme(legend.position = "right") +
  labs(
    x = "Likelihood of Adoption",
    y = "Observations",
    title = "Reported Likelihood of Adoption"
  ) +
  theme_light(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    text = element_text(family = "lato"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12)
  )

# save the plot
ggsave("likert_histogram_acct.svg", plot = likert_hist_acct, width = 16, height = 12, units = "cm", dpi = 300)

# for gas
likert_ga <- likert_ga %>%
  mutate(value = as.numeric(as.character(value))) %>%  # numeric
  mutate(value = round(value)) 

# update the factor levels to include all values, even those with no observations
likert_ga <- likert_ga %>%
  mutate(value = factor(value, levels = c(1, 2, 3, 4, 5)))  # Ensure all levels are included

# include zero counts
likert_summary_ga <- likert_ga %>%
  group_by(value) %>%
  summarize(count = n(), .groups = "drop") %>%
  complete(value = factor(c(1, 2, 3, 4, 5), levels = c(1, 2, 3, 4, 5)), fill = list(count = 0))

limits <- c(1, 5)
# create the plot
likert_hist_ga <- ggplot() +
  geom_bar(
    data = likert_summary_ga,
    aes(x = value, y = count, fill = as.factor(value)),
    stat = "identity",
    width = 1  
  ) +
  geom_text(
    data = likert_summary_ga,
    aes(x = value, y = count, label = count),
    vjust = -0.5,  # Position the text slightly above the bars
    size = 4,  # Adjust label size
    family = "lato"
  ) +
  scale_fill_manual(
    name = "Score",
    values = c(
      "1" = "#B22222",  # very unlikely
      "2" = "#FF4500",  # unlikely
      "3" = "#FFD700",  # neutral
      "4" = "#90EE90",  # likely
      "5" = "#4CBB17"   # very likely
    ),
    breaks = c(1, 2, 3, 4, 5),
    labels = c(
      "1 = very unlikely",
      "2 = unlikely",
      "3 = neutral",
      "4 = likely",
      "5 = very likely"
    )
  ) +
  coord_cartesian(xlim = limits) +
  theme(legend.position = "right") +
  labs(
    x = "Likelihood of Adoption",
    y = "Observations",
    title = "Reported Likelihood of Adoption"
  ) +
  theme_light(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    text = element_text(family = "lato"),
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 12)
  )

# save the plot
ggsave("likert_histogram_ga.svg", plot = likert_hist_ga, width = 16, height = 12, units = "cm", dpi = 300)

#### step 11: generate likert (q7) tables and print output ####
likert_table <- likert_summary %>% 
  mutate(value = case_when(
    value == 1 ~ "Very unlikely",
    value == 2 ~ "Unlikely",
    value == 3 ~ "Neutral",
    value == 4 ~ "Likely",
    value == 5 ~ "Very Likely"
  )) %>%
  mutate(
    percentage = percent(count/sum(count))
  ) %>% 
  adorn_totals("row") %>% 
  mutate(
    percentage = ifelse(value == "Total", "100.0%", percentage)
  )
write_csv(likert_table, "likert_table.csv")
total_likert_score <- sum(likert_summary$count[likert_summary$value %in% c(4, 5)]) / sum(likert_summary$count)
print(paste("Total Likert Score:", round(total_likert_score * 100, 1),"%"))


likert_table_acct <- likert_summary_acct %>% 
  mutate(value = case_when(
    value == 1 ~ "Very unlikely",
    value == 2 ~ "Unlikely",
    value == 3 ~ "Neutral",
    value == 4 ~ "Likely",
    value == 5 ~ "Very likely"
  )) %>%
  mutate(
    percentage = percent(count/sum(count))
  ) %>% 
  adorn_totals("row") %>% 
  mutate(
    percentage = ifelse(value == "Total", "100.0%", percentage)
  )
write_csv(likert_table_acct, "likert_table_acct.csv")
total_likert_score_acct <- sum(likert_summary_acct$count[likert_summary_acct$value %in% c(4, 5)]) / sum(likert_summary_acct$count)
print(paste("Total Likert Score for Account Managers:", round(total_likert_score_acct * 100, 1),"%"))

likert_table_broker <- likert_summary_broker %>% 
  mutate(value = case_when(
    value == 1 ~ "Very unlikely",
    value == 2 ~ "Unlikely",
    value == 3 ~ "Neutral",
    value == 4 ~ "Likely",
    value == 5 ~ "Very likely"
  )) %>%
  mutate(
    percentage = percent(count/sum(count))
  ) %>% 
  adorn_totals("row") %>% 
  mutate(
    percentage = ifelse(value == "Total", "100.0%", percentage)
  )
write_csv(likert_table_broker, "likert_table_broker.csv")
total_likert_score_broker <- sum(likert_summary_broker$count[likert_summary_broker$value %in% c(4, 5)]) / sum(likert_summary_broker$count)
print(paste("Total Likert Score for brokers:", round(total_likert_score_broker * 100, 1),"%"))

likert_table_ga <- likert_summary_ga %>% 
  mutate(value = case_when(
    value == 1 ~ "Very unlikely",
    value == 2 ~ "Unlikely",
    value == 3 ~ "Neutral",
    value == 4 ~ "Likely",
    value == 5 ~ "Very Likely"
  )) %>%
  mutate(
    percentage = percent(count/sum(count))
  ) %>% 
  adorn_totals("row") %>% 
  mutate(
    percentage = ifelse(value == "Total", "100.0%", percentage)
  )
write_csv(likert_table_ga, "likert_table_ga.csv")
total_likert_score_ga <- sum(likert_summary_ga$count[likert_summary_ga$value %in% c(4, 5)]) / sum(likert_summary_ga$count)
print(paste("Total Likert Score for GAs:", round(total_likert_score_ga * 100, 1),"%"))

#### step 12: additional inferential stats####
# is usability difference between GAs and non-GAs stat. sig.?

#first, create a df of uxum scores of non-GAs
uxum_non_ga <- bind_rows(uxum_acct, uxum_broker)


# Perform  t-test
welch_t_test <- t.test(
  uxum_non_ga$final_score, 
  uxum_ga$final_score, 
  alternative = "two.sided", 
  var.equal = FALSE          
)

# Combine the data into one dataframe
combined_uxum_data <- data.frame(
  score = c(uxum_non_ga$final_score, uxum_ga$final_score),
  group = c(rep("Non-GA", nrow(uxum_non_ga)), rep("GA", nrow(uxum_ga)))
)

# boxplot
ga_boxplot <- ggplot(combined_uxum_data, aes(x = group, y = score, fill = group)) +
  geom_boxplot() +
  labs(
    title = "Usability Scores: GAs and non-GAs",
    x = "Group",
    y = "Score"
  ) 
theme_light(base_size = 14) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    text = element_text(family = "lato")
  )

ggsave("ga_boxplot.svg", plot = ga_boxplot, width = 16, height = 12, units = "cm", dpi = 300)











