# __________________
# surveycleaning.R
# created by Peace Gwam on 01Nov24
# imports all self-service data and preps it for analysis
# see appendix for more details
# __________________

#### libraries ####
# remember to install packages first!
library(tidyverse)
library(janitor)

#### step 1: import .csv ####
# update for subsequent cohorts
cohort_names <- paste0("cohort", 1:4)
cohorts <- lapply(cohort_names, function(name) {
  read_csv(paste0(name, ".csv")) %>%
    clean_names() %>%
    slice(-1) # remove header row if present
})
names(cohorts) <- cohort_names

#### step 2: rename columns and remove metadata ####
cohorts <- lapply(cohorts, function(cohort) {
  cohort %>%
    # remove metadata and open-text columns
    select(-custom_data_3, 
           -custom_data_4, 
           -custom_data_6, 
           -custom_data_7, 
           -custom_data_8,
           -respondent_id,
           -collector_id, 
           -start_date, 
           -end_date, 
           -ip_address,
           -optional_how_could_we_enhance_your_experience,
           -optional_what_would_need_to_change_for_you_to_bring_more_of_your_business_to_the_beam_platform,
           -is_there_anything_else_you_would_like_to_share) %>%
    # rename analytics columns
    rename(
      mde = custom_data_1,
      brokerage = custom_data_2,
      salesforce_id = custom_data_5
    ) %>%
    # rename question columns
    rename_with(~ ifelse(. %in% c(
      "how_satisfied_are_you_with_your_overall_experience_using_the_beam_platform",
      "how_satisfied_are_you_with_beam_s_combined_digital_quoting_winning_signing_and_enrolling_process"
    ), "q1", .)) %>%
    rename_with(~ ifelse(. == "how_satisfied_are_you_with_your_quoting_experience_on_the_beam_platform", "q2", .)) %>%
    rename_with(~ ifelse(. == "how_satisfied_are_you_with_your_signing_experience_on_the_beam_platform", "q3", .)) %>%
    rename(
      q5_a = please_indicate_the_extent_to_which_you_agree_or_disagree_with_each_statement,
      q6 = how_does_the_beam_platform_compare_to_other_quoting_tools_you_have_used,
      q7 = how_likely_are_you_to_use_the_beam_platform_for_your_next_client,
      q9 = which_of_the_following_best_describes_your_brokerage,
      q10 = which_of_the_following_best_describes_your_role,
      q11 = would_you_like_to_participate_in_future_research_studies_with_beam_participants_will_receive_a_gift_card_upon_completion_of_each_study
    ) %>%
    rename_with(~ ifelse(. %in% c("x18", "x20"), "q5_b", .))
})

# export cleaned cohorts to the global environment
list2env(setNames(cohorts, cohort_names), envir = .GlobalEnv)

#### step 3: recode "other" observations and prepare wide data ####
# recode specific observations in cohort2
cohorts[["cohort2"]] <- cohorts[["cohort2"]] %>%
  filter(email_address != "beth@texasinsurancebrokerage.com") %>%
  mutate(q9 = case_when(
    x23 == "Independent Agent with multiple locations" ~ 2,
    x23 == "One CA office licensed in multiple States" ~ 2,
    TRUE ~ as.numeric(q9)
  ))

# remove all open-text columns (columns starting with 'x') and convert question columns to numeric
cohorts <- lapply(cohorts, function(cohort) {
  cohort %>%
    select(-any_of(c("x23", "x25", "x27"))) %>%
    mutate(across(starts_with("q"), as.numeric))
})

# export updated cohorts to the global environment
list2env(setNames(cohorts, cohort_names), envir = .GlobalEnv)

#### step 4: convert scales for consistency ####
# for q1, q5_a, q5_b, q7
# using https://measuringu.com/convert-point-scales/ as guide
cohorts[c("cohort1", "cohort2")] <- lapply(cohorts[c("cohort1", "cohort2")], function(cohort) {
  cohort %>%
    mutate(
      q1 = (q1 - 1) * (4 / 6) + 1,
      q5_a = (q5_a - 1) * (4 / 6) + 1,
      q5_b = (q5_b - 1) * (4 / 6) + 1,
      q7 = (q7 - 1) * (4 / 6) + 1
    )
})

list2env(setNames(cohorts, cohort_names), envir = .GlobalEnv)

cohorts <- lapply(cohorts, function(cohort) {
  cohort %>%
    mutate(across(starts_with("q"), ~ round(as.numeric(.), 1)))  # round to one decimal place while keeping numeric
})

list2env(setNames(cohorts, cohort_names), envir = .GlobalEnv)

#### step 5: collapse key variables and create dfs for analysis ####
all_cohorts <- bind_rows(cohorts)

# make missing obs in q2 and q3 0
all_cohorts <- all_cohorts %>%
  mutate(
    q2 = if_else(is.na(q2), 0, q2),
    q3 = if_else(is.na(q3), 0, q3)
  )

# collapse brokerage type (q9)
all_cohorts <- all_cohorts %>%
  mutate(q9 = case_when(
    q9 %in% c(1, 2) ~ 1,  # national and regional brokerages
    q9 %in% c(3, 4, 5) ~ 2,  # local and indiv agents
    q9 %in% c(6, 7) ~ 3,  # gas
    TRUE ~ q9 
  ))

# collapse broker type (q10)
all_cohorts <- all_cohorts %>%
  mutate(q10 = case_when(
    q10 %in% c(1, 4) ~ 1,  # broker and broker leaders
    q10 %in% c(2, 3, 6, 7) ~ 2,  # acct manager, etc.
    q10 %in% c(5) ~ 3,  # gas
    TRUE ~ q10
  ))

# create separate dfs by user type
all_cohorts_broker <- all_cohorts %>% 
  filter(q10 == 1)

all_cohorts_acct <- all_cohorts %>% 
  filter(q10 == 2)

all_cohorts_ga <- all_cohorts %>% 
  filter (q10 == 3)
#### step 6: make data tidy and export .csvs ####
# make data long
all_cohorts <- all_cohorts %>% 
  pivot_longer(
    cols = starts_with("q"),
    names_to = "question")

all_cohorts_broker <- all_cohorts_broker %>% 
  pivot_longer(
    cols = starts_with("q"),
    names_to = "question")

all_cohorts_acct <- all_cohorts_acct %>% 
  pivot_longer(
    cols = starts_with("q"),
    names_to = "question")

all_cohorts_ga <- all_cohorts_ga %>% 
  pivot_longer(
    cols = starts_with("q"),
    names_to = "question")

write_csv(all_cohorts, "all_cohorts.csv")
write_csv(all_cohorts_broker, "all_cohorts_broker.csv")
write_csv(all_cohorts_acct, "all_cohorts_acct.csv")
write_csv(all_cohorts_ga, "all_cohorts_ga.csv")




