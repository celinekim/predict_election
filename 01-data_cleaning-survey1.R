#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from the Democracy Fund 
# and UCLA Nationscape ‘Full Data Set’
# Author: Celine Kim
# Data: 2 November 2020
# Contact: celinekim.kim@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("/Users/celinekim/Desktop/ps3")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("inputs/ns20200625/ns20200625.dta")
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(vote_2020,
         employment,
         gender,
         census_region,
         hispanic,
         race_ethnicity,
         age,
         state,
         foreign_born)

# Remove ages below 18 and divide into 4 age groups which are ages18to29, 
# ages30to44, ages45to59 and ages60plus.
reduced_data$agecopy <- as.integer(reduced_data$age)

reduced_data <-
  reduced_data %>%
  mutate(agecopy = agecopy-1)

reduced_data <-
  reduced_data %>%
  filter(agecopy >= 18)

reduced_data$age <- reduced_data$agecopy

reduced_data <-
  reduced_data %>%
  mutate(age_group =
           ifelse((age>=18 & age<=29), "ages18to29",
                  ifelse((age>=30 & age<=44), "ages30to44",
                         ifelse((age>=45 & age<=59), "ages45to59", "ages60plus")
                  )
           )
  )

reduced_data<-
  reduced_data %>%
  mutate(vote_trump_2020 =
           ifelse(vote_2020=="Donald Trump", 1, 0))

# If full-time, part-time or self-employed, is_working is 1 and 0 otherwise.
reduced_data <-
  reduced_data %>%
  mutate(is_working =
           ifelse(employment=="Full-time employed" | 
                    employment=="Part-time employed" | 
                    employment=="Self-employed", 1, 0))

# Gender is 1 if female and 2 if male.
reduced_data <-
  reduced_data %>%
  mutate(gender =
           ifelse(gender=="Female", 1, gender)) 

# census_region is 1 if Northeast, 2 if Midwest, 3 if South and 4 if West.
reduced_data <-
  reduced_data %>%
  mutate(census_region =
           ifelse(census_region=="Northeast", 1, census_region)) 

# If respondent is of Hispanic, Latino or Spanish origin, hispanic is 1.
reduced_data <-
  reduced_data %>%
  mutate(hispanic =
           ifelse(hispanic=="Not Hispanic", 0, hispanic)) %>%
  mutate(hispanic =
           ifelse(hispanic=="2" | hispanic=="3" | hispanic=="4" |
                    hispanic=="5" | hispanic=="6" | hispanic=="7"|
                    hispanic=="8" | hispanic=="9" | hispanic=="10" |
                    hispanic=="11" | hispanic=="12" | hispanic=="13" |
                    hispanic=="14" | hispanic=="15", 1, hispanic))

# If respondent's racial ethnicity is White, 1 or 0 otherwise.
reduced_data <-
  reduced_data %>%
  mutate(is_white =
           ifelse(race_ethnicity=="White", 1, 0))

# If respondent's racial ethnicity is Black or African American, 1 or 0 
# otherwise.
reduced_data <-
  reduced_data %>%
  mutate(is_black =
           ifelse(race_ethnicity=="Black, or African American", 1, 0))

# If respondent's racial ethnicity is American Indian, 1 or 0 otherwise.
reduced_data <-
  reduced_data %>%
  mutate(is_american_indian =
           ifelse(race_ethnicity=="American Indian or Alaska Native", 1, 0))

# If respondent's racial ethnicity is Asian or Pacific Islander, 1 or 0 
# otherwise.
reduced_data <-
  reduced_data %>%
  mutate(is_asian_or_pacific_islander =
           ifelse(race_ethnicity=="Asian (Asian Indian)" |
                    race_ethnicity=="Asian (Chinese)" |
                    race_ethnicity=="Asian (Filipino)" |
                    race_ethnicity=="Asian (Japanese)" |
                    race_ethnicity=="Asian (Korean)" |
                    race_ethnicity=="Asian (Vietnamese)" |
                    race_ethnicity=="Asian (Other)" |
                    race_ethnicity=="Pacific Islander (Native Hawaiian)" |
                    race_ethnicity=="Pacific Islander (Guamanian)" |
                    race_ethnicity=="Pacific Islander (Samoan)" |
                    race_ethnicity=="Pacific Islander (Other)", 1, 0))

# If respondent was born in the United States of America, us_born is 1.
reduced_data <-
  reduced_data %>%
  mutate(us_born =
           ifelse(foreign_born=="The United States", 1, 0))

reduced_data <-
  reduced_data %>%
  select(vote_trump_2020,
       is_working,
       gender,
       census_region,
       hispanic,
       is_white,
       is_black,
       is_american_indian,
       is_asian_or_pacific_islander,
       age_group,
       state,
       us_born)

# Saving the survey/sample data as a csv file in my working directory
write_csv(reduced_data, "outputs/survey_data.csv")

