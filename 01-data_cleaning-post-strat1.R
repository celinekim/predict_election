#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from the American 
# Community Surveys
# Author: Celine Kim
# Data: 2 November 2020
# Contact: celinekim.kim@mail.utoronto.ca
# License: MIT

#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("/Users/celinekim/Desktop/ps3")
raw_data <- read_dta("inputs/usa_00003.dta")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(region,
         sex,
         age,
         race,
         hispan,
         empstat,
         bpl,
         stateicp)
         
reduced_data <-
  reduced_data %>%
  filter(age != "less than 1 year old")

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

reduced_data <-
  reduced_data %>%
  filter(region != "Military/Military reservations") %>%
  filter(region != "PUMA boundaries cross state lines-1% sample") %>%
  filter(region != "State not identified") %>%
  filter(region != "Not identified")

reduced_data <-
  reduced_data %>%
  mutate(census_region =
           ifelse(region=="new england division" |
                    region=="middle atlantic division", 1, region)) %>%
  mutate(census_region =
           ifelse(census_region==4 | census_region==5, 2, census_region)) %>%
  mutate(census_region =
           ifelse(census_region==7 | census_region==8 | census_region==9, 3, census_region)) %>%
  mutate(census_region =
           ifelse(census_region==11 | census_region==12, 4, census_region))

reduced_data <-
  reduced_data %>%
  mutate(gender =
           ifelse(sex=="female", 100, sex)) %>%
  mutate(gender =
           ifelse(gender==1, 2, gender)) %>%
  mutate(gender =
           ifelse(gender==100, 1, gender))

reduced_data <-
  reduced_data %>%
  mutate(is_white =
           ifelse(race=="white", 1, 0))

reduced_data <-
  reduced_data %>%
  mutate(is_black =
           ifelse(race=="black/african american/negro", 1, 0))

reduced_data <-
  reduced_data %>%
  mutate(is_american_indian =
           ifelse(race=="american indian or alaska native", 1, 0))

reduced_data <-
  reduced_data %>%
  mutate(is_asian_or_pacific_islander =
           ifelse(race=="chinese" | race=="japanese" |
                    race=="other asian or pacific islander", 1, 0))

reduced_data <-
  reduced_data %>%
  mutate(hispanic =
           ifelse(hispan=="mexican" | hispan=="puerto rican" |
                    hispan=="cuban" | hispan=="other", 1, 0))

reduced_data <-
  reduced_data %>%
  mutate(hispanic =
           ifelse(hispan=="mexican" | hispan=="puerto rican" |
                    hispan=="cuban" | hispan=="other", 1, 0))

reduced_data <-
  reduced_data %>%
  mutate(is_working =
           ifelse(empstat=="employed", 1, 0))

reduced_data$us_born <- as.integer(reduced_data$bpl)

reduced_data <-
  reduced_data %>%
  mutate(us_born =
           ifelse((us_born <= 58 & us_born >= 1), 1, 0))

reduced_data$state <- as.integer(reduced_data$stateicp)

reduced_data <-
  reduced_data %>%
  mutate(state = ifelse(state==1, "CT", state)) %>%
  mutate(state = ifelse(state==2, "ME", state)) %>%
  mutate(state = ifelse(state==3, "MA", state)) %>%
  mutate(state = ifelse(state==4, "NH", state)) %>%
  mutate(state = ifelse(state==5, "RI", state)) %>%
  mutate(state = ifelse(state==6, "VT", state)) %>%
  mutate(state = ifelse(state==7, "DE", state)) %>%
  mutate(state = ifelse(state==8, "NJ", state)) %>%
  mutate(state = ifelse(state==9, "NY", state)) %>%
  mutate(state = ifelse(state==10, "PA", state)) %>%
  mutate(state = ifelse(state==11, "IL", state)) %>%
  mutate(state = ifelse(state==12, "IN", state)) %>%
  mutate(state = ifelse(state==13, "MI", state)) %>%
  mutate(state = ifelse(state==14, "OH", state)) %>%
  mutate(state = ifelse(state==15, "WI", state)) %>%
  mutate(state = ifelse(state==16, "IA", state)) %>%
  mutate(state = ifelse(state==17, "KS", state)) %>%
  mutate(state = ifelse(state==18, "MN", state)) %>%
  mutate(state = ifelse(state==19, "MO", state)) %>%
  mutate(state = ifelse(state==20, "NE", state)) %>%
  mutate(state = ifelse(state==21, "ND", state)) %>%
  mutate(state = ifelse(state==22, "SD", state)) %>%
  mutate(state = ifelse(state==23, "VA", state)) %>%
  mutate(state = ifelse(state==24, "AL", state)) %>%
  mutate(state = ifelse(state==25, "AR", state)) %>%
  mutate(state = ifelse(state==26, "FL", state)) %>%
  mutate(state = ifelse(state==27, "GA", state)) %>%
  mutate(state = ifelse(state==28, "LA", state)) %>%
  mutate(state = ifelse(state==29, "MS", state)) %>%
  mutate(state = ifelse(state==30, "NC", state)) %>%
  mutate(state = ifelse(state==31, "SC", state)) %>%
  mutate(state = ifelse(state==32, "TX", state)) %>%
  mutate(state = ifelse(state==33, "KY", state)) %>%
  mutate(state = ifelse(state==34, "MD", state)) %>%
  mutate(state = ifelse(state==35, "OK", state)) %>%
  mutate(state = ifelse(state==36, "TN", state)) %>%
  mutate(state = ifelse(state==37, "WV", state)) %>%
  mutate(state = ifelse(state==38, "AZ", state)) %>%
  mutate(state = ifelse(state==39, "CO", state)) %>%
  mutate(state = ifelse(state==40, "ID", state)) %>%
  mutate(state = ifelse(state==41, "MT", state)) %>%
  mutate(state = ifelse(state==42, "NV", state)) %>%
  mutate(state = ifelse(state==43, "NM", state)) %>%
  mutate(state = ifelse(state==44, "UT", state)) %>%
  mutate(state = ifelse(state==45, "WY", state)) %>%
  mutate(state = ifelse(state==46, "CA", state)) %>%
  mutate(state = ifelse(state==47, "OR", state)) %>%
  mutate(state = ifelse(state==48, "WA", state)) %>%
  mutate(state = ifelse(state==49, "AK", state)) %>%
  mutate(state = ifelse(state==50, "HI", state)) %>%
  mutate(state = ifelse(state==51, "PR", state))


reduced_data <-
  reduced_data %>%
  filter(state != 52) %>%
  filter(state != 53) %>%
  filter(state != 54) %>%
  filter(state != 55)

reduced_data <-
  reduced_data %>%
  select(is_working,
         gender,
         census_region,
         hispanic,
         is_white,
         is_black,
         is_american_indian,
         is_asian_or_pacific_islander,
         age,
         age_group,
         us_born,
         state)

total_data <- length(reduced_data$is_working)

reduced_data <-
  reduced_data %>%
  group_by(state) %>%
  mutate(state_total=n()) %>%
  select(is_working, gender, hispanic, is_white, is_black, age, age_group,
         us_born, state, state_total)

reduced_data <-
  reduced_data %>%
  group_by(state, age_group, is_working, gender, hispanic, is_white,
           is_black, us_born, state_total) %>%
  summarise(number=n())

reduced_data <-
  reduced_data %>%
  mutate(cell_prop_of_division_total = number/total_data) %>%
  mutate(cell_prop_of_division_total_state = number/state_total)

# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data, "outputs/census_data.csv")



         