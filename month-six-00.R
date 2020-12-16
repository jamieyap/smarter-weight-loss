# -----------------------------------------------------------------------------
# Read in packages and data
# -----------------------------------------------------------------------------

library(dplyr)
path_input_data <- Sys.getenv("path_input_data")
dat <- read.csv(file.path(path_input_data, "SMART Daily weights 6mo wt.csv"))

# -----------------------------------------------------------------------------
# Beginning with the original dataset, select subset of rows belonging to those
# individuals who were assigned to the APP ONLY condition at first-stage
# randomization and construct new columns
# -----------------------------------------------------------------------------

colnames(dat)[1] <- "id"

dat <- dat %>% 
  mutate(rerand = -1*(rerand-1)) %>%
  rename(weight_1 = weight.1, weight_2 = weight.2, weight_3 = weight.3,
         weight_4 = weight.4, weight_5 = weight.5, weight_6 = weight.6, 
         weight_7 = weight.7) %>%
  # Construct three indicator variables
  # outcome1: =1 if individual was rerandomized, 0 if not rerandomized
  # outcome2: =1 if individual lost 5% or more of their baseline weight by month 3
  # outcome3: =1 if individual lost 5% or more of their baseline weight by month 6
  mutate(outcome1 = rerand, 
         outcome2 = if_else((weight_bl_lbs - weight_3mo_lbs)/weight_bl_lbs >= 0.05, 1, 0),
         outcome3 = if_else((weight_bl_lbs - weight_6mo_lbs)/weight_bl_lbs >= 0.05, 1, 0),
         outcome4 = if_else((weight_bl_lbs - weight_3mo_lbs)/(3*4) >= 2, 1, 0),
         outcome5 = if_else((weight_bl_lbs - weight_6mo_lbs)/(6*4) >= 2, 1, 0)) %>%
  # Percent weight loss from baseline to day t, t=1,...,7
  # delta.t <0 refers to weight gain while delta.t>0 refers to weight loss
  mutate(delta_1 = 100*(weight_bl_lbs - weight_1)/weight_bl_lbs,
         delta_2 = 100*(weight_bl_lbs - weight_2)/weight_bl_lbs,
         delta_3 = 100*(weight_bl_lbs - weight_3)/weight_bl_lbs,
         delta_4 = 100*(weight_bl_lbs - weight_4)/weight_bl_lbs,
         delta_5 = 100*(weight_bl_lbs - weight_5)/weight_bl_lbs,
         delta_6 = 100*(weight_bl_lbs - weight_6)/weight_bl_lbs,
         delta_7 = 100*(weight_bl_lbs - weight_7)/weight_bl_lbs) %>%
  # An indicator variable for whether weight at day t is missing
  mutate(miss_day1 = 1*(is.na(weight_1)),
         miss_day2 = 1*(is.na(weight_2)),
         miss_day3 = 1*(is.na(weight_3)),
         miss_day4 = 1*(is.na(weight_4)),
         miss_day5 = 1*(is.na(weight_5)),
         miss_day6 = 1*(is.na(weight_6)),
         miss_day7 = 1*(is.na(weight_7))) %>%
  # Only retain individuals in the APP ONLY condition since we wish to address the question:
  # Is it Possible to Identify Non-Responders to App Alone During the First Week? 
  filter(A1==1) %>%
  mutate(plotid = 1:nrow(.)) %>%
  select(plotid, everything()) %>%
  # Exclude individual due to high probability of incorrectly recorded data on day 6
  filter(plotid!=95)

# -----------------------------------------------------------------------------
# Calculate summary statistics for particpants who were assigned to the
# APP ONLY condition during first-stage randomization
# -----------------------------------------------------------------------------

# tot_rows: How many participants are in the data?
# percent_dayt: How many percent of individuals did not provide weights at day t?
missdf <- dat %>% 
  summarise(tot_rows = n(), 
            percent_day1 = 100*sum(miss_day1)/nrow(.), 
            percent_day2 = 100*sum(miss_day2)/nrow(.), 
            percent_day3 = 100*sum(miss_day3)/nrow(.), 
            percent_day4 = 100*sum(miss_day4)/nrow(.), 
            percent_day5 = 100*sum(miss_day5)/nrow(.), 
            percent_day6 = 100*sum(miss_day6)/nrow(.), 
            percent_day7 = 100*sum(miss_day7)/nrow(.),
            percent1 = 100*sum(is.na(outcome1))/nrow(.),
            percent2 = 100*sum(is.na(outcome2))/nrow(.),
            percent3 = 100*sum(is.na(outcome3))/nrow(.),
            miss_analysis = 100*sum(miss_day1 | miss_day4 | miss_day7)/nrow(.)
  ) %>%
  round(., digits=3)

