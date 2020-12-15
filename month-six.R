library(dplyr)
library(pROC)
library(rpart)
library(rpart.plot)

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
  # outcome3: =1 if individual lost an average of 2 pounds or more per week by month 3
  mutate(outcome1 = rerand, 
         outcome2 = if_else((weight_bl_lbs - weight_3mo_lbs)/weight_bl_lbs >= 0.05, 1, 0),
         outcome3 = if_else((weight_bl_lbs - weight_6mo_lbs)/weight_bl_lbs >= 0.05, 1, 0)) %>%
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
  # Only retain individuals in the APP Only condition since we wish to address the question:
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

# How many participants are in the data?
nrow(dat)

# How many percent of individuals did not provide weights at day t?
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
            miss_analysis = 100*sum(is.na(outcome1) | miss_day1 | miss_day4 | miss_day7)/nrow(.)
            ) %>%
  round(., digits=3)

#print(missdf)

# -----------------------------------------------------------------------------
# Construct training data and test data
# -----------------------------------------------------------------------------

set.seed(342386787)
usedat <- dat %>% 
  # Stratify by sex, outcome1, outcome2, outcome3
  group_by(sex, outcome1, outcome2, outcome3) %>%  
  mutate(tot_in_group = n(), 
         num_leave_out_in_group = ceiling(0.20*tot_in_group), 
         num_retain_in_group = n()-ceiling(0.20*tot_in_group)) %>%
  # For each possible set of values of (gender, outcome1, outcome2)
  # Assign 80% to training data and 20% to test data
  mutate(leave_out = c(rep(1, num_leave_out_in_group[1]), rep(0, num_retain_in_group[1]))) %>%
  arrange(desc(leave_out))

# Finally, take subset of individuals who will be used as training data and test data
dat_train <- usedat %>% filter(leave_out==0) %>% ungroup(.)
dat_test <- usedat %>% filter(leave_out==1) %>% ungroup(.)

# -----------------------------------------------------------------------------
# Using training data, calculate Y_pred
# -----------------------------------------------------------------------------
dat_overall_train <- dat_train %>% 
  select(id, Y = outcome1, delta_1, delta_4, delta_7, outcome2, outcome3)

dat_overall_train <- dat_overall_train[complete.cases(dat_overall_train[,c("Y", "delta_1", "delta_4", "delta_7")]),] 

dat_overall_train <- dat_overall_train %>% 
  mutate(Y_predicted = case_when(
    (delta_7 < 0) ~ 1,
    (delta_4 < -0.22) ~ 1,
    (delta_1 >= -0.19) & (delta_7 <= 1.5)  ~ 1,
    TRUE ~ 0
  ))

tbl_cart_train <- table(dat_overall_train$Y, dat_overall_train$Y_predicted)

tmp_dat <- dat_train %>% 
  select(id, Y = outcome1, delta_1, delta_4, delta_7, outcome2, outcome3)

tmp_dat <- tmp_dat[complete.cases(tmp_dat[,c("Y", "delta_1", "delta_4", "delta_7")]),] 
tmp_dat <- tmp_dat %>%
  # Those who have any missing values in days 1,4,7 will be considered 
  # non-responders (i.e., since outcome1=rerand, setting Y_predicted=1 is
  # equivalent to rerand=1)
  mutate(Y_predicted = 1)

# Combine rows corresponding to individuals who provided weights at 
# days 1, 4, and 7 and rows corresponding to individuals who did not provide
# weights in either day 1, 4, or 7
dat_overall_train <- rbind(dat_overall_train, tmp_dat)

# Create table for calculating sensitivity and specificity in the training data
# for overall decision rule
tbl_overall_train <- table(dat_overall_train$Y, dat_overall_train$Y_predicted)

# CART only rule: Specificity
cart_only_specificity_train <- tbl_cart_train[1,1]/(tbl_cart_train[1,1] + tbl_cart_train[1,2])

# CART only rule: Sensitivity
cart_only_sensitivity_train <- tbl_cart_train[2,2]/(tbl_cart_train[2,1] + tbl_cart_train[2,2])

# Overall rule: Specificity
overall_specificity_train <- tbl_overall_train[1,1]/(tbl_overall_train[1,1] + tbl_overall_train[1,2])

# Overall rule: Sensitivity
overall_sensitivity_train <- tbl_overall_train[2,2]/(tbl_overall_train[2,1] + tbl_overall_train[2,2])

# -----------------------------------------------------------------------------
# Using test data, calculate Y_pred
# -----------------------------------------------------------------------------

dat_overall_test <- dat_test %>% 
  select(id, Y = outcome1, delta_1, delta_4, delta_7, outcome2, outcome3)

dat_overall_test <- dat_overall_test[complete.cases(dat_overall_test[,c("Y", "delta_1", "delta_4", "delta_7")]),] 

dat_overall_test <- dat_overall_test %>% 
  mutate(Y_predicted = case_when(
    (delta_7 < 0) ~ 1,
    (delta_4 < -0.22) ~ 1,
    (delta_1 >= -0.19) & (delta_7 <= 1.5)  ~ 1,
    TRUE ~ 0
  ))


tbl_cart_test <- table(dat_overall_test$Y, dat_overall_test$Y_predicted)

tmp_dat <- dat_test %>% 
  select(id, Y = outcome1, delta_1, delta_4, delta_7, outcome2, outcome3)

tmp_dat <- tmp_dat[complete.cases(tmp_dat[,c("Y", "delta_1", "delta_4", "delta_7")]),] 
tmp_dat <- tmp_dat %>%
  # Those who have any missing values in days 1,4,7 will be considered 
  # non-responders (i.e., since outcome1=rerand, setting Y_predicted=1 is
  # equivalent to rerand=1)
  mutate(Y_predicted = 1)

# Combine rows corresponding to individuals who provided weights at 
# days 1, 4, and 7 and rows corresponding to individuals who did not provide
# weights in either day 1, 4, or 7
dat_overall_test <- rbind(dat_overall_test, tmp_dat)

# Create table for calculating sensitivity and specificity in the testing data
# for overall decision rule
tbl_overall_test <- table(dat_overall_test$Y, dat_overall_test$Y_predicted)

# CART only rule: Specificity
cart_only_specificity_test <- tbl_cart_test[1,1]/(tbl_cart_test[1,1] + tbl_cart_test[1,2])

# CART only rule: Sensitivity
cart_only_sensitivity_test <- tbl_cart_test[2,2]/(tbl_cart_test[2,1] + tbl_cart_test[2,2])

# Overall rule: Specificity
overall_specificity_test <- tbl_overall_test[1,1]/(tbl_overall_test[1,1] + tbl_overall_test[1,2])

# Overall rule: Sensitivity
overall_sensitivity_test <- tbl_overall_test[2,2]/(tbl_overall_test[2,1] + tbl_overall_test[2,2])

# -----------------------------------------------------------------------------
# Combine calculations into a table
# -----------------------------------------------------------------------------

tbl_cart_only <- data.frame(type = c("training", "validation"),
                            sensitivity = c(cart_only_sensitivity_train, cart_only_sensitivity_test),
                            specificity = c(cart_only_specificity_train, cart_only_specificity_test))

tbl_overall <- data.frame(type = c("training", "validation"),
                          sensitivity = c(overall_sensitivity_train, overall_sensitivity_test),
                          specificity = c(overall_specificity_train, overall_specificity_test))

#print(tbl_cart_only)
#print(tbl_overall)

row.names(tbl_cart_only) <- c("Training","Validation")
row.names(tbl_overall) <- c("Training","Validation")

# -----------------------------------------------------------------------------
# Now, perform a logistic regression to check whether
# response status at week 8 predictive of weight loss at month 3? 
# -----------------------------------------------------------------------------

dat_reg <- rbind(dat_overall_train, dat_overall_test)

mod_outcome2 <- glm(outcome2 ~ Y_predicted, family = "binomial", data = dat_reg)
tbl_mod_outcome2 <- summary(mod_outcome2)$coefficients
row.names(tbl_mod_outcome2) <- c("beta0","beta1")

#print(tbl_mod_outcome2)

mod_outcome3 <- glm(outcome3 ~ Y_predicted, family = "binomial", data = dat_reg)
tbl_mod_outcome3 <- summary(mod_outcome3)$coefficients
row.names(tbl_mod_outcome3) <- c("beta0","beta1")

#print(tbl_mod_outcome3)

