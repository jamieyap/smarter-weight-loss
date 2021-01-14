#use_this_outcome <- "outcome1"
#myseed <- 342386787

# -----------------------------------------------------------------------------
# Construct training data and test data
# -----------------------------------------------------------------------------

set.seed(myseed)
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
# What is the outcome that will be used in the classification rule?
# -----------------------------------------------------------------------------
dat_train[["Y"]] <- dat_train[[use_this_outcome]]  # see top of script
dat_test[["Y"]] <- dat_test[[use_this_outcome]]  # see top of script


if(FALSE){
  dat_train <- dat_train %>%
    mutate(delta_1 = if_else(is.na(delta_1), 0, delta_1),
           delta_4 = if_else(is.na(delta_4), 0, delta_4),
           delta_7 = if_else(is.na(delta_7), 0, delta_7))
  
  dat_test <- dat_test %>%
    mutate(delta_1 = if_else(is.na(delta_1), 0, delta_1),
           delta_4 = if_else(is.na(delta_4), 0, delta_4),
           delta_7 = if_else(is.na(delta_7), 0, delta_7))
  
}



