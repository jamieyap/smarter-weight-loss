# -----------------------------------------------------------------------------
# Select a decision rule to implement on individuals with any missing data on
# either Y, day 1, 4, or 7
# -----------------------------------------------------------------------------
#use_miss_rule <- "miss_rule5"

# -----------------------------------------------------------------------------
# Using training data, calculate Y_pred for those individuals with
# incomplete data on Y and days 1, 4, 7
# -----------------------------------------------------------------------------
idx_train_incomplete_case <- (!complete.cases(dat_train[,c("Y", "delta_1", "delta_4", "delta_7")]))
dat_train_incomplete_case <- dat_train %>% 
  select(id, Y, delta_1, delta_4, delta_7, 
         delta_2, delta_3, delta_5, delta_6,
         outcome1, outcome2, outcome3, outcome4, outcome5) %>%
  filter(idx_train_incomplete_case)

# Note: since outcome1=rerand, setting Y_predicted=1 is equivalent to rerand=1
dat_train_incomplete_case <- dat_train_incomplete_case %>%
  mutate(miss_rule1 = TRUE,
         miss_rule2 = is.na(delta_1),
         miss_rule3 = (is.na(delta_4) | is.na(delta_7)),
         miss_rule4 = ((is.na(delta_1) & is.na(delta_4)) | is.na(delta_7))) %>%
  mutate(miss_rule5 = 1*is.na(delta_1) + 1*is.na(delta_2) + 1*is.na(delta_3) + 1*is.na(delta_4) + 1*is.na(delta_5) + 1*is.na(delta_6) + 1*is.na(delta_7)) %>%
  mutate(miss_rule5 = if_else(miss_rule5>=2, TRUE, FALSE))
  
dat_train_incomplete_case[["Y_predicted"]] <- if_else(dat_train_incomplete_case[[use_miss_rule]], 1, 0)
dat_train_incomplete_case <- dat_train_incomplete_case %>% select(-grep(pattern = "miss_rule", colnames(dat_train_incomplete_case)))

# -----------------------------------------------------------------------------
# Using test data, calculate Y_pred for those individuals with
# incomplete data on Y and days 1, 4, 7
# -----------------------------------------------------------------------------
idx_test_incomplete_case <- (!complete.cases(dat_test[,c("Y", "delta_1", "delta_4", "delta_7")]))
dat_test_incomplete_case <- dat_test %>% 
  select(id, Y, delta_1, delta_4, delta_7, 
         delta_2, delta_3, delta_5, delta_6,
         outcome1, outcome2, outcome3, outcome4, outcome5) %>%
  filter(idx_test_incomplete_case)

# Note: since outcome1=rerand, setting Y_predicted=1 is equivalent to rerand=1
dat_test_incomplete_case <- dat_test_incomplete_case %>%
  mutate(miss_rule1 = TRUE,
         miss_rule2 = is.na(delta_1),
         miss_rule3 = (is.na(delta_4) | is.na(delta_7)),
         miss_rule4 = ((is.na(delta_1) & is.na(delta_4)) | is.na(delta_7))) %>%
  mutate(miss_rule5 = 1*is.na(delta_1) + 1*is.na(delta_2) + 1*is.na(delta_3) + 1*is.na(delta_4) + 1*is.na(delta_5) + 1*is.na(delta_6) + 1*is.na(delta_7)) %>%
  mutate(miss_rule5 = if_else(miss_rule5>=2, TRUE, FALSE))


dat_test_incomplete_case[["Y_predicted"]] <- if_else(dat_test_incomplete_case[[use_miss_rule]], 1, 0)
dat_test_incomplete_case <- dat_test_incomplete_case %>% select(-grep(pattern = "miss_rule", colnames(dat_test_incomplete_case)))

# -----------------------------------------------------------------------------
# Combine rows corresponding to individuals who provided Y and weights at 
# days 1, 4, and 7 and rows corresponding to individuals who did not provide
# Y or weights in either day 1, 4, or 7
# -----------------------------------------------------------------------------
dat_overall_train <- rbind(dat_train_complete_case, dat_train_incomplete_case)
dat_overall_test <- rbind(dat_test_complete_case, dat_test_incomplete_case)

# -----------------------------------------------------------------------------
# Create table for calculating sensitivity and specificity in the training data
# for overall decision rule
# -----------------------------------------------------------------------------
tbl_overall_train <- table(dat_overall_train$Y, dat_overall_train$Y_predicted)
tbl_overall_test <- table(dat_overall_test$Y, dat_overall_test$Y_predicted)

# -----------------------------------------------------------------------------
# Calculate sensitivity and specificity for for those individuals with
# incomplete data on Y and days 1, 4, 7 (i.e., CART only rule)
# -----------------------------------------------------------------------------

# Using training data: Specificity
overall_specificity_train <- tbl_overall_train[1,1]/(tbl_overall_train[1,1] + tbl_overall_train[1,2])
# Using training data: Sensitivity
overall_sensitivity_train <- tbl_overall_train[2,2]/(tbl_overall_train[2,1] + tbl_overall_train[2,2])
# Using test data: Specificity
overall_specificity_test <- tbl_overall_test[1,1]/(tbl_overall_test[1,1] + tbl_overall_test[1,2])
# Using test data: Sensitivity
overall_sensitivity_test <- tbl_overall_test[2,2]/(tbl_overall_test[2,1] + tbl_overall_test[2,2])

# Combine results into one table
tbl_overall <- data.frame(sensitivity = c(overall_sensitivity_train, overall_sensitivity_test),
                          specificity = c(overall_specificity_train, overall_specificity_test))
row.names(tbl_overall) <- c("Training","Validation")

# -----------------------------------------------------------------------------
# Now, perform a logistic regression to check whether predicted
# response status at week 8 predictive of weight loss success at month X?
# -----------------------------------------------------------------------------

#dat_reg <- rbind(dat_train_complete_case, dat_test_complete_case)

dat_reg <- rbind(dat_overall_train, dat_overall_test)
dat_reg2 <- dat_reg[,c("Y_predicted","outcome2")]
dat_reg3 <- dat_reg[,c("Y_predicted","outcome3")]

mod_outcome2 <- glm(outcome2 ~ Y_predicted, family = "binomial", data = dat_reg2)
tbl_mod_outcome2 <- summary(mod_outcome2)$coefficients
row.names(tbl_mod_outcome2) <- c("beta0","beta1")

mod_outcome3 <- glm(outcome3 ~ Y_predicted, family = "binomial", data = dat_reg3)
tbl_mod_outcome3 <- summary(mod_outcome3)$coefficients
row.names(tbl_mod_outcome3) <- c("beta0","beta1")

# -----------------------------------------------------------------------------
# View results
# -----------------------------------------------------------------------------
if(FALSE){
  print(tbl_cart_only)
  print(tbl_overall)
  print(tbl_mod_outcome2)
  print(tbl_mod_outcome3)
}

