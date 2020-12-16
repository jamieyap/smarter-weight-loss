# -----------------------------------------------------------------------------
# Using training data, calculate Y_pred for those individuals with
# complete data on Y and days 1, 4, 7
# -----------------------------------------------------------------------------
idx_train_complete_case <- complete.cases(dat_train[,c("Y", "delta_1", "delta_4", "delta_7")])
dat_train_complete_case <- dat_train %>% 
  select(id, Y, delta_1, delta_4, delta_7, outcome1, outcome2, outcome3, outcome4, outcome5) %>%
  filter(idx_train_complete_case)

dat_train_complete_case <- dat_train_complete_case %>% 
  mutate(Y_predicted = case_when(
    (delta_7 < 0) ~ 1,
    (delta_4 < -0.22) ~ 1,
    (delta_1 >= -0.19) & (delta_7 >= 0 & delta_7 < 1.5)  ~ 1,
    TRUE ~ 0
  ))

tbl_cart_train <- table(dat_train_complete_case$Y, dat_train_complete_case$Y_predicted)

# -----------------------------------------------------------------------------
# Using test data, calculate Y_pred for those individuals with
# complete data on Y and days 1, 4, 7
# -----------------------------------------------------------------------------
idx_test_complete_case <- complete.cases(dat_test[,c("Y", "delta_1", "delta_4", "delta_7")])
dat_test_complete_case <- dat_test %>% 
  select(id, Y, delta_1, delta_4, delta_7, outcome1, outcome2, outcome3, outcome4, outcome5) %>%
  filter(idx_test_complete_case)

dat_test_complete_case <- dat_test_complete_case %>% 
  mutate(Y_predicted = case_when(
    (delta_7 < 0) ~ 1,
    (delta_4 < -0.22) ~ 1,
    (delta_1 >= -0.19) & (delta_7 >= 0 & delta_7 < 1.5)  ~ 1,
    TRUE ~ 0
  ))

tbl_cart_test <- table(dat_test_complete_case$Y, dat_test_complete_case$Y_predicted)

# -----------------------------------------------------------------------------
# Calculate sensitivity and specificity for for those individuals with
# complete data on Y and days 1, 4, 7 (i.e., CART only rule)
# -----------------------------------------------------------------------------

# Use training data: Specificity
cart_only_specificity_train <- tbl_cart_train[1,1]/(tbl_cart_train[1,1] + tbl_cart_train[1,2])
# Use training data: Sensitivity
cart_only_sensitivity_train <- tbl_cart_train[2,2]/(tbl_cart_train[2,1] + tbl_cart_train[2,2])
# Use test data: Specificity
cart_only_specificity_test <- tbl_cart_test[1,1]/(tbl_cart_test[1,1] + tbl_cart_test[1,2])
# Use test data: Sensitivity
cart_only_sensitivity_test <- tbl_cart_test[2,2]/(tbl_cart_test[2,1] + tbl_cart_test[2,2])

# Combine results into one table
tbl_cart_only <- data.frame(sensitivity = c(cart_only_sensitivity_train, cart_only_sensitivity_test),
                            specificity = c(cart_only_specificity_train, cart_only_specificity_test))
row.names(tbl_cart_only) <- c("Training","Validation")


