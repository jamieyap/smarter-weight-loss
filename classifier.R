# -----------------------------------------------------------------------------
# Define function for implementing overall decision rule 1
# -----------------------------------------------------------------------------

ClassifyParticipantRule1 <- function(delta_1, delta_2, delta_3, delta_4, delta_5, delta_6, delta_7){
  
  # Inputs:
  #     Data on percent change in weight from baseline to day t for one participant.
  #     Inputs delta_1, delta_4, delta_7 are calculated as follows:
  #         delta_t = 100*(((baseline weight in lbs) - (day t weight in lbs))/(baseline weight in lbs))
  #
  # Output:
  #    Y_predicted = 1, if the rule predicts that a participant will be re-randomized
  #    Y_predicted = 0, if the rule predicts that a participant will NOT be re-randomized
  
  check_missing_array <- c(delta_1, delta_2, delta_3, delta_4, delta_5, delta_6, delta_7)
  check_missing_array <- is.na(check_missing_array)
  count_miss <- sum(1*(check_missing_array))
  
  # Proceed with classifying participant using overall decision rule
  if(count_miss == 0){
    if(delta_7 < 0){
      Y_predicted <- 1
    }else if(delta_4 < -0.22){
      Y_predicted <- 1
    }else if((delta_1 >= -0.19) & (delta_7 >= 0 & delta_7 < 1.5)){
      Y_predicted <- 1
    }else{
      Y_predicted <- 0
    }
  }else{
    if(sum(1*c(check_missing_array[1]+check_missing_array[4]+check_missing_array[7]))>=1){
      Y_predicted <- 1
    }else{
      next
    }
  }
  
  return(Y_predicted)
}

# -----------------------------------------------------------------------------
# Examples
# -----------------------------------------------------------------------------

ClassifyParticipantRule1(delta_1 = 0, delta_2 = 1, delta_3 = 1, delta_4 = 0, delta_5 = 1, delta_6 = -1, delta_7 = -5)
ClassifyParticipantRule1(delta_1 = NA, delta_2 = 0, delta_3 = 1, delta_4 = 0, delta_5 = NA, delta_6 = NA, delta_7 = NA)
ClassifyParticipantRule1(delta_1 = 0, delta_2 = 1, delta_3 = 1, delta_4 = 0, delta_5 = 1, delta_6 = -1, delta_7 = 3)


# -----------------------------------------------------------------------------
# Define function for implementing overall decision rule 5
# -----------------------------------------------------------------------------

ClassifyParticipantRule5 <- function(delta_1, delta_2, delta_3, delta_4, delta_5, delta_6, delta_7){
  
  # Inputs:
  #     Data on percent change in weight from baseline to day t for one participant.
  #     Inputs delta_1, delta_4, delta_7 are calculated as follows:
  #         delta_t = 100*(((baseline weight in lbs) - (day t weight in lbs))/(baseline weight in lbs))
  #
  # Output:
  #    Y_predicted = 1, if the rule predicts that a participant will be re-randomized
  #    Y_predicted = 0, if the rule predicts that a participant will NOT be re-randomized
  
  check_missing_array <- c(delta_1, delta_2, delta_3, delta_4, delta_5, delta_6, delta_7)
  check_missing_array <- is.na(check_missing_array)
  count_miss <- sum(1*(check_missing_array))
  
  # Proceed with classifying participant using overall decision rule
  if(count_miss == 0){
    if(delta_7 < 0){
      Y_predicted <- 1
    }else if(delta_4 < -0.22){
      Y_predicted <- 1
    }else if((delta_1 >= -0.19) & (delta_7 >= 0 & delta_7 < 1.5)){
      Y_predicted <- 1
    }else{
      Y_predicted <- 0
    }
  }else{
    if(count_miss>=2){
      Y_predicted <- 1
    }else{
      Y_predicted <- 0
    }
  }
  
  return(Y_predicted)
}

# -----------------------------------------------------------------------------
# Examples
# -----------------------------------------------------------------------------

ClassifyParticipantRule5(delta_1 = 0, delta_2 = 1, delta_3 = 1, delta_4 = 0, delta_5 = 1, delta_6 = -1, delta_7 = -5)
ClassifyParticipantRule5(delta_1 = NA, delta_2 = 0, delta_3 = 1, delta_4 = 0, delta_5 = NA, delta_6 = NA, delta_7 = NA)
ClassifyParticipantRule5(delta_1 = 0, delta_2 = 1, delta_3 = 1, delta_4 = 0, delta_5 = 1, delta_6 = -1, delta_7 = 3)


