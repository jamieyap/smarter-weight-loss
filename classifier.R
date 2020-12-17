
# -----------------------------------------------------------------------------
# Define function for implementing overall decision rule
# -----------------------------------------------------------------------------

ClassifyParticipant <- function(delta_1, delta_2, delta_3, delta_4, delta_5, delta_6, delta_7){
  
  # Inputs:
  #     Data on percent change in weight from baseline to day t for one participant.
  #     Inputs delta_1, delta_4, delta_7 are calculated as follows:
  #         delta_t = 100*(((baseline weight in lbs) - (day t weight in lbs))/(baseline weight in lbs))
  #
  # Output:
  #    Y_predicted = 1, if the rule predicts that a participant will be re-randomized
  #    Y_predicted = 0, if the rule predicts that a participant will NOT be re-randomized
  
  # no_missing=1 if participant has data on day 1 AND day 4 AND day 7
  # no_missing=0 if participant has missing data on day 1 OR day 4 OR day 7
  no_missing <- 1*((!is.na(delta_1)) & (!is.na(delta_4)) & (!is.na(delta_7)))
  
  # Proceed with classifying participant using overall decision rule
  if(no_missing==1){
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
    count_miss <- 1*is.na(delta_1) + 1*is.na(delta_2) + 1*is.na(delta_3) + 1*is.na(delta_4) + 1*is.na(delta_5) + 1*is.na(delta_6) + 1*is.na(delta_7)
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

ClassifyParticipant(delta_1 = 0, delta_2 = 1, delta_3 = 1, delta_4 = 0, delta_5 = 1, delta_6 = -1, delta_7 = -5)
ClassifyParticipant(delta_1 = NA, delta_2 = 0, delta_3 = 1, delta_4 = 0, delta_5 = NA, delta_6 = NA, delta_7 = NA)
ClassifyParticipant(delta_1 = 0, delta_2 = 1, delta_3 = 1, delta_4 = 0, delta_5 = 1, delta_6 = -1, delta_7 = 3)


