# -----------------------------------------------------------------------------
# Define function for implementing CART-only decision rule
# -----------------------------------------------------------------------------

ClassifyParticipantCARTOnly <- function(delta_1, delta_2, delta_3, delta_4, delta_5, delta_6, delta_7){
  
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
  
  # Proceed with classifying participant using overall decision rule
  if(sum(1*c(check_missing_array[1]+check_missing_array[4]+check_missing_array[7]))==0){
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
    Y_predicted <- NA
  }
  
  return(Y_predicted)
}


# -----------------------------------------------------------------------------
# Examples
# -----------------------------------------------------------------------------

ClassifyParticipantCARTOnly(delta_1 = 0, delta_2 = 1, delta_3 = 1, delta_4 = 0, delta_5 = 1, delta_6 = -1, delta_7 = -5)
ClassifyParticipantCARTOnly(delta_1 = NA, delta_2 = 0, delta_3 = 1, delta_4 = 0, delta_5 = NA, delta_6 = NA, delta_7 = NA)
ClassifyParticipantCARTOnly(delta_1 = 0, delta_2 = 1, delta_3 = 1, delta_4 = 0, delta_5 = 1, delta_6 = -1, delta_7 = 3)

