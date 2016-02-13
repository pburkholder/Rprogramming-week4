best <- function(state, outcome) {
  ## read outcome data
  allOutcomes<- read.csv('outcome-of-care-measures.csv', colClasses = "character")
  outcomeByState <- split(allOutcomes, allOutcomes$State)

  ## check that state and outcome are valid
  if (is.null(nrow(byState[[state]]))) {
    stop("invalid state")
  }

  valid_outcomes <- list(
    "heart attack"=11,
    "heart failure"=17,
    "pneumonia"=25)

  if (is.null(valid_outcomes[[outcome]])) {
    stop("invalid outcome")
  }

  ## Return hospital name in that state with lowest
  ## 30-day death rate
  column = valid_outcomes[[outcome]]

  outcomeByState[[state]][,column] <- as.numeric(outcomeByState[[state]][,column])
  first <- order(outcomeByState[[state]][,column])[1]
  (outcomeByState[[state]][first,])[["Hospital.Name"]]
}
