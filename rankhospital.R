rankhospital <- function(state, outcome, rank=5) {
  ## read outcome data
  allOutcomes<- read.csv('outcome-of-care-measures.csv', colClasses = "character")

  ## check that state and outcome are valid
  if (is.null(nrow(byState[[state]]))) {
    stop("invalid state")
  }

  valid_outcomes <- list(
    "heart attack"=11,
    "heart failure"=17,
    "pneumonia"=25)

  column = valid_outcomes[[outcome]]

  if (is.null(valid_outcomes[[outcome]])) {
    stop("invalid outcome")
  }

  ## Return hospital name in that state with lowest
  ## 30-day death rate
  outcomeByState <- split(allOutcomes, allOutcomes$State)
  myState <- outcomeByState[[state]]

  myState[,column] <- as.numeric(myState[,column])
  top <- order(myState[,column], myState$Hospital.Name)[1:rank]
  
  (myState[top,])[["Hospital.Name"]]
}
