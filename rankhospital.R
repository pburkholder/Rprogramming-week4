rankhospital <- function(state, outcome, rank=5) {
  ## read outcome data
  allOutcomes<- read.csv('outcome-of-care-measures.csv', colClasses = "character")
  outcomeByState <- split(allOutcomes, allOutcomes$State)
  
  ## check that state and outcome are valid
  if (is.null(nrow(outcomeByState[[state]]))) {
    stop("invalid state")
  }

  valid_outcomes <- list(
    "heart attack"=11,
    "heart failure"=17,
    "pneumonia"=23)

  column = valid_outcomes[[outcome]]

  if (is.null(valid_outcomes[[outcome]])) {
    stop("invalid outcome")
  }

  ## Return hospital name in that state with lowest
  ## 30-day death rate
  myState <- outcomeByState[[state]]

  myState[,column] <- as.numeric(myState[,column])
  ranked_by_outcome <- order(myState[,column], myState[["Hospital.Name"]], na.last=NA)

  superlative_to_n <- function(x) {
    if (x == "") return(1)
    if (x == "worst") return(length(ranked_by_outcome))
    x
  }

  (myState[ranked_by_outcome[superlative_to_n(rank)],])[["Hospital.Name"]]
}
