#rankall <- function(outcome="heart failure", num = "best") {
rankall <- function(outcome="heart failure", num = 1) {
  ## read outcome data
  allOutcomes<- read.csv('outcome-of-care-measures.csv', colClasses = "character")

  valid_outcomes <- list(
    "heart attack"=11,
    "heart failure"=17,
    "pneumonia"=25)

  column = valid_outcomes[[outcome]]

  if (is.null(valid_outcomes[[outcome]])) {
    stop("invalid outcome")
  }

  ## Return a data frame with hospital names and
  ## the (abbreviated) state name

  outcomeByState <- split(allOutcomes, allOutcomes$State)

  rankByState <- lapply(outcomeByState, function(x) {
      x[,column] <- as.numeric(x[,column])
      order(x[,column], x[["Hospital.Name"]], na.last=NA)[1] # 1 is best
    }
  )


  superlative_to_n <- function(x) {
    if (x == "best") return(1)
    if (x == "worst") return(length(ranked_by_outcome))
    x
  }

  state="TX"
  outcomeByState[[state]][ rankByState[[state]][1], 2 ]
#  (myState[ranked_by_outcome[superlative_to_n(rank)],])[["Hospital.Name"]]

}
