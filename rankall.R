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

  listByState <- lapply(
    names(rankByState),
    function(x) {
      c("state"=x, "hospital"=outcomeByState[[x]][rankByState[[x]][1], 2])
    }
  )
 
  listByState

}
