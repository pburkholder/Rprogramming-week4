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
      
      superlative_to_n <- function(y,z) {
        if (y == "best") return(1)
        if (y == "worst") return(length(z))
        y
      }
      o <- order(x[,column], x[["Hospital.Name"]], na.last=NA)
      numnum <- superlative_to_n(num,o)
      o[numnum]
    }
  )

  listByState <- lapply(
    names(rankByState),
    function(x) {
      c(x,outcomeByState[[x]][rankByState[[x]][1], 2])
    }
  )

  #data.frame(state=listByState[,1], hospital=listByState[,2])
  listByState

}
