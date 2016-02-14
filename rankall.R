rankall <- function(outcome="pneumonia", num = "best") {
  ## read outcome data
  allOutcomes<- read.csv('outcome-of-care-measures.csv', colClasses = "character")

  valid_outcomes <- list(
    "heart attack"=11,
    "heart failure"=17,
    "pneumonia"=23)

  # convert string outcome to a colum number
  column = valid_outcomes[[outcome]]

  if (is.null(valid_outcomes[[outcome]])) {
    stop("invalid outcome")
  }

  ## Return a data frame with hospital names and
  ## the (abbreviated) state name

  # split by State
  outcomeByState <- split(allOutcomes, allOutcomes$State)


  # pass each element (state) of outcomeByState
  # into lapply and apply a function to do rank by outcome
  rankByState <- lapply(outcomeByState, function(x) {
      # column is a global here
      suppressWarnings(
       x[,column] <- as.numeric(x[,column])
      )
      # convert 'best' to 1
      # convert 'worst' to the last element of z
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
        c(outcomeByState[[x]][rankByState[[x]][1], 2], x)
    }
    
  )

  m <- matrix(unlist(listByState), ncol=2, byrow=T)
  colnames(m) <- c("Hospital", "State")
  data.frame(m)
}
