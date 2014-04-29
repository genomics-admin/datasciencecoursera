source("submitscript3.R")
rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (!state %in% unique(outcomeData[,"State"])) stop("invalid state")
  if (!outcome %in% c("heart attack","heart failure","pneumonia")) stop("invalid outcome")
  ## Return hospital name in that state with the given rank
  if(outcome == "heart attack"){
    ocm <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    bestDF <- outcomeData[outcomeData["State"] == state,][c("Hospital.Name","State",ocm)]
    bestDF <- bestDF[!bestDF[ocm]=="Not Available",][c("Hospital.Name","State",ocm)]
    bestDF[, ocm] <- as.numeric(bestDF[, ocm])
    bestDF <- bestDF[order(bestDF$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",bestDF$"Hospital.Name"),]
  }
  else if(outcome == "heart failure"){
    ocm <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    bestDF <- outcomeData[outcomeData["State"] == state,][c("Hospital.Name","State",ocm)]
    bestDF <- bestDF[!bestDF[ocm]=="Not Available",][c("Hospital.Name","State",ocm)]
    bestDF[, ocm] <- as.numeric(bestDF[, ocm])
    bestDF <- bestDF[order(bestDF$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",bestDF$"Hospital.Name"),]
  }
  else if(outcome == "pneumonia"){
    ocm <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    bestDF <- outcomeData[outcomeData["State"] == state,][c("Hospital.Name","State",ocm)]
    bestDF <- bestDF[!bestDF[ocm]=="Not Available",][c("Hospital.Name","State",ocm)]
    bestDF[, ocm] <- as.numeric(bestDF[, ocm])
    bestDF <- bestDF[order(bestDF$"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",bestDF$"Hospital.Name"),]
  }
  ## 30-day death rate
  if(num == "best"){
    print(bestDF)
    print("--------------")
    print(bestDF[1,"Hospital.Name"])
    print("--------------")
    return(bestDF[1,"Hospital.Name"])
  }
  else if(num == "worst"){
    print(bestDF)
    print("--------------")
    print(bestDF[nrow(bestDF),"Hospital.Name"])
    print("--------------")
    return(bestDF[nrow(bestDF),"Hospital.Name"])
  }
  else if(num > nrow(bestDF)){
    print("NA")
    return("NA")
  }
  else if(num < nrow(bestDF)){
    print(bestDF)
    print("--------------")
    print(bestDF[num,"Hospital.Name"])
    print("--------------")
    return(bestDF[num,"Hospital.Name"])
  }
  
}

#test data
#rankhospital("NC", "heart attack", "worst")
#rankhospital("WA", "heart attack", 7)
#rankhospital("WA", "pneumonia", 1000)
#rankhospital("NY", "heart attak", 7)