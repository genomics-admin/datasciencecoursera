source("submitscript3.R")
rankall <- function(outcome, num = "best") {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (!outcome %in% c("heart attack","heart failure","pneumonia")) stop("invalid outcome")
  ## For each state, find the hospital of the given rank
  bdf <- data.frame()
  
  ###Handling specific reason for death
  if(outcome == "heart attack"){
    ocm <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    for (state in unique(outcomeData[,"State"])){
      print(state)
      bestDF <- outcomeData[outcomeData["State"] == state,][c("Hospital.Name","State",ocm)]
      bestDF <- bestDF[!bestDF[ocm]=="Not Available",][c("Hospital.Name","State",ocm)]
      bestDF[, ocm] <- as.numeric(bestDF[, ocm])
      bestDF <- bestDF[order(bestDF$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack",bestDF$"Hospital.Name"),]
      bdf <- rbind(bdf,bDF(bestDF,num,state))
    }
  }
  else if(outcome == "heart failure"){
    ocm <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    for (state in unique(outcomeData[,"State"])){
      print(state)
      bestDF <- outcomeData[outcomeData["State"] == state,][c("Hospital.Name","State",ocm)]
      bestDF <- bestDF[!bestDF[ocm]=="Not Available",][c("Hospital.Name","State",ocm)]
      bestDF[, ocm] <- as.numeric(bestDF[, ocm])
      bestDF <- bestDF[order(bestDF$"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure",bestDF$"Hospital.Name"),]
      bdf <- rbind(bdf,bDF(bestDF,num,state))
    }
  }
  else if(outcome == "pneumonia"){
    ocm <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    for (state in unique(outcomeData[,"State"])){
      print(state)
      bestDF <- outcomeData[outcomeData["State"] == state,][c("Hospital.Name","State",ocm)]
      bestDF <- bestDF[!bestDF[ocm]=="Not Available",][c("Hospital.Name","State",ocm)]
      bestDF[, ocm] <- as.numeric(bestDF[, ocm])
      bestDF <- bestDF[order(bestDF$"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia",bestDF$"Hospital.Name"),]
      bdf <- rbind(bdf,bDF(bestDF,num,state))
    }
  }
  
  ###Sorting based on state to handle NA states
  bdf <- bdf[order(bdf$"State"),]
  
  ###Handling the column names
  colnames(bdf) <- c("hospital","state","rate")
  
    
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  print(bdf[,c("hospital","state")])
  return(bdf[,c("hospital","state")])
}






###Funciton to handle different num types : returns single row data frame
bDF <- function(bestDF,num,state){
  if(num == "best"){
    print(bestDF)
    print("--------------")
    print(bestDF[1,])
    print("--------------")
    return(bestDF[1,])
  }
  else if(num == "worst"){
    print(bestDF)
    print("--------------")
    print(bestDF[nrow(bestDF),])
    print("--------------")
    return(bestDF[nrow(bestDF),])
  }
  else if(num > nrow(bestDF)){
    print("NA")
    return(c("NA",state,"NA"))
  }
  else if(num < nrow(bestDF)){
    print(bestDF)
    print("--------------")
    print(bestDF[num,])
    print("--------------")
    return(bestDF[num,])
  }  
}

##Test Data
#rankall("heart attack", 4)
#rankall("pneumonia", "worst")
#rankall("heart failure", 10)