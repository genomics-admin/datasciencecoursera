source("submitscript3.R")
best <- function(state, outcome) {
  ## Read outcome data
  outcomeData <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ## Check that state and outcome are valid
  if (!state %in% unique(outcomeData[,"State"])) stop("invalid state")
  if (!outcome %in% c("heart attack","heart failure","pneumonia")) stop("invalid outcome")
  ## Return hospital name in that state with lowest 30-day death
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
  ## rate
  print(bestDF)
  print("--------------")
  print(bestDF[1,"Hospital.Name"])
  print("--------------")
  return(bestDF[1,"Hospital.Name"])
}

##Test data
#best("SC", "heart attack")
best("NY", "pneumonia")
#best("NN", "pneumonia")
#best("NY", "hert attack")























# [1] "Provider.Number"                                                                      
# [2] "Hospital.Name"                                                                        
# [3] "Address.1"                                                                            
# [4] "Address.2"                                                                            
# [5] "Address.3"                                                                            
# [6] "City"                                                                                 
# [7] "State"                                                                                
# [8] "ZIP.Code"                                                                             
# [9] "County.Name"                                                                          
# [10] "Phone.Number"                                                                         
# [11] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"                            
# [12] "Comparison.to.U.S..Rate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"  
# [13] "Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
# [14] "Upper.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack" 
# [15] "Number.of.Patients...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"       
# [16] "Footnote...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"                 
# [17] "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"                           
# [18] "Comparison.to.U.S..Rate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure" 
# [19] "Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
# [20] "Upper.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
# [21] "Number.of.Patients...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"      
# [22] "Footnote...Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"                
# [23] "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"                               
# [24] "Comparison.to.U.S..Rate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"     
# [25] "Lower.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"    
# [26] "Upper.Mortality.Estimate...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"    
# [27] "Number.of.Patients...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"          
# [28] "Footnote...Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"                    
# [29] "Hospital.30.Day.Readmission.Rates.from.Heart.Attack"                                  
# [30] "Comparison.to.U.S..Rate...Hospital.30.Day.Readmission.Rates.from.Heart.Attack"        
# [31] "Lower.Readmission.Estimate...Hospital.30.Day.Readmission.Rates.from.Heart.Attack"     
# [32] "Upper.Readmission.Estimate...Hospital.30.Day.Readmission.Rates.from.Heart.Attack"     
# [33] "Number.of.Patients...Hospital.30.Day.Readmission.Rates.from.Heart.Attack"             
# [34] "Footnote...Hospital.30.Day.Readmission.Rates.from.Heart.Attack"                       
# [35] "Hospital.30.Day.Readmission.Rates.from.Heart.Failure"                                 
# [36] "Comparison.to.U.S..Rate...Hospital.30.Day.Readmission.Rates.from.Heart.Failure"       
# [37] "Lower.Readmission.Estimate...Hospital.30.Day.Readmission.Rates.from.Heart.Failure"    
# [38] "Upper.Readmission.Estimate...Hospital.30.Day.Readmission.Rates.from.Heart.Failure"    
# [39] "Number.of.Patients...Hospital.30.Day.Readmission.Rates.from.Heart.Failure"            
# [40] "Footnote...Hospital.30.Day.Readmission.Rates.from.Heart.Failure"                      
# [41] "Hospital.30.Day.Readmission.Rates.from.Pneumonia"                                     
# [42] "Comparison.to.U.S..Rate...Hospital.30.Day.Readmission.Rates.from.Pneumonia"           
# [43] "Lower.Readmission.Estimate...Hospital.30.Day.Readmission.Rates.from.Pneumonia"        
# [44] "Upper.Readmission.Estimate...Hospital.30.Day.Readmission.Rates.from.Pneumonia"        
# [45] "Number.of.Patients...Hospital.30.Day.Readmission.Rates.from.Pneumonia"                
# [46] "Footnote...Hospital.30.Day.Readmission.Rates.from.Pneumonia"