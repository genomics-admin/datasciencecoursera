outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
#outcome[, 11] <- as.numeric(outcome[, 11])
outcome[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"] <- as.numeric(outcome[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"])
## You may get a warning about NAs being introduced; that is okay
hist(outcome[, "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"])
