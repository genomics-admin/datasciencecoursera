
pollutantmean <- function(directory, pollutant, id = 1:332) {
  ###Directory Validation
  if (!file.exists(toString(paste(getwd(),directory,sep="/")))) {
    stop("Directory/Folder does not exist in given path. Check Directory variable.")
  } 
#   else {
#     print("subDir exists in mainDir and is a directory")
#   }
  
  ###File validation and listing
  files <- 0
  if (class(id) == 'numeric'){
    filepattern=paste(toString(id),".csv",sep="")
    #print(filepattern)
    files <- dir(toString(paste(getwd(),directory,sep="/")), recursive=TRUE, full.names=TRUE, pattern=filepattern)
    files <- files[1]
    #print(files)  
  }
  else if (class(id) == 'integer'){
    #filepattern <-{
      #"100\\.csv$|200\\.csv$"
      #"(001|200)\\.csv$"
      #paste("(^[0]*",paste(id,collapse='|^[0]*'),")","\\.csv$",sep="")
    #}
    #print(filepattern)
    files <- dir(toString(paste(getwd(),directory,sep="/")), recursive=TRUE, full.names=TRUE, pattern=paste("(^[0]*",paste(id,collapse='|^[0]*'),")","\\.csv$",sep=""))
  }
  else{
    stop("ID parameter seems to be of incorrect format..\n\tExpected: \n\t\t1. Single numaric value, for example 35. \n\t\t2. A range of (integer type)value, example 2:300\n")
  }
  
  #Validating file paths
  #print(files)

  ###pushing all the file data to a data frame
  myfiles <- data.frame()
  for(i in 1:length(files)){
    myfiles <- rbind(myfiles, read.csv(files[i],header=TRUE))  
  }
  
  #validating data
  #print(myfiles[1,])
  #print(nrow(myfiles))

  ###Aggregating 
  meanval <- list()
  for (i in 1:nrow(myfiles)){
    if (!is.na(myfiles[i,][pollutant])){
      meanval[length(meanval)+1] <- myfiles[i,][pollutant]
    }
  }

  print(round(Reduce("+",meanval)/length(meanval),digits=3))
  
}

###Test code
#pollutantmean("specdata", "sulfate", 1:10)
#pollutantmean("specdata", "nitrate", 70:72)
#pollutantmean("specdata", "nitrate", 23)
#pollutantmean("specdata", "nitrate")
