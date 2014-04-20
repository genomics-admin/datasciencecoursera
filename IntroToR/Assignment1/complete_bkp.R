
complete <- function(directory, id = 1:332) {
  ###Directory Validation
  if (!file.exists(toString(paste(getwd(),directory,sep="/")))) {
    stop("Directory/Folder does not exist in given path. Check Directory variable.")
  } 
  
  ###File validation and listing
  files <- dir(toString(paste(getwd(),directory,sep="/")), recursive=TRUE, full.names=TRUE, pattern=paste("(^[0]*",paste(id,collapse='|^[0]*'),")","\\.csv$",sep=""))
  print(files)
  
  ###pushing all the file data to a data frame
  myfiles <- data.frame()
  for(i in 1:length(files)){
    myfiles <- rbind(myfiles, read.csv(files[i],header=TRUE))  
    print(i)
  }
  
  #validating data
  #print(myfiles[1,])
  #print(nrow(myfiles))
  
  ###actual processing
  outputDF <- data.frame(id= integer(0), nobs= integer(0))
  for (k in id){
    print(k)
    j <- 0
    for (i in 1:nrow(myfiles)){
      print(i)
      if (!is.na(myfiles[i,]["sulfate"]) & !is.na(myfiles[i,]["nitrate"]) & myfiles[i,]["ID"] == k){
        j <- j+1
        
      }
    }
    
    #validate data
    print(paste(k,"---",j,sep=""))
    
    ###storing in desired output format
    outputDF <- rbind(outputDF,data.frame(id=k,nobs=j))
    
  }
  
  print(outputDF)
  return(outputDF)
}

###Test code
#complete("specdata", 1)
#complete("specdata", c(2, 4, 8, 10, 12))
#complete("specdata", 30:25)
#complete("specdata", 3)
complete("specdata", 332:1)