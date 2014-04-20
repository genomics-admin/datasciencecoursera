###optimized
complete <- function(directory, id = 1:332) {
  ###Directory Validation
  if (!file.exists(toString(paste(getwd(),directory,sep="/")))) {
    stop("Directory/Folder does not exist in given path. Check Directory variable.")
  } 
  
  ###File validation and listing
  files <- dir(toString(paste(getwd(),directory,sep="/")), recursive=TRUE, full.names=TRUE, pattern=paste("(^[0]*",paste(id,collapse='|^[0]*'),")","\\.csv$",sep=""))
  #print(files)
  
  ###pushing all the file data to a list of data frames
  myfiles <- list()
  for(i in 1:length(files)){
    myfiles[i] <- lapply(files[i], read.csv)  
  }
    
  #validating data
  #print(nrow(myfiles[[1]])) 
  
  ###actual processing
  outputDF <- data.frame(id= integer(0), nobs= integer(0))
  for (k in id){
    j <- 0
    for(i in 1:length(myfiles)){
      if(myfiles[[i]][1,"ID"]==k){
        k1 <- i
      }
    }
    #print(k1)
    #print(nrow(myfiles[[k1]]))
    for (i in 1:nrow(myfiles[[k1]])){
      if ((myfiles[[k1]][i,"ID"]==k) & (!is.na(myfiles[[k1]][i,"sulfate"])) & (!is.na(myfiles[[k1]][i,"nitrate"]))){
        j <- j+1
        #print(j)
      }
    }
    #validate data
    #print(paste(k,"---",j,sep=""))
    
    ###storing in desired output format
    outputDF <- rbind(outputDF,data.frame(id=k,nobs=j))
    
  }
  #print(outputDF)
  return(outputDF)
  
}

###Test code
#complete("specdata", 1)
#complete("specdata", c(2, 4, 8, 10, 12))
#complete("specdata", 30:25)
#complete("specdata", 3)
#complete("specdata", 332:1)