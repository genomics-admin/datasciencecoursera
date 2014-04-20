source("complete.R")
corr <- function(directory, threshold = 0) {
  ###call the function and list out the complete pairs
  df1 <- complete(directory,1:332)
  #print(df1[,1])
  
  ###build the list of id's that are higher than the threshold
  id <- list()
  j <- 0
  for(i in 1:nrow(df1)){
    if(df1[i,]["nobs"]>threshold){
      #print(df1[,1])
      j <- j+1
      id[length(id)+1] <- df1[i,]["id"]
    }
    
  }
  
  #if nothing matches threshold requirement
  if(j==0){
    numvec <- numeric()
    print(summary(numvec))
    print(length(numvec))
    print("0 hits")
    return(numvec) 
  }
  
  ###else
  print(id)
  
  ###Directory Validation
  if (!file.exists(toString(paste(getwd(),directory,sep="/")))) {
    stop("Directory/Folder does not exist in given path. Check Directory variable.")
  } 
  
  ###File validation and listing
  filepattern <- paste("(^[0]*",paste(id,collapse='|^[0]*'),")","\\.csv$",sep="")##"\\.csv$"
  files <- dir(toString(paste(getwd(),directory,sep="/")), recursive=TRUE, full.names=TRUE, pattern=filepattern)
  print(files)
  
  ###pushing all the file data to a list of data frames
  x <- numeric(0)
  myfiles <- list()
  for(i in 1:length(files)){
    myfiles[i] <- lapply(files[i], read.csv)  
    x <- c(x,cor(myfiles[[i]][,2],myfiles[[i]][,3],use = "pairwise.complete.obs"))
  }
  
  #print(cor(myfiles[,2],myfiles[,3], use = "pairwise.complete.obs"))
  #print(apply(myfiles[,-1],1,function(u) cor(u[,2],u[,3],use = "pairwise.complete.obs")))
  
  
  
  
  print(head(x))
  return(x)
  
}

###Test code
#cr <- corr("specdata", 150)
#print(head(cr))
#print(summary(cr))
#print(length(cr))

# cr <- corr("specdata", 400)
# head(cr)
# summary(cr)
# 
#cr <- corr("specdata", 5000)
#summary(cr)
#length(cr)
# 
# cr <- corr("specdata")
# summary(cr)
# length(cr)