


getwd()
dirc <- paste(getwd(),"/rprog-data-specdata/",sep="")
dirc

# Make a function to process each file
processFile <- function(f) {
  df <- read.csv(f)
  # ...and do stuff...
  file.info(f)$size # dummy result
}

# Find all .csv files
files <- dir(paste(getwd(),directory,sep="/"), recursive=TRUE, full.names=TRUE, pattern="\\.csv$")

# Apply the function to all files.
result <- sapply(files, processFile)

result




id <- 30:150

id1 <- paste("(",paste(id,collapse='|'),")","\\.csv$",sep="")
id1


if (file.exists(paste(getwd(), directory, "/", sep = "/", collapse = "/"))) {
  print("subDir exists in mainDir and is a directory")
} else {
  stop("directory does not exist in given path")
}


###pushing all the file data to a list of data frames
myfiles <- list()
for(i in 1:length(files)){
  myfiles[i] <- lapply(files[i], read.csv)  
}

#validating data
print(myfiles[[1]][1,]) 

id <- c(1,2,3)
class(id)

id <- 9:3#c(1,2,3)
for (k in id){
  print(k)
}


nodata <- data.frame(id= integer(0), nobs= integer(0))
str(nodata)

tdata <- data.frame(id=1,nobs=5)
nodata <- rbind(nodata,tdata)
nodata <- rbind(nodata,data.frame(id=2,nobs=6))


mat<-matrix(1:9,1,9) 
mat1 <- which(mat==5,arr.ind=TRUE)
mat1[[1,1]]


files <- dir(paste(getwd(),"specdata",sep="/"), recursive=TRUE, full.names=TRUE, pattern="\\.csv$")

myfiles <- list()
for(i in 1:length(files)){
  myfiles[i] <- lapply(files[i], read.csv)  
}

y <- data.frame(group = letters[1:5], a = rnorm(5) , b = rnorm(5), c = rnorm(5), d = rnorm(5) )
print(y)
apply(y[,-1],1,function(x) cor(x[1:2],x[3:4]))
cor(y[c(2,3)])
print(head(cor(myfiles[[1]][2:3], use = "pairwise.complete.obs")))