


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


class(c(1, -1/4))

x=rbind(c(3,6,3),c(5,2,1),c(1,2,1))
det(x)
solve(x)


new_counter <- function() {
  i <- 0
  function() {
    # do something useful, then ...
    i <<- i + 1
    i
  }
}

counter_one <- new_counter()
counter_two <- new_counter()

counter_one() # -> [1] 1
counter_one() # -> [1] 2
counter_two() # -> [1] 1
counter_one()




t2 <- function(x){
  
  get <- function(x){
    print('...')
    print(solve(x))
  }
  
  set <- function() print('.....')
  list(set = set, get = get)
  
}

t1 <- function(x){
  print(x)
  #t3<-t2(x)
  #t3.get(x)
  x=as.data.frame(x)
  x$t2.set()
  
}


x=rbind(c(1, -1/4), c(-1/4, 1))
x
t1(x)


sandbox <-new.env()

sandbox$f <- function()
{
  value <- if(exists("x")) x else "not found."
  cat("This is function f looking for symbol x:", value, "\n")
}

sandbox$g <- function()
{
  x <- 123
  cat("This is function g. ")
  f()
}

environment(sandbox$f) <- sandbox
environment(sandbox$g) <- sandbox

sandbox$g()

t1<- function(x=matrix()){
  get1<-function(x){
    print(".")
    solve(x)
  }
  get2<-function() solve(x)
  
  list(get1=get1,get2=get2)
}


t2<- function(x){
  a <- t1(x)
  print(class(a))
  b <- a$get1(x)
  print(class(a$get1(x)))
  print(b)
}

x=rbind(c(1, -1/4), c(-1/4, 1))
t2(x)