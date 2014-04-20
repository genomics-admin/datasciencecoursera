

read.csv("hw1_data.csv")

x <- list(2, "a", "b", TRUE)
x[[1]]
class(x[1])
class(x[[1]])

x <- 1:4
y <- 2
x + y

x <- c(3, 5, 1, 10, 12, 6)
x[x %in% 1:5] <- 0
x

head(read.csv("hw1_data.csv"),2)
tail(read.csv("hw1_data.csv"),2)

datastore <- read.csv("hw1_data.csv")
class(datastore)
datastore[47,]["Ozone"] 


datastore[47,]["Ozone"]
subset(datastore,is.na("Ozone"))

nrow(datastore)

j <- 0
for (i in 1:nrow(datastore)){
	
	if (is.na(datastore[i,]["Ozone"])){
		print(datastore[i,])
		j<- j+1
		print(j)
	}

}



j <- 0
k <- 0
meanval <- vector("list")
for (i in 1:nrow(datastore)){
	if (!is.na(datastore[i,]["Ozone"])){
		if(FALSE){print(datastore[i,]["Ozone"])}
		k <- k + datastore[i,]["Ozone"]
		j<- j+1
		meanval[j] <- datastore[i,]["Ozone"]
		if(FALSE){print(j)}
	}
}

print(k/j)

Reduce("+",meanval)/length(meanval)




j <- 0
k <- 0
meanval <- vector("list")
for (i in 1:nrow(datastore)){
	if (!is.na(datastore[i,]["Ozone"])){
		if ((datastore[i,]["Ozone"]>31) & (datastore[i,]["Temp"]>90)){
			print(datastore[i,])
			k <- k + datastore[i,]["Solar.R"]
			j<- j+1
			meanval[j] <- datastore[i,]["Solar.R"]
			if(FALSE){print(j)}
		}
	}
}

print(k/j)

Reduce("+",meanval)/length(meanval)






j <- 0
k <- 0
meanval <- vector("list")
for (i in 1:nrow(datastore)){
	if (!is.na(datastore[i,]["Month"])){
		if (datastore[i,]["Month"]==6){
			print(datastore[i,])
			k <- k + datastore[i,]["Temp"]
			j<- j+1
			meanval[j] <- datastore[i,]["Temp"]
			if(FALSE){print(j)}
		}
	}
}

print(k/j)

Reduce("+",meanval)/length(meanval)




j <- 0
k <- 0
meanval <- vector("list")
for (i in 1:nrow(datastore)){
	if (!is.na(datastore[i,]["Ozone"])){
		if (datastore[i,]["Month"]==5){
			print(datastore[i,])
			if (k < datastore[i,]["Ozone"]){
				k <- datastore[i,]["Ozone"]
			}
			j<- j+1
			meanval[j] <- datastore[i,]["Ozone"]
			if(FALSE){print(j)}
		}
	}
}

print(k)

#####################################################

install.packages(swirl)

library(swirl)  # Loads swirl
swirl()  # Runs swirl


#####################################################
cube <- function(x, n) {
  x^3
}

cube(3)

x <- 1:10
if(x > 5) {
  x <- 0
}


f <- function(x) {
  g <- function(y) {
    y + z
  }
  z <- 4
  x + g(x)
}

z <- 10
f(3)


x <- 5
y <- if(x < 3) {
  NA
} else {
  10
}


h <- function(x, y = NULL, d = 3L) {
  z <- cbind(x, d)
  if(!is.null(y))
    z <- z + y
  else
    z <- z + f
  g <- x + y / z
  if(d == 3L)
    return(g)
  g <- g + 10
  g
}

library(datasets)
data(iris)

iris

j<-0
k<-0
meanval <- vector("list")
for (i in 1:nrow(iris)){
    if (iris[i,]["Species"]=='virginica'){
      #print(iris[i,])
      k <- k + iris[i,]["Sepal.Length"]
      j<- j+1
      meanval[j] <- iris[i,]["Sepal.Length"]
      #if(FALSE){print(j)}
    }
}

print(k/j)

Reduce("+",meanval)/length(meanval)

rowMeans(iris[, 1:4])
colMeans(iris)
apply(iris, 1, mean)
apply(iris[, 1:4], 2, mean)

library(datasets)
data(mtcars)

mtcars

split(mtcars, mtcars$cyl)
tapply(mtcars$mpg, mtcars$cyl, mean)
tapply(mtcars$cyl, mtcars$mpg, mean)
sapply(mtcars, cyl, mean)

ls

debug(ls)

undebug(ls)

ls()

t1 <-list(mtcars$hp)
t1 <- mean(mtcars$hp)
t1

t1 <- mtcars[mtcars$cyl=='4',]
t2 <- mean(t1$hp)

t2

t3 <- mtcars[mtcars$cyl=='8',]
t4 <- mean(t3$hp)
t4

mean((mtcars[mtcars$cyl=='8',])$hp)-mean((mtcars[mtcars$cyl=='4',])$hp)



tapply(mtcars$hp, mtcars[mtcars$cyl=='4',]$cyl, mean)




set.seed(1)
rpois(5, 2)

set.seed(10)
x <- rbinom(10, 10, 0.5)
e <- rnorm(10, 0, 20)
y <- 0.5 + 2 * x + e
y
plot(x,y)



y <- 3
x1 <- 1
x2 <- 2
library(datasets)
Rprof()
fit <- lm(y ~ x1 + x2)
Rprof(NULL)

summaryRprof(by.total)
by.total


