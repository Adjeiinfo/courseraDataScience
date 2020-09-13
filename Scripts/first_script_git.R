print("This file was created within RStudio")

print("And now it lives on GitHub")

library(datasets)
data(iris)

#question 1 
?iris
mean(iris[which(iris[,"Species"] == "virginica"),"Sepal.Length"])
mean(subset(iris$Sepal.Length, iris$Species=="virginica"))


#question2: mean for cols 1:4 
apply(iris[,1:4],2,mean)

#question 3
library(datasets)
data("mtcars")
?mtcars

#Question 3 
tapply(mtcars$mpg,mtcars$cyl,mean)
sapply(split(mtcars$mpg,mtcars$cyl),mean)
with(mtcars,tapply(mpg, cyl, mean))

#question 4 
82.63636-209.21429

makeVector <- function(x = numeric(),na.rm=TRUE,) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

cachemean <- function(x,na.rm=TRUE, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}

#testing 
x<-1:20
#create the special vector: Need to saved it in a named variable 
aVector <- makeVector(x)

#run the mean: user the named variable
cachemean(aVector)

#generating a random numbers from a linera model 
# y = b0 + b1x + epsilon 
set.seed(10)
#x <- rbinom(100,1,0.5)
x<-rnorm(100,0,2)
e <- rnorm(100,0,2)
y <- 0.5 + 2*x + e
summary(y)
plot(x,y)



