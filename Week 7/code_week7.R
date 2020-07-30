
## -----------------------------------------------------------------------------
x <- rnorm(10)                               # Generate ten realisation from N(0,1)
x[x<0] <- 0                                  # Set all negative x to 0


## -----------------------------------------------------------------------------
x <- rnorm(10)                               # Generate ten realisation from N(0,1)
x <- ifelse(x<0, 0, x)                       # Set all negative x to 0


## -----------------------------------------------------------------------------
x <- 2
if (x==2) {
  print("x is 2")
} else {
  print("x is not 2")
}


## -----------------------------------------------------------------------------
x <- rnorm(1)
if (x>0) {
  x <- sqrt(x)
} else {
  x <- -sqrt(-x)
}

## -----------------------------------------------------------------------------
# Task 1
x <- runif(1,0,1)
y <- runif(1,0,1)
if (x > y){
  y <- x
} else {
  x <- y
}

## -----------------------------------------------------------------------------
x <- 10
x <- x/2 + 1/x
x <- x/2 + 1/x
x <- x/2 + 1/x
x <- x/2 + 1/x
x <- x/2 + 1/x
x <- x/2 + 1/x
x <- x/2 + 1/x
x <- x/2 + 1/x
x <- x/2 + 1/x
x <- x/2 + 1/x


## -----------------------------------------------------------------------------
for (i in 1:3)
    print(i)


## -----------------------------------------------------------------------------
for (day in c("M", "Tu", "W", "Th", "F")) 
    print(day)


## -----------------------------------------------------------------------------
x <- rnorm(10)
for (i in seq_along(x))
    if (x[i]<0)
        x[i] <- 0
x


## -----------------------------------------------------------------------------
n <- 1000
x <- numeric(n)
x[1] <- rnorm(1)


## -----------------------------------------------------------------------------
for (i in 2:n) {                            # We start the loop at i=2!
    epsilon <- rnorm(1)
    x[i] <- 0.8*x[i-1] + 0.6 * epsilon
}


## ----  dev.args=list(pointsize=10), out.width=".7\\textwidth"-----------------
plot(x, type="l")


## -----------------------------------------------------------------------------
x <- rnorm(100)                      # Generate white noise
x[sample(100,10)] <- NA              # Sneak in 10 missing values
# Task 2.
for (i in seq_along(x)){
  if (is.na(x[i]))
    x[i] <- 0
}

## -----------------------------------------------------------------------------
#Task 3
y <- x[!is.na(x)]                    # Only copy non-missing entries
x <- rnorm(100)                      # Generate white noise
x[sample(100,10)] <- NA              # Sneak in 10 missing values
# Task 2.
y=numeric(90)
j=1
for (i in seq_along(x)){
  y[j] <- x[i]
  if (!is.na(x[i]))
    j=j+1
  print(j)
  print(i)
}


## -----------------------------------------------------------------------------
for (i in 1:2)
    for (j in 1:3)
        print(c(i,j))


## -----------------------------------------------------------------------------
x <- seq(from=-3, to=3, length.out=50)      # Create sequence of grid for the x axis
y <- seq(from=-3, to=3, length.out=50)      # Create sequence of grid for the y axis
z <- matrix(nrow=length(x), ncol=length(y)) # Create matrix to store function values


## -----------------------------------------------------------------------------
for (i in seq_along(x))                     # For all rows ...
    for (j in seq_along(y))                 # For all columns ...
        z[i,j] <- dnorm(x[i])*dnorm(y[j])   # Compute f(x,y)


## -----------------------------------------------------------------------------
persp(x, y, z, theta=60, phi=30, col="yellow", shade=0.5) 


## -----------------------------------------------------------------------------
x <- 1
for (i in 1:10) 
    x <- x/2 + 1/x


## -----------------------------------------------------------------------------
x <- 1
for (i in 1:100) {
    x.old <- x                             # Store the old value of x
    x <- x/2 + 1/x                         # Update x
    if (abs(x-x.old)<1e-8)                 # Check for convergence
        break
}

i
## -----------------------------------------------------------------------------
#Task 4.
x = numeric(100)
x[1] <- 1
for (i in 2:100) {
  x[i] <- x[i-1]/2 + 1/x[i-1]                         # Update x
  if (abs(x[i]-x[i-1])<1e-8)                 # Check for convergence
    break
}


## -----------------------------------------------------------------------------
for (x in 1:10) {
    if (x%%2==0)
        next
    print(x)
}


## -----------------------------------------------------------------------------
x <- 1
x.old <- 0
while(abs(x-x.old)>1e-8) {
    x.old <- x                             # Store the old value of x
    x <- x/2 + 1/x                         # Update x
}
x


## -----------------------------------------------------------------------------
for (i in 1:3)
    print(i)


## -----------------------------------------------------------------------------
i <- 1
while (i<=3) {
    print(i)
    i <- i+1
}

## ---- echo=FALSE--------------------------------------------------------------
x <- rnorm(10)

## -----------------------------------------------------------------------------
for (i in seq_along(x))
    if (x[i]<0)
        x[i] <- 0


## -----------------------------------------------------------------------------
x <- ifelse(x<0, 0, x)


## -----------------------------------------------------------------------------
n <- 5
x <- sample(n)
y <- sample(n)


## -----------------------------------------------------------------------------
z <- numeric(n)                  # Create vector to hold result
for (i in 1:n) {
  if (x[i]>y[i]) {               # If x[i] is larger ...
    z[i] <- x[i]                 # ... set z[i] to  x[i]
  } else {                       # Otherwise (i.e. if x[i] is not larger) ...
    z[i] <- y[i]                 # ... set z[i] to  y[i]
  }
}


## -----------------------------------------------------------------------------
z <- ifelse(x>y, x, y)


## -----------------------------------------------------------------------------
z <- x                          # Start with a copy of x
select <- y>x                   # Find out for which entries y is larger than x
z[select] <- y[select]          # Set these to y

#Task 5. 
# 2,6,7

#Task 6.
x <- rnorm(10) # Generate some white noise
out <- numeric(length(x))
for (i in seq_along(x)) {
  if (x[i]>0) {
    out[i] <- x[i]
  } else {
    out[i] <- -x[i]
  }
}

x <- rnorm(10) # Generate some white noise
out <- ifelse(x>0, x, -x)

## ---- echo=FALSE--------------------------------------------------------------
library(compiler)
x <- enableJIT(3)


## -----------------------------------------------------------------------------
n <- 1e5
x <- rnorm(n)
y <- rnorm(n)


## -----------------------------------------------------------------------------
system.time(z <- x + y)


## -----------------------------------------------------------------------------
system.time( {
  z <- numeric(n)                  # Create vector of correct size
  for (i in 1:n)                   # Set entries one-by-one
    z[i] <- x[i]+y[i]
} )


## -----------------------------------------------------------------------------
system.time( {
  z <- c()
  for (i in 1:n)
    z <- c(z, x[i]+y[i])
} )


## -----------------------------------------------------------------------------
system.time( {
  z <- c()                           # Create an empty vector and let R extend it
  for (i in 1:n)                     # Set entries one-by-one
      z[i] <- x[i]+y[i]
})


## -----------------------------------------------------------------------------
system.time( {
    n <- length(x)
    d <- numeric(n-1)
    for (i in 1:(n-1))
        d[i] <- x[i+1] - x[i]
} )


## -----------------------------------------------------------------------------
system.time( {
    n <- length(x)
    d <- x[-1] - x[-n]
} )

## ---- echo=FALSE--------------------------------------------------------------
x <- rnorm(10)
x[x<0] <- 0

x <- rnorm(10)
for (i in 1:10){
  if (x[i] < 0)
    x[i] <- 0
}

x <- rnorm(10)
x <- ifelse(x<0, 0, x)

for (i in 2:10){
  x[i] <- x[i]+x[i-1]
}

#Task 10.
x <- 1
for (i in 1:50){
  x.old <- x
  x = 1 + 1/x
  if (abs(x-x.old)<1e-10)
    break
  print(i)
}
## -----------------------------------------------------------------------------



## -----------------------------------------------------------------------------
cumsum(x)


## ---- echo=FALSE--------------------------------------------------------------
set.seed(1234)

## ---- echo=FALSE, dev.args=list(pointsize=10), out.width=".7\\textwidth"------
n <- 10                                   # Simulate points
coords <- matrix(rnorm(2*n), ncol=2)     
plot(coords, pch=16, xlab="x", ylab="y")  # Draw the points
for (i in 1:(n-1))
  for (j in (i+1):n)
    lines(coords[c(i,j),], col="blue")    
closest.pair <- c(NA,NA)                   # Initialise closest pair
closest.distance <- Inf                    # Initialise closest distance
for (i in 1:(n-1))                         # Go through all pairs of points
  for (j in (i+1):n) {
    dist <- sum((coords[i,]-coords[j,])^2) # Compute (squared) distance
    if (dist<closest.distance) {           # If we find a pair which is closer ...
      closest.pair <- c(i,j)               # ... store it ...
      closest.distance <- dist             # ... along with the distance
    }     
  }
lines(coords[closest.pair,], col="red", lwd=4)

