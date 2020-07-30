
## -----------------------------------------------------------------------------
x <- rnorm(1)
y <- rnorm(1)
if (x<y) {
    x <- y
} else {
    y <- x
}


## -----------------------------------------------------------------------------
x <- y <- max(x,y)

## -----------------------------------------------------------------------------
y <- numeric(length(x))
for (i in seq_along(x)) 
    if (!is.na(x[i])) {
        y[i] <- x[i]
    } else {
        y[i] <- 0
    }


## -----------------------------------------------------------------------------
y <- x
y[is.na(y)] <- 0


## -----------------------------------------------------------------------------
y <- c()
for (i in seq_along(x)) 
    if (!is.na(x[i])) 
        y <- c(y, x[i])


## -----------------------------------------------------------------------------
n <- 100
x <- numeric(n)
x[1] <-  1
for (i in 2:100) {
    x[i] <- x[i-1]/2 + 1/x[i-1]           # Update x
    if (abs(x[i]-x[i-1])<1e-8)            # Check for convergence
        break
}
x[i]



## -----------------------------------------------------------------------------
x <- rnorm(1)
if (x<0) {
  x <- 0
}
x


## -----------------------------------------------------------------------------
x <- rnorm(10)
x <- ifelse(x<0, 0, x)
x


## -----------------------------------------------------------------------------
x <- rnorm(10)
for (i in seq_along(x))
    if (x[i]<0)
        x[i] <- 0
x


## -----------------------------------------------------------------------------
x <- rnorm(10)
x[x<0] <- 0
print(x)


## -----------------------------------------------------------------------------
cumsum.x <- numeric(length(x))            # Create empty vector to hold result

cumsum.x[1] <- x[1]                       # Set first entry
for (i in 2:length(x))
  cumsum.x[i] <- cumsum.x[i-1]+x[i]       # Set remaining entries

cumsum.x                                  # Print result
cumsum(x)                                 # Compare to built-in function


## -----------------------------------------------------------------------------
x <- 1                                    # Set initial value (arbitrary)
for (i in 1:50) {                         # Repeat at most 50 times
  old.x <- x                              # Store old value
  x <- 1 + 1/x                            # Update x
  if (abs(old.x-x)<10e-10)                # Check for convergence
    break
}

x                                         # Print result
(1+sqrt(5)) / 2                           # Compare to desired answer


## ---- echo=FALSE--------------------------------------------------------------
set.seed(1234)

## ---- dev.args=list(pointsize=10), out.width=".7\\textwidth"------------------
n <- 10                                   # Simulate points
coords <- matrix(rnorm(2*n), ncol=2)     
plot(coords, pch=16, xlab="x", ylab="y")  # Draw the points
for (i in 1:n)                            # For all pairs of points ...
  for (j in 1:n)
    lines(coords[c(i,j),], col="blue")    # Connect i-th and j-th point


## -----------------------------------------------------------------------------
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


## ---- echo=FALSE, dev.args=list(pointsize=10), out.width=".7\\textwidth"------
plot(coords, pch=16, xlab="x", ylab="y")  # Draw the points
for (i in 1:(n-1))
  for (j in (i+1):n)
    lines(coords[c(i,j),], col="blue")    
lines(coords[closest.pair,], col="red", lwd=4)
                                           # Connect two closest points



## -----------------------------------------------------------------------------
n.sim <- 1e4                     # Set number of simulations
results <- logical(n.sim)        # Vector to hold result
for (i in 1:n.sim) {             # Perform n.sim simulations
  while (TRUE) {                 # Keep sampling ...
    car <- sample(3,1)           # Randomly place the car
    if (car==1) door <- sample(2:3,1)
    if (car==2) door <- 3
    if (car==3) door <- 2
    if (door==2)                 # ... until host opens door 2
      break
  }
  results[i] <- car==1           # Record whether car is behind door 1
}
mean(results)

