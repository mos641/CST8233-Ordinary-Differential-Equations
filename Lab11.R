# Lab 11
# solving ODE

# function for solving ODE from x values 0 to 6, with provided steps
solveODE <- function(stepSize){
  # create a vector of x values and y values
  xVec <- seq(from = 0, to = 6, by = stepSize)
  yVec <- vector(mode = "numeric", length = length(xVec))
  # define ynot as provided value
  yVec[1] <- 1.241
  
  # loop through to calculate each y value
  i <- 1
  while(i < length(xVec)){
    # f(x,y) = -ycosx     y(i+1) = yi + f(x,y)h
    yVec[i+1] <- yVec[i] + ( (-yVec[i]*cos(xVec[i])) * stepSize )
    i <- i + 1
  }
  
  # return our y values
  return(yVec)
}

# caclulate observed values and the errors
errors <- function(yFitted){
  # calculate the x values
  allData <- data.frame("x.values" = seq(from = 0, to = 6, by = 0.5), "y.observed" = c(1:length(yFitted)), "y.fitted" = yFitted, "absolute.errors" = c(1:length(yFitted)), "relative.errors" = c(1:length(yFitted)))
  
  # loop to calculate observed values and errors
  i <- 1
  while(i <= length(yFitted)){
    # caclulate observed values y = 0.5e^(sin2)e^(-sinx)
    allData$y.observed[i] <- 0.5 * exp(sin(2)) * exp(-sin(allData$x.values[i]))
    
    # calculate absolute error
    allData$absolute.errors[i] <- abs(allData$y.observed[i] - allData$y.fitted[i])
    
    # calculate relative error
    allData$relative.errors[i] <- abs(allData$absolute.errors[i] / allData$y.observed[i]) * 100
    
    i <- i + 1
  }
  
  # return our data
  return(allData)
}

# calculate the various step sizes
yStep5 <- solveODE(0.5)
yStep25 <- solveODE(0.25)
yStep1 <- solveODE(0.1)

# calculate the errors
errorDF <- errors(yStep5)

# calculate exact values by 0.01 for graphing
exactX <- seq(from = 0, to = 6, by = 0.01)
exactVals <- 0.5 * exp(sin(2)) * exp(-sin(exactX))

# print data frame
print(errorDF)

# plot the different graphs
plot(exactX, exactVals, ylim = c(0, 3.5), type="l", lty=1, col="black", main="Displacement vs Time",
     sub = "Observed in black, step by 0.1 blue, 0.25 green, 0.5 red",xlab="Time in Seconds", ylab="Displacement")
lines(seq(from = 0, to = 6, by = 0.1), yStep1, col="blue")
lines(seq(from = 0, to = 6, by = 0.25), yStep25, col="green")
lines(seq(from = 0, to = 6, by = 0.5), yStep5, col="red")

# 
# # ODE step by .5
# plot(seq(from = 0, to = 6, by = 0.5), yStep5, type="l", lty=1, col="blue", main="ODE step by 0.5 Displacement vs Time",
#      xlab="Time in Seconds", ylab="Displacement")
# 
# # ODE step by .25
# plot(seq(from = 0, to = 6, by = 0.25), yStep25, type="l", lty=1, col="blue", main="ODE step by 0.25 Displacement vs Time",
#      xlab="Time in Seconds", ylab="Displacement")
# 
# # ODE step by .1
# plot(seq(from = 0, to = 6, by = 0.1), yStep1, type="l", lty=1, col="blue", main="ODE step by 0.1 Displacement vs Time",
#      xlab="Time in Seconds", ylab="Displacement")
# 
# # solution by 0.5
# plot(seq(from = 0, to = 6, by = 0.5), errorDF$y.observed, type="l", lty=1, col="blue", main="Observed Displacement vs Time",
#      xlab="Time in Seconds", ylab="Displacement")
# 
# # plot the fitted line
# lines(seq(from = 0, to = 6, by = 0.5), errorDF$y.fitted, col="black")
