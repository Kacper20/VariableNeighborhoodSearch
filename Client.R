
## @knitr setup1
low <- -100
high <- 100
pointFromVector<-function(vec) {
  embedded <- list(coordinates=vec)
  return (list(embedded))
}

eval <- function(coordinates) {
  return (coordinates * (coordinates - 2) * (coordinates + 2) * (coordinates - 5) )
}
eval1 <- function(coordinates) {
  val <- (coordinates[1] - 2) * coordinates[1] + coordinates[2] * coordinates[2]
  return (val)
}
vectorOf <-function(number, dim) {
  return (c(rep(number, dim)))
}
evalcec <- function(problemNumber) {
  function(coordinates) {
    return (cec2013(problemNumber,coordinates))
  }
}


vectorOfRandomNumbers <- function(dimension) {
  randomVector <- sample(0:100, dimension, replace=T)
  return (randomVector)
}

resultList <- function(res) {
  model <- res[[1]]
  return (list(history=res[[2]], points=model[[4]], best=model[[1]], bestQuality=model[[5]]))
}

getVectors <- function(points) {
  xPoints <- unlist(lapply(points, function(point){ point$coordinates[1] }))
  yPoints <- unlist(lapply(points, function(point){ point$coordinates[2] }))
  return (list(xPoints, yPoints))
}

getQualityVector <- function(points) {
  quality <- unlist(lapply(points, function(point) {point$quality}))
  return (quality)
}

generateTable <- function(optimumValue, iterations, dimension, cecProblem, maxIterFactor, kmax, localSearches, density) {
  vectorOfResults <- numeric()
  vectorOfErrors <- numeric()
  vectorOfRelativeErrors <- numeric()
  for (i in 1:iterations) {
    newList<-pointFromVector(vectorOfRandomNumbers(dimension))
    res <- alhe::vnsSearch(newList, vectorOf(low, length(newList)), vectorOf(high, length(newList)), evalcec(cecProblem), maxIterFactor * dimension, kmax, localSearches, density)
    b <- resultList(res)$best
    bestQuality <- b[[2]]
    vectorOfResults[i] <-bestQuality
    vectorOfErrors[i] <- bestQuality - optimumValue
    vectorOfRelativeErrors[i] <- abs((bestQuality - optimumValue)/optimumValue)
  }
  df <- data.frame(vectorOfResults, vectorOfErrors, vectorOfRelativeErrors)
  meanErr <- mean(vectorOfErrors)
  cat("Średni błąd wyniósł: ", meanErr)
  return (df)
}




## @knitr cec1_2_setup


numberOfIters <- 1
newList<-pointFromVector(vectorOfRandomNumbers(2))
res <- alhe::vnsSearch(newList, vectorOf(low, length(newList)), vectorOf(high, length(newList)), evalcec(1), 500 * length(newList[[1]]), 50, 50, 0.25)



## @knitr cec1_2_plot1


#Wykres wszystkich punktów z historii
x <- resultList(res)$points
pts <- getVectors(x)
q1_21 <- qplot(pts[[1]], pts[[2]])
q1_21 <- q1_21 + labs(title = "Changing best points chart")
q1_21 <- q1_21 + labs(x="X axis")
q1_21 + labs(y="Y axis")




## @knitr cec1_2_plot2

history <- resultList(res)$history
historyPts <- getVectors(history)
q1_22 <- qplot(historyPts[[1]], historyPts[[2]])
q1_22 <- q1_22 + labs(title = "History of points chart")
q1_22 <- q1_22 + labs(x="X axis")
q1_22 + labs(y="Y axis")


## @knitr cec1_2_plot3

bestQA <-resultList(res)$bestQuality
qualityX <- bestQA[[2]]
qualityY <- getQualityVector(bestQA[[1]])
q1_23 <- qplot(qualityX, qualityY)
q1_23 <- q1_23 + labs(title = "Change of quality over iterations")
q1_23 <- q1_23 + labs(x="Iteration number")
q1_23 + labs(y="Quality value")




## @knitr cec1_2_table2

df <- generateTable(optimumValue = -1400, iterations =  numberOfIters, dimension = 2, cecProblem = 1, maxIterFactor = 500, kmax = 50, localSearches = 50 , density=0.25)
knitr::kable(df, col.names=c("Result", "Error", "Relative error"))


## @knitr cec1_2_table5

df <- generateTable(optimumValue = -1400, iterations =  numberOfIters, dimension = 5, cecProblem = 1, maxIterFactor = 500, kmax = 50, localSearches = 50, density=0.25 )
knitr::kable(df, col.names=c("Result", "Error", "Relative error"))

## @knitr cec1_2_table20

df <- generateTable(optimumValue = -1400, iterations =  numberOfIters, dimension = 20, cecProblem = 1, maxIterFactor = 500, kmax = 50, localSearches = 50, density=0.25 )
knitr::kable(df, col.names=c("Result", "Error", "Relative error"))

## @knitr cec1_2_table20_higher_density

df <- generateTable(optimumValue = -1400, iterations =  numberOfIters, dimension = 20, cecProblem = 1, maxIterFactor = 500, kmax = 200, localSearches = 50, density=0.05 )
knitr::kable(df, col.names=c("Result", "Error", "Relative error"))






########################
########################
########################
########################
########################
########################



## @knitr cec8_2_setup


numberOfIters <- 5
newList<-pointFromVector(vectorOfRandomNumbers(2))
res <- alhe::vnsSearch(newList, vectorOf(low, length(newList)), vectorOf(high, length(newList)), evalcec(8), 500 * length(newList[[1]]), 500, 50, 0.25)



## @knitr cec8_2_plot1


#Wykres wszystkich punktów z historii
x <- resultList(res)$points
pts <- getVectors(x)
q8_21 <- qplot(pts[[1]], pts[[2]])
q8_21 <- q8_21 + labs(title = "Changing best points chart")
q8_21 <- q8_21 + labs(x="X axis")
q8_21 + labs(y="Y axis")




## @knitr cec8_2_plot2

history <- resultList(res)$history
historyPts <- getVectors(history)
q8_22 <- qplot(historyPts[[1]], historyPts[[2]])
q8_22 <- q8_22 + labs(title = "History of points chart")
q8_22 <- q8_22 + labs(x="X axis")
q8_22 + labs(y="Y axis")


## @knitr cec8_2_plot3

bestQA <-resultList(res)$bestQuality
qualityX <- bestQA[[2]]
qualityY <- getQualityVector(bestQA[[1]])
q8_23 <- qplot(qualityX, qualityY)
q8_23 <- q8_23 + labs(title = "Change of quality over iterations")
q8_23 <- q8_23 + labs(x="Iteration number")
q8_23 + labs(y="Quality value")




## @knitr cec8_2_table2

df <- generateTable(optimumValue = -700, iterations =  numberOfIters, dimension = 2, cecProblem = 8, maxIterFactor = 500, kmax = 500, localSearches = 50 , density=0.25)
knitr::kable(df, col.names=c("Result", "Error", "Relative error"))


## @knitr cec8_2_table5

df <- generateTable(optimumValue = -700, iterations =  numberOfIters, dimension = 5, cecProblem = 8, maxIterFactor = 500, kmax = 500, localSearches = 50, density=0.25 )
knitr::kable(df, col.names=c("Result", "Error", "Relative error"))

## @knitr cec8_2_table20

df <- generateTable(optimumValue = -700, iterations =  numberOfIters, dimension = 20, cecProblem = 8, maxIterFactor = 500, kmax = 500, localSearches = 50, density=0.25 )
knitr::kable(df, col.names=c("Result", "Error", "Relative error"))

## @knitr cec8_2_table20_higher_density

df <- generateTable(optimumValue = -700, iterations =  numberOfIters, dimension = 20, cecProblem = 8, maxIterFactor = 1000, kmax = 600, localSearches = 50, density=0.05 )
knitr::kable(df, col.names=c("Result", "Error", "Relative error"))

########################
########################
########################
########################
########################
########################



## @knitr cec14_2_setup


numberOfIters <- 5
newList<-pointFromVector(vectorOfRandomNumbers(2))
res <- alhe::vnsSearch(newList, vectorOf(low, length(newList)), vectorOf(high, length(newList)), evalcec(14), 1000 * length(newList[[1]]), 500, 50, 0.25)



## @knitr cec14_2_plot1


#Wykres wszystkich punktów z historii
x <- resultList(res)$points
pts <- getVectors(x)
q1_21 <- qplot(pts[[1]], pts[[2]])
q1_21 <- q1_21 + labs(title = "Changing best points chart")
q1_21 <- q1_21 + labs(x="X axis")
q1_21 + labs(y="Y axis")




## @knitr cec14_2_plot2

history <- resultList(res)$history
historyPts <- getVectors(history)
q1_22 <- qplot(historyPts[[1]], historyPts[[2]])
q1_22 <- q1_22 + labs(title = "History of points chart")
q1_22 <- q1_22 + labs(x="X axis")
q1_22 + labs(y="Y axis")


## @knitr cec14_2_plot3

bestQA <-resultList(res)$bestQuality
qualityX <- bestQA[[2]]
qualityY <- getQualityVector(bestQA[[1]])
q1_23 <- qplot(qualityX, qualityY)
q1_23 <- q1_23 + labs(title = "Change of quality over iterations")
q1_23 <- q1_23 + labs(x="Iteration number")
q1_23 + labs(y="Quality value")




## @knitr cec14_2_table2

df <- generateTable(optimumValue = -100, iterations =  numberOfIters, dimension = 2, cecProblem = 14, maxIterFactor = 1000, kmax = 500, localSearches = 50 , density=0.25)
knitr::kable(df, col.names=c("Result", "Error", "Relative error"))


## @knitr cec14_2_table5

df <- generateTable(optimumValue = -100, iterations =  numberOfIters, dimension = 5, cecProblem = 14, maxIterFactor = 1000, kmax = 500, localSearches = 50, density=0.25 )
knitr::kable(df, col.names=c("Result", "Error", "Relative error"))

## @knitr cec14_2_table20

df <- generateTable(optimumValue = -100, iterations =  numberOfIters, dimension = 20, cecProblem = 14, maxIterFactor = 1000, kmax = 500, localSearches = 50, density=0.25 )
knitr::kable(df, col.names=c("Result", "Error", "Relative error"))

## @knitr cec14_2_table20_higher_density

df <- generateTable(optimumValue = -100, iterations =  numberOfIters, dimension = 20, cecProblem = 14, maxIterFactor = 1000, kmax = 600, localSearches = 50, density=0.05 )
knitr::kable(df, col.names=c("Result", "Error", "Relative error"))










