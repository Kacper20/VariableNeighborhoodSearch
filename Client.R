
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
  return (list(history=res[[2]], points=model[[4]], best=model[[1]]))
}

vectorsFunc <- function(points) {
  xPoints <- unlist(lapply(points, function(point){ point$coordinates[1] }))
  yPoints <- unlist(lapply(points, function(point){ point$coordinates[2] }))
  return (list(xPoints, yPoints))
}

## @knitr cec1_2_plot

newList<-pointFromVector(vectorOfRandomNumbers(2))
res <- alhe::vnsSearch(newList, vectorOf(low, length(newList)), vectorOf(high, length(newList)), evalcec(1), 30 * length(newList), 100, 50)

points <- vectorsFunc(resultList(res)$points)
qplot(points[1], points[2])

qplot(history[1], history[2])




## @knitr cec1_2_table

optimumValue <- -1400

vectorOfResults <- numeric()
vectorOfErrors <- numeric()
for (i in 1:2) {
  newList<-pointFromVector(vectorOfRandomNumbers(20))
  res <- alhe::vnsSearch(newList, vectorOf(low, length(newList)), vectorOf(high, length(newList)), evalcec(1), 100 * length(newList), 100, 50)
  best <- resultList(res)$best
  bestQuality <- best$quality
  vectorOfResults[i] <-bestQuality
  vectorOfErrors[i] <- bestQuality - optimumValue
}

df <- data.frame(vectorOfResults, vectorOfErrors)
kable(df, col.names=c("Wynik", "Blad"))
meanErr <- mean(vectorOfErrors)
cat("Średni błąd wyniósł: ", meanErr)










## @knitr test

newList<-pointFromVector(vectorOfRandomNumbers(2))
res <- alhe::vnsSearch(newList, vectorOf(low, length(newList)), vectorOf(high, length(newList)), evalcec(8), 20000, 300, 50)
points <- vectorsFunc(resultList(res)$points)
qplot(points[1], points[2])







