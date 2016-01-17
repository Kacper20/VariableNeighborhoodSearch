

#res <-alheLocalsearch::hillclimbing(newList, vectorOf(low, length(newList)), vectorOf(high, length(newList)), evalcec, maxIter=600)

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



newList<-pointFromVector(c(30, -10))
res <- alhe::vnsSearch(newList,vectorOf(low, length(newList)), vectorOf(high, length(newList)), evalcec(8), 20000, 300)



model <- res[[1]]
history <- res[[2]]
points <- model[[4]]
xPoints <- lapply(points, function(point){ point$coordinates[1] })
yPoints <- lapply(points, function(point){ point$coordinates[2] })
x <- unlist(xPoints)
y <- unlist(yPoints)
historyX <- unlist(lapply(history, function(point){point$coordinates[1]}))
historyY <- unlist(lapply(history, function(point){point$coordinates[2]}))

qplot(historyX, historyY)