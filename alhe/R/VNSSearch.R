#GLOBAL VARIABLES - TODO: Get it from user.


####HELPER FUNCTIONS

#Functions of probabilistic sirtsibution. Gets low and high boundaries, and returns another specialized function
normalDistribution <- function(low, high)
{
  function(position, k) {
    value <- 0.25 * k
    val <-  hchange(position, low, high, rnorm, mean=0, sd=value, round=FALSE)
    return (val)
  }
}
#function that returns vector of random coordinates.

hchange=function(par,lower,upper,dist,round=TRUE,...)
{
  D=length(par) # dimension
  step=dist(D,...) # slight step
  if(round) step=round(step)
  par1=par+step
  # return par1 within [lower,upper]:
  return(ifelse(par1<lower,lower, ifelse(par1>upper,upper,par1)))
}


#MODEL :
#1: list that defines point structure
#2: current number of iterations
#3: current k
#### TO BE DEFINED BY THE USER

#selection of a LIST of points from the history
#to be defined
selection<-function(history, model)
{
  #select a number of points from the history using the
  #method's parameters and the current state of the model
  return (model[1]) # return best point from current model
}

#update of a model based on a LIST of points
#to be defined
modelUpdate<-function(selectedPoints, oldModel, evalFunc, low, high, numberOfLocalSearches)
{
  #take a look at the list of selectedPoints and
  #on the current state of the model, update it
  #and then return
  model <- alheLocalsearch::hillclimbing(selectedPoints, low, high, evalFunc, length(low) * numberOfLocalSearches )
  newPoint <- model[[1]]
  evaluatedPoints <- evaluateList(list(newPoint), evalFunc)
  singlePoint <- evaluatedPoints[[1]]

  modelPoint <- oldModel[[1]]
  iterations <- oldModel[[2]] + 1
  cat("k : ", oldModel[[3]])
  cat(" Current Iter: ", oldModel[[2]], "\n")
  if (singlePoint$quality < modelPoint$quality) {

    cat("Actual best: ", singlePoint$coordinates, "\n")
    cat("Quality: ", singlePoint$quality, "\n")
    old <- oldModel[[4]]
    old[[length(old)+1]] <- singlePoint
    return (list(singlePoint, iterations, 1,old))
  }
  else {
    return (list(modelPoint, iterations, oldModel[[3]] + 1, oldModel[[4]]))
  }
}

#generation of a LIST of new points
#to be defined



variation<-function(selectedPoints, model, generationFunction)
{
  #generate the list of newPoints and then
  position <- selectedPoints[[1]]$coordinates
  k <- model[[3]]
  newCoordinates<- generationFunction(position, k)
  point <- list(coordinates=newCoordinates)
  return (list(point))
}

#####  THE METAHEURISTIC "ENGINE"

#An aggregated operator takes the list of historical points anf the model
#and generates the list of new points
#A "side effect" is the model update
aggregatedOperator<-function(history, oldModel, eval, low, high, numberOfLocalSearches)
{
  selectedPoints<-selection(history, oldModel)
  #newModel<-modelUpdate(selectedPoints, oldModel)
  newPoints<-variation(selectedPoints, oldModel, normalDistribution(low, high))
  newModel<-modelUpdate(newPoints, oldModel, eval, low, high, numberOfLocalSearches)
  return (list(newPoints=newPoints,newModel=newModel))
}

#The main loop of a metaheuristic.
#The user must define a LIST of start points,
#a termination condition, an initialization procedure
#and an evaluation procedure.
#The result is the history of the run
metaheuristicRun<-function(initialization, startPoints, termination, eval, low, high, numberOfLocalSearches)
{

  history<-initialization(startPoints)
  history<-evaluateList(history, eval)
  model<-initModel(history)
  while (!termination(history,model))
  {
    aa<-aggregatedOperator(history, model, eval, low, high, numberOfLocalSearches)
    #aa$newPoints<-evaluateList(aa$newPoints, eval)
    history<-historyPush(history,aa$newPoints)
    model<-aa$newModel
  }
  return(list(model=model, history=history))
}
#Public function
#point: List, where List::coordinates is vector of numeric values(of size dimension)
#dimension:
#low: vector of lowest possible values of coordinates in each dimension(size dimension)
#heigh: vector of highest possible values of coordinates in each dimension(size dimension)
#Returns
#hillclimbing(point, dimension, low, heigh, goalFunction, maxIter = 500)
#{


#return metaheuristicRun


#}

#initializes model - DONE.
initModel<-function(history)
{
  return (list(points=history[[1]], iterations=0, k=1, bestHistory=history[1]))
}

#push a LIST of points into the history
historyPush<-function(oldHistory, newPoints)
{
  newHistory<-c(oldHistory,newPoints)
  return (newHistory)
}
#read a LIST of points pushed recently into the history
historyPop<-function(history, number)
{
  stop=length(history)
  start=max(stop-number+1,1)
  return(history[start:stop])
}

#evaluate a LIST of points
evaluateList<-function(points, evaluation)
{
  for (i in 1:length(points))
    points[[i]]$quality<-evaluation(points[[i]]$coordinates)
  return (points)
}

vnsSearch <- function(points , low, high, goalFunction, maxIter, kMax, numberOfLocalSearches) {

  initialization <- function (startPoints) {
    return (startPoints)
  }

  termination <- function(history, model)
  {
    return (model[2] > maxIter || model[3] > kMax)
  }
  result <- metaheuristicRun(initialization, points, termination, goalFunction, low, high, numberOfLocalSearches)
  return (result)

}








