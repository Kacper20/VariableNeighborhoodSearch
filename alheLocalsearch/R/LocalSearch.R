#A general pattern of a metaheuristic method
#(C)Jaroslaw Arabas, ALHE, 2012
#To define the METHOD completely the user must
#code the procedures of selection, model update, and variation.
#Proper execution of the metaheuristic method needs
#a list of start points, an evaluation method
#an initialization procedure, and a termination condition

############################################################
#GLOBAL VARIABLES - TODO: Get it from user.


density<- 0.5
low <- c(-100)
high <- c(100)

####HELPER FUNCTIONS


hchange=function(par,lower,upper,dist,round=TRUE,...)
{
  D=length(par) # dimension
  step=dist(D,...) # slight step
  if(round) step=round(step)
  par1=par+step
  # return par1 within [lower,upper]:
  return(ifelse(par1<lower,lower, ifelse(par1>upper,upper,par1)))
}

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
modelUpdate<-function(selectedPoints, oldModel)
{
  #take a look at the list of selectedPoints and
  #on the current state of the model, update it
  #and then return
  modelPoint <- oldModel[[1]]
  iterations <- oldModel[[2]] + 1
  for (i in 1:length(selectedPoints)) {
    if (selectedPoints[[i]]$quality < modelPoint$quality) {
      return (list(selectedPoints[[i]], iterations))
    }
  }
  return (list(modelPoint, iterations))
}

#generation of a LIST of new points
#to be defined



variation<-function(selectedPoints, model)
{
  #generate the list of newPoints and then
  numberOfNewPoints <- 10

  position <- selectedPoints[[1]]$coordinates
  newList<- list()
  for (i in 1:numberOfNewPoints) {
    newCoordinates<- hchange(position, low, high, rnorm, mean=0, sd=0.5, round=FALSE)
    point <- list(coordinates=newCoordinates)
    newList[[i]] = point
  }
  return (newList)
}

#####  THE METAHEURISTIC "ENGINE"

#An aggregated operator takes the list of historical points anf the model
#and generates the list of new points
#A "side effect" is the model update
aggregatedOperator<-function(history, oldModel, eval)
{

  selectedPoints<-selection(history, oldModel)
  #newModel<-modelUpdate(selectedPoints, oldModel)
  newPoints<-variation(selectedPoints, newModel)
  newPoints<-evaluateList(newPoints, eval)
  newModel<-modelUpdate(newPoints, oldModel)
  return (list(newPoints=newPoints,newModel=newModel))
}

#The main loop of a metaheuristic.
#The user must define a LIST of start points,
#a termination condition, an initialization procedure
#and an evaluation procedure.
#The result is the history of the run
metaheuristicRun<-function(initialization, startPoints, termination, eval)
{

  history<-initialization(startPoints)
  history<-evaluateList(history, eval)
  model<-initModel(history)
  while (!termination(history,model))
  {
    aa<-aggregatedOperator(history, model, eval)
    #aa$newPoints<-evaluateList(aa$newPoints, eval)
    #history<-historyPush(history,aa$newPoints)
    model<-aa$newModel
  }
  return(list(history=history, model=model))
}







#initializes model - DONE.
initModel<-function(history)
{
  return (list(history[[1]], 0))
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


####  THAT'S ALL FOLKS
####FUNCTION PARAMETERS


#Public function
#point: List, where List::coordinates is vector of numeric values(of size dimension)
#low: vector of lowest possible values of coordinates in each dimension(size dimension)
#heigh: vector of highest possible values of coordinates in each dimension(size dimension)
#Returns

hillclimbing<-function(point, low, high, goalFunction, maxIter = 500)
{
  initialization <- function (startPoints) {
    return (startPoints)
  }
  termination <- function(history, model)
  {
    return (model[2] > maxIter)
  }
  result <- metaheuristicRun(initialization, point, termination, goalFunction)
  return (result$model)
}















