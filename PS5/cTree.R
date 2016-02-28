
# Load required packages

if (!require("formula.tools")) install.packages("formula.tools")
library(formula.tools)

# Requiered functions
loss.fun<-function(prob,loss){
  
  if(loss=="ME") {return(1 - max(prob)) }
  if(loss=="Gini") {return(sum(prob*(1-prob)) )}
  if(loss=="Entropy"){return(- sum(prob*log(prob)))}
}


# Function: find the best split in a variable
best.split<-function(x, y, loss, minPoints){

  x.sort<-sort(x)
  y<-as.vector(y)
  noPoints <- length(x)
  errors <- rep(NA, noPoints-1)
  thresholds <- rep(NA, noPoints-1)

    
  p<-table(y)/length(y)
  general.score<-loss.fun(p, loss)
  
  for(i in 1:length(x)-1){
    potThres <- mean(x.sort[i:(i+1)])
    
    y.left<- as.vector(y[x<=potThres])
    y.right<-as.vector(y[x>potThres])
    
    if(length(y.left) < minPoints | length(y.right) < minPoints){ next}
    
    p.left<-table(y.left)/length(y.left)
    p.right<-table(y.right)/length(y.right) 
    
    l.left<-loss.fun(p.left, loss)
    l.right<-loss.fun(p.right, loss)
    
    loss.error<-l.left*length(y.left)/noPoints +l.right*length(y.right)/noPoints
    
     #record accuracy
     errors[i] <- loss.error
     thresholds[i] <- potThres
  }
  
  OK<-!is.na(errors)
  errors<-errors[OK]
  thresholds<-thresholds[OK]
  new.score<- min(errors)
  bestThreshold <- thresholds[which(errors==new.score)]
  # if more than 1 threshold has the same accuracy we choose the first
  bestThreshold <- bestThreshold[1]
  
  if(general.score<new.score){
    return(c(NA,NA))
  } else {
    return(c(bestThreshold, new.score))
  }
  
}

best.variable(X,y,"ME", 20)
best.variable(X[data.points,], y[data.points], loss, minPoints)
x<-X[data.points,1]
y<-y[data.points]
loss<-"Entropy"

#Function: Find best split among several variables
best.variable<-function(X,y, loss, minPoints){
  m<-dim(X)[2]
  results.variables<-sapply(1:m, function(i) best.split(X[,i], y, loss, minPoints))
  
  #potential splits
  OK<-!is.na(results.variables[1,])
  
  if(sum(!is.na(results.variables[1,]))==0) {
    result<-c(NA,NA,NA)
    return(result)
  } else {
  
  results.variables<-results.variables[,OK]
  
  # take best split
  best.error<-min(results.variables[2,])
  best.var<-which(results.variables[2,]==best.error)
  best.var<-best.var[1]
  
  #return best variable and threshold
  result<-c(best.var, results.variables[1,best.var], results.variables[2,best.var])
  names(result)<-c('Best Variable', 'Threshold', 'loss')
  return(result)
  }
}



data<-train
loss<-"ME"
depth<-3
minPoints<-30

# Function: Creates a decision tree
ctree<-function(formula, data, loss, depth, minPoints, test){
  
  #Data
  labels<-get.vars(lhs(formula)) #label
  vars<-get.vars(rhs(formula)) #predictors
  
  y<-data[,labels]
  X<-data[,vars]
  X.test<-test[,vars]
  
  #initialize index of tree
  index<-rep(0,nrow(X))
  index.test<-rep(0,nrow(X.test))
  Num.Nodes<-length(unique(index))
  ALL.SPLITS<-list()
  
  while(Num.Nodes<=depth){
    
    #all possible new splits
    pot.splits<-matrix(NA,length(unique(index)), 3)
    
    #computing potential splits
        for(j in 1:length(unique(index))){
        
         categories<-unique(index)
         data.points<-(index==categories[j])
         #tryCatch({
         pot.splits[j,]<-best.variable(X[data.points,], y[data.points], loss, minPoints)
         #},error=function(e){})
          }
    #Potential splits
    #OK<-!is.na(pot.splits[,3])
    OK<-which(!is.na(pot.splits[,3])==TRUE)
    
    if( sum(!is.na(pot.splits[,3]))==0 ){
      break
    } else{
    
    #Take best
    loss.trees<-pot.splits[OK,3]
    best.tree<-which(pot.splits[,3]==min(loss.trees))
    best.tree<-best.tree[1]
    ALL.SPLITS[[j]]<-pot.splits[best.tree,]
    current.tree<-pot.splits[best.tree,]
    
    #category that has the best split
    label<-unique(index)[best.tree]
    new.label<-max(index)+1
    index<-ifelse(X[,current.tree[1]]> current.tree[2] & index==label , new.label, index)
    index.test<-ifelse(X.test[,current.tree[1]]> current.tree[2] & index.test==label , new.label, index.test)
    Num.Nodes<-length(unique(index))
      }
  }
  
  #Prediction
  group.table<-tapply(y,index,table)
  prediction<-index
  prediction.test<-index.test
  probability<-index
  probability.test<-index.test
  
  for(i in 0:(length(group.table)-1)){
 
    #labels
    max.label<-which(group.table[[i+1]]==max(group.table[[i+1]]))
    prediction[prediction==i]<-names(group.table[[i+1]])[max.label]
    prediction.test[prediction.test==i]<-names(group.table[[i+1]])[max.label]
  
    #probability
    max.prob<-(group.table[[i+1]]/sum(group.table[[i+1]]))[max.label]
    probability[probability==i]<-max.prob
    probability.test[probability.test==i]<-max.prob
    }
  
  predLabels<-prediction.test
  prob<-probability.test
  predLabels.train<-prediction
  prob.train<-probability
  
  return(list(predLabels=predLabels, prob=prob, predLabels.train=predLabels.train, prob.train=prob.train))
  
  }

