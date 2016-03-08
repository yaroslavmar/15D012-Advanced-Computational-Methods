# AdaBoost Function
adaBoost<-function(formula, data, depth, noTrees, test){
  
  # Transform data
  labels<-get.vars(lhs(formula)) 
  vars<-get.vars(rhs(formula)) 
  
  y<-data[,labels]
  X<-data[,vars]
  
  y<-ifelse(y==0,-1,y)
  # Matrices for parameters
  
  Pred.Labels<-matrix(NA, nrow(X), noTrees)
  Pred.Labels.test<-matrix(NA, nrow(test), noTrees)
  #Pred.Labels<-matrix(NA, noTrees, nrow(X))
  alpha<-rep(NA, noTrees)
  
  # STEP 1. First Model
  n<-nrow(X)
  w<-rep(1/n, n)
  
  # STEP 2. M iterations
  for( m in 1:noTrees){
    tree<-rpart( as.factor(y) ~. , data=X, weights = w , control=rpart.control(maxdepth = depth))
    Pred.Labels[,m]<- as.character(predict(tree, type= "class"))
    Pred.Labels.test[,m]<- as.character(predict(tree,newdata = test, type= "class"))
    
    ind<-Pred.Labels[,m]!=y
    err<-sum(w*ind)/sum(w)
    alpha[m]<-log((1-err)/err)
    
    w<-w*exp(alpha[m]*ind)
    print(m)
  }
  
  # STEP 3. Final Classifier
  Pred.Labels<-apply(Pred.Labels , 2, as.numeric)
  Pred.Labels.test<-apply(Pred.Labels.test , 2, as.numeric)
  
  A<-diag(alpha)
  prediction<-rowSums(Pred.Labels%*%A)
  prediction<-sign(prediction)
  
  prediction.test<-rowSums(Pred.Labels.test%*%A)
  prediction.test<-sign(prediction.test)
  
  return(list(predLabels=prediction, predLabels.test=prediction.test))
  
}