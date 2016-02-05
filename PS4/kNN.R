
kNN<-function(features, test,  labels, k, p){
  
  # Compute distance
  
  noObs <- nrow(test)
  distMatrix <- matrix(NA, nrow(features), noObs)
  for (obs in 1:noObs) {
    
    # getting the probe for the current observation
    probe <- test[obs,]
    probeExpanded <- matrix(probe, nrow = nrow(features), ncol = ncol(test), byrow = TRUE)
    probeExpanded<-apply(probeExpanded,2,as.numeric)
    # computing distances between the probe and exemplars in train dataset(features)
    if (p %in% c(1,2)) {
      distMatrix[,obs] <- ( rowSums((abs(features - 
                                            probeExpanded))^p) )^(1/p)
    } else if (p==Inf) {
      distMatrix[obs,] <- apply(abs(features - probeExpanded), 1, max)
    }  
  }
  

  
  #Find k- nearest neighbors class
  neighbors <- apply(as.matrix(distMatrix), 2, order)
  neighbors <- neighbors[1:k,]
  #rm(distMatrix)
  n<-nrow(test)
  neighbors.class<-sapply(1:n, function(i){
    labels[neighbors[,i]]
  })
  
  # Frequency of each class
  
  n.cat<-length(table(labels))
  
  freq<-matrix(0, n, n.cat)
  colnames(freq)<-names(table(labels))
  
  for( i in 1:n){
    
    categ<-table(match(as.factor(neighbors.class[,i]), colnames(freq)))
    
    if(length(categ)>1) {
      #freq[i,]<-categ
      freq[i,as.numeric(names(categ))]<-categ
    } else {
      
      #freq[i,as.numeric(names(categ))]<-categ
      freq[i,as.numeric(names(categ))]<-categ
    }
   
  }
  
  
  
  
  # predicted label
  PredLabels<- rep(NA,n)
  for(i in 1:n){
    PredLabels[i]<-colnames(freq)[which.max(freq[i,])]
    
  }
  #Probability of each class
  prob<-freq/k
  
  return(list(PredLabels=as.factor(PredLabels), prob=prob))  
}




