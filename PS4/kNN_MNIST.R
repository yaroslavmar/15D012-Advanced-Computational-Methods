rm(list=ls())
library(class)

#Get data
train <- read.csv("~/BGSE/15D012 Advanced Computational Methods/datasets/MNIST/MNIST_training.csv", header=FALSE)
test <- read.csv("~/BGSE/15D012 Advanced Computational Methods/datasets/MNIST/MNIST_test.csv", header=FALSE)

#valuation dataset
set.seed(666)
out<-sample(1:6000, 5000)

#Find best model
k<-c(1,3,5,7,9,11,13)
acc<-matrix(NA,3, length(k)); colnames(acc)<-k
acc<-rep(NA,length(k)) ; names(acc)<-k

  for(i in 1:length(k)){
    model<-knn(train[-out,-1], train[out,-1],
               train[-out,1], k=k[i])
    
#     model<-kNN(train[-out,-1], train[out,-1],
#                 train[-out,1], k=j, p=i)
    
    acc[i]<-mean(model==train[out,1])
    print(i)
  }

# acc
#     1      3      5      7      9     11     13 
# 0.9280 0.9188 0.9086 0.9034 0.8970 0.8878 0.8816 
# 1-nn best model

#Prediction in test dataset
model_new<-knn(train[-out,-1], test ,train[-out,1], k=1)
write.csv(as.numeric(model_new), 'MNIST_predictions.csv', row.names = FALSE)
