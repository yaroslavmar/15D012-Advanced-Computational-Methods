
# Load data and packages 
source("adaBoost.R")

if (!require("formula.tools")) install.packages("formula.tools"); library(formula.tools)
if (!require("rpart")) install.packages("rpart"); library(rpart)
if (!require("h2o")) install.packages("h2o"); library(h2o)

data <- read.csv("spambase.data", header=FALSE)

#formula
variables<-paste(' V',1:57, sep="" )
variables<-paste(variables, collapse="+")
formula<-as.formula(paste('V58~', variables))

# Prediction with adaBoost
trees<-c(20,30,40,50,60)
acc<-rep(NA,length(trees))

for( i in 1:length(trees)){
  # Model
  pred<-adaBoost(formula, data[train,], 10, trees[i], data[-train,])
  
  # Prediction
  predicted<-ifelse(pred$predLabels.test==1,1,0)
  
  # Accuracy
  acc[i]<-mean(predicted==data[-train, 58])
}


# GBM in H2O
h2o.init()
data.h2o <- as.h2o(data)

dependent<-"V58"
independent<-variables<-paste('V',1:57, sep="" )
acc.h2o<-rep(NA,length(trees))

for(i in 1:length(acc.h2o)){
  #model
  model.gbm<-h2o.gbm(y = dependent, x = independent, training_frame = data.h2o[train,],
                     ntrees = trees[i], max_depth = 20, min_rows = 2)
  
  # Prediction
  predictions<-h2o.predict(model.gbm, newdata=data.h2o[-train,], n.trees=10 )
  pred<-as.numeric(as.vector(predictions[,1]))
  
  # Accuracy
  acc.h2o[i]<-mean(pred==data[-train,58])
}


# Plot accuracy of both models
pdf("adaBoost.pdf")
plot(trees,acc, t="b", col="darkblue", ylim=c(0.93,0.96), ylab="", pch=16,
     main= "Accuracy out-of-sample",
     xlab="Number of trees")
lines(trees,acc.h2o, t="b", col="red", pch=16)
legend('bottomright', c("adaBoost", "H2O - GBM"), pch=16, col=c("darkblue", "red"))
grid()
dev.off()
