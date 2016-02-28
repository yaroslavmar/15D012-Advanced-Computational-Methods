
# SPAM Dataset
library(formula.tools)
setwd("~/BGSE/15D012 Advanced Computational Methods/datasets/Spam/")
spam <- read.csv("spambase.data", header=FALSE)
n<-dim(spam)[1]
set.seed(111)
in.train<-sample(n, round(0.75*n) )

# train & test datasets
train<-spam[in.train,]
test<-spam[-in.train,]

#formula
variables<-paste(' V',1:57, sep="" )
variables<-paste(variables, collapse="+")
formula<-as.formula(paste('V58~', variables))



### rpart
library(rpart)

# grow tree
fit <- rpart(formula, method="class", data=train)

nodes<-c(3,5,7,9,11)
error.rpart.train<-rep(NA,length(nodes))
error.rpart.test<-rep(NA,length(nodes))
for(i in 1:length(nodes)){
    
  fit <- rpart(formula, data = train, method = "class", control = rpart.control( maxdepth = nodes[i], minsplit = 20))
  pred <- predict(fit, test, type = "class")
  pred.train<-predict(fit, train, type = "class")
  error.rpart.train[i]<-mean(pred.train!=train[,58])
  error.rpart.test[i]<-mean(pred!=test[,58])

}

### cTree


error.ctree.test<-rep(NA,length(nodes))
error.ctree.train<-rep(NA,length(nodes))

for(k in 1:length(nodes)){
  tree<-ctree(formula ,train, loss="ME", depth=nodes[k], minPoints=20, test=test)
  acc.ctree.test[k]<-mean(tree$predLabels==test$V58)
  acc.ctree.train[k]<-mean(tree$predLabels.train==test$V58)
}



#Create pdf 
pdf("rpart.vs.ctree.pdf")
plot(nodes,error.ctree.test, t="b", ylim=c(0.05,0.7), 
     xlab="Number of nodes", xaxt = "n",
     ylab="", col="blue",
     main="Misclassification error")
axis(at=nodes, side=1)
lines(nodes,error.ctree.train, t="b", col="blue", lty=2)
lines(nodes,error.rpart.train, t="b", col="red", lty=2)
lines(nodes,error.rpart.test, t="b", col="red")
grid()
legend("topright", c('ctree', 'rpart', 'test', 'train'), col=c('blue', 'red', 'black', 'black'), 
       lty=c(1,1, 1,2), cex=0.85)
dev.off()
