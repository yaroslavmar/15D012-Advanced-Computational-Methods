
# CREATE DATA
genSun <- function(n = 200, 
                   features = 2, 
                   seed = NA, mus = NULL, sigma = NULL,
                   saveData = TRUE, 
                   savePlot = TRUE) {
  
  
  # Libraries
  if (! require(mvtnorm)) { stop('required package not installed: mvtnorm') }
  if (! require(ggplot2)) { stop('required package not installed: ggplot2') }
  
  # For simplicity we restrict to 2 or 3 dimensions (this can be relaxed)
  if (! as.numeric(features) %in% 2:3) {
    stop('argument "features" must be 2 or 3.')
  }
  
  # Default values
  if (! is.na(seed)) { set.seed(seed) }
  if (is.null(sigma)) { sigma <- diag(features) }
  if (is.null(mus)) { mus <- rep(0, features) }
  
  # Simulate points from a bivariate normal
  phi <- rmvnorm(n, mean = mus, sigma = sigma)
  
  # Decide which belong to each cluster
  rad <- (2 ** (features - 1) * gamma(1 + features / 2) /
            (pi ** (features / 2))) ** (1 / features)
  ones <- apply(phi, 1, function(x) { jitter(sum((x - mus) ** 2)) }) > rad ** 2
  #ones <- apply(phi, 1, function(x) { sum((x - mus) ** 2) }) > rad ** 2    
  category <- rep(0, length = n)
  category[ones] <- 1
  
  # Build the final data frame
  new.phi <- cbind.data.frame(phi, as.character(category))
  new.phi[, 3] <- as.factor(new.phi[, 3])
  colnames(new.phi) <- c("x1", "x2", 'y')
  
  return(new.phi)
}  


  
#train data
data<-genSun()
model<-kNN(data[,1:2], data[,1:2], data[,3], 5, 2)
PredLabels<-model$PredLabels
prob<-model$prob[,2]
newdata<-cbind(data,PredLabels,prob)
write.csv(newdata, 'predictions.csv', row.names = FALSE)

#data to plot boundaries
x<-seq(-3,3,length.out = 50)
y<-seq(-3,3, length.out = 50)
new <- expand.grid(x = x, y = y)
colnames(new)[1:2]<-colnames(data)[1:2]

#knn model
model<-kNN(data[,1:2], new, data[,3], 5, 2)
prob<-model$prob[,2]
PredLabels<-model$PredLabels

#compute probability
prob15 <- matrix(prob, length(x), length(y))

#plot data
pdf('plot.pdf')
par(mar=rep(2,4))
contour(x, y, prob15, levels=0.5, labels="", xlab="", ylab="", main="knn boundaries")
points(data[,1:2], col=ifelse(data[,3]==1, "coral", "cornflowerblue"), pch=16)
points(new, pch=".", cex=1.2, col=ifelse(prob15>0.5, "coral", "cornflowerblue"))
legend("topright", inset=.05,  c("1","0"), col=c("coral", "cornflowerblue"), pch=16)
dev.off()
