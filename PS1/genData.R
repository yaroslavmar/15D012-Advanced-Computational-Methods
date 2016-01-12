
#EXERCISE 1
library(ggplot2)

genData<-function(obs0=100, obs1=100, variance= 0.01, save.data=FALSE, save.plot=FALSE){
  
  # Generating data
  num<-seq(-1,1,length.out = obs1)
  
  x1<-sin(num)
  y1<-cos(num)+rnorm(obs1, 0, variance)
  
  x0<-seq(-1,1,length.out = obs0)
  y0<-seq(0.6,1,length.out = obs0)+rnorm(obs0,0,variance)
  
  x<-c(x0,x1)
  y<-c(y0,y1)
  target<-c(rep(0,obs0), rep(1,obs1))
  
  data<-data.frame(x=x,y=y,target=target)
  data$target<-as.factor(data$target)
  
  # Save data
  if(save.data==TRUE){
    write.table(data, 'dataset.csv', row.names = FALSE, sep=";")
  }
  
  # Create plot
  if(save.plot==TRUE){
    
    pdf(file='dataPlot.pdf')
    
    ggplot(data = data, aes(x = x, y = y, colour=target, fill=target)) +     
      geom_point(size=3) +
      theme_bw() 
    
    dev.off()
    
  }
  return(data)
}

genData(200,200,0.01, save.data = TRUE, save.plot = TRUE)
