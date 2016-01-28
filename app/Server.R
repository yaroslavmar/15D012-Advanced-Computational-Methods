palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))

shinyServer(function(input, output, session) {
  
  # Combine the selected variables into a new data frame
  selectedData <- reactive({
    library(mvtnorm)
    # create small wrapper functions
    sigmaXY <- function(rho, sdX, sdY) {
      covTerm <- rho * sdX * sdY
      VCmatrix <- matrix(c(sdX^2, covTerm, covTerm, sdY^2), 
                         2, 2, byrow = TRUE)
      return(VCmatrix)
    }
    
    genBVN <- function(n = 1, seed = NA, muXY=c(0,1), sigmaXY=diag(2)) {
      if(!is.na(seed)) set.seed(seed)
      rdraws <- rmvnorm(n, mean = muXY, sigma = sigmaXY)
      return(rdraws)
    }
    
    loanData <- function(noApproved, noDenied, muApproved, muDenied, sdApproved, 
                         sdDenied, rhoApproved, rhoDenied, seed=1111) {
      sigmaApproved <- sigmaXY(rho=rhoApproved, sdX=sdApproved[1], sdY=sdApproved[2])
      sigmaDenied <- sigmaXY(rho=rhoDenied, sdX=sdDenied[1], sdY=sdDenied[2])
      approved <- genBVN(noApproved, muApproved, sigmaApproved, seed = seed)
      denied <- genBVN(noDenied, muDenied, sigmaDenied, seed = seed+1)
      loanDf <- as.data.frame(rbind(approved,denied))
      deny <- c(rep("Approved", noApproved), rep("Denied", noDenied))
      target = c(rep(0, noApproved), rep(1, noDenied))
      loanDf <- data.frame(loanDf, deny, target)
      colnames(loanDf) <- c("PIratio", "solvency", "deny", "target")
      return(loanDf)
    }
    
    # generating some data
    loanDf <- loanData(noApproved=50, noDenied=50, c(input$mu.PI0, input$mu.sol0), c(input$mu.PI1, input$mu.sol1), 
                       c(input$sd.PI0, input$sd.sol0), c(input$sd.PI1, input$sd.sol1), -0.1, 0.6, 1221)
    
    
    
  })
  
  
  output$plot1 <- renderPlot({
    library(ggplot2)
    
    #Model
    datafit <- lm(target ~ solvency + PIratio + 1, data=selectedData())
    
    #Boundary
    weights <- coef(datafit)[c("solvency", "PIratio")]
    bias <- coef(datafit)[1]
    
    intercept <- (-bias + 0.5)/weights["PIratio"]
    slope <- -(weights["solvency"]/weights["PIratio"])
    
    #Plot
    ggplot(data = selectedData(), aes(x = solvency, y = PIratio, 
                              colour=deny, fill=deny)) + 
      geom_point() +
      xlab("Solvency") +
      ylab("PIratio") +
      theme_bw() +
      geom_abline(intercept = intercept, slope = slope)
   
  })
  
  output$table <- renderTable({
    datafit <- lm(target ~ solvency + PIratio + 1, data=selectedData())
    
    # compute misclassification
    predictedLabels <- ifelse(predict(datafit) < 0.5, "Approved", "Denied")
    
    # confusion matrices
    confMatrixFreq <- table(selectedData()$deny, predictedLabels)
    
    confMatrixProp <- prop.table(confMatrixFreq, 1)
    confMatrixFreq })
  
})