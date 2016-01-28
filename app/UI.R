
shinyUI(pageWithSidebar(
  headerPanel('Linear classifier: loan data'),

  column(4,
         column(6, "Denied",
                sliderInput("mu.PI1", "PI ratio mean:", 
                            min=2, max=15, value=10),
                
                
                sliderInput("sd.PI1", "PI ratio sd", 
                            min=0, max=4, value=2),
                
                sliderInput("mu.sol1", "Solvency mean:", 
                            min=50, max=200, value=100),
                
                
                sliderInput("sd.sol1", "Solvency sd", 
                            min=10, max=40, value=30)
         ),
         
         
      column(6, "Accepted", 
         
         sliderInput("mu.PI0", "PI ratio mean:", 
                     min=2, max=15, value=4),
         
        
         sliderInput("sd.PI0", "PI ratio sd", 
                     min=0, max=4, value=1),
        
         sliderInput("mu.sol0", "Solvency mean:", 
                     min=50, max=200, value=150),
         
         
         sliderInput("sd.sol0", "Solvency sd", 
                     min=10, max=40, value=20)
  )),
  
  
  mainPanel(
    plotOutput('plot1')
  ,
  
  tableOutput('table'))
))