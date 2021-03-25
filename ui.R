library(shiny)  



shinyUI(pageWithSidebar(  
  
  headerPanel("Discrete Random Variables"),  
  
  sidebarPanel( 
    
    selectInput("dismodel", "Select Model", 
                
                choices = c("Binomial" = "binomial", 
                            
                            "Poisson" = "poisson", 
                            
                            "Geometric" = "geometric"), 
                
                selected = "binomial" 
                
    ), 
    
    conditionalPanel( 
      
      condition = "input.dismodel == 'binomial'", 
      
      numericInput("n", "No. of trails" , value = 10), 
      
      numericInput("p", "Proability of success" , min = 0, max = 1,value = 0.5,step = 0.1) 
      
    ), 
    
    
    
    conditionalPanel(     
      
      condition = "input.dismodel == 'poisson'", 
      
      numericInput("lam", "parameter lambda in Poisson" , value = 1) 
      
    ), 
    
    
    
    conditionalPanel(     
      
      condition = "input.dismodel == 'geometric'", 
      
      numericInput("p", "parameter p in Geometric" ,min = 0, max = 1,value = 0.5,step = 0.1) 
      
    ), 
    
    
    
    numericInput("max", "upper limit for x" , value = 5),  
    
    sliderInput("s", "number of simulated data" ,min=1, max=10000, value = 10),  
    
    
    
    conditionalPanel( 
      
      condition = "input.dismodel == 'binomial'", 
      
      numericInput("j1", "j for Bin" , value = 1) 
      
    ), 
    
    
    
    conditionalPanel( 
      
      condition = "input.dismodel == 'poisson'", 
      
      numericInput("j2", "j for Poisson" , value = 1) 
      
    ), 
    
    
    
    conditionalPanel( 
      
      condition = "input.dismodel == 'geometric'", 
      
      numericInput("j3", "j for geometric" , value = 1) 
      
    ) 
    
    
    
    
    
  ),  
  
  mainPanel(  
    
    plotOutput("histogram"),  
    
    tableOutput('tab')  
    
  )  
  
))  





