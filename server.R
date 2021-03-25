# server.R 



#install.packages('shiny') 



library(shiny)  



shinyServer(  
  
  function(input, output) {  
    
    
    
    output$histogram <- renderPlot({ 
      
      
      
      # binomial  
      
      if (input$dismodel == 'binomial') { 
        
        par(mfrow=c(2,1))  
        
        d <- density(rbinom(input$s,input$n,input$p))  
        
        plot(d, main="Kernel Density of generated data")  
        
        polygon(d, col="blue", border="red") 
        
        x=0:input$max
        
        plot(x,dbinom(x,input$n,input$p),type = 'o')  
      
        
        
        
      } 
      
      
      
      
      
      # poisson 
      
      
      
      if (input$dismodel == 'poisson') { 
        
        par(mfrow=c(1,2))   
        
        D=rpois(input$s, input$lam)  
        
        tab=table(D)  
        
        barplot(tab,col='blue')  
        
        x1=0:input$max  
        
        y1=dpois(x1,input$lam)  
        
        plot(x1,y1,type='b')  
        
      } 
      
      ##  
      
      
      
      
      
      
      
      # geometric  
      
      if (input$dismodel == 'geometric') { 
        
        par(mfrow=c(1,2)) 
        
        D=rgeom(input$s, input$p)  
        
        tab=table(D)  
        
        barplot(tab,col='blue')  
        
        x2=0:input$max  
        
        y2=dgeom(x2,input$p)  
        
        plot(x2,y2,type='b')  
        
      } 
      
      
      
    })    
    
    
    
    output$tab <- renderTable({  
      
      
      
      p1=dbinom(input$j1,input$n, input$p)  
      
      p2=dpois(input$j2,input$lam)  
      
      p3=dgeom(input$j3,input$p)  
      
      
      
      
      
      c(p1,p2,p3) 
      
      
      
      
      
    })  
    
    
    
    
    
  }  
  
) 

