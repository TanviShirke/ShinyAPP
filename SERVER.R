function(input, output, session) {
  observe({
    dataset1 <- dataset
    # Encoding categorical data
    dataset1$State = factor(dataset1$State,
                            levels = c('New York', 'California', 'Florida'),
                            labels = c(1, 2, 3))
    
    nrows = nrow(dataset1) 
    
    indexes = sample(nrows, nrows*(input$ratio/100))
    
    trainset = dataset1[indexes,]
    
    testset = dataset1[-indexes,]
    
    regressor = lm(formula = Profit ~ ., data = trainset)
    
    profitPred = predict(regressor, newdata = testset)
    
    residual <- residuals(regressor)
    
    output$distPlot <- renderPlot({
      
      choice <- switch (input$selectInput,
                        "R&D Spend" = dataset$R.D.Spend,
                        "Administration Spend" = dataset$Administration,
                        "Marketing Spend" = dataset$Marketing.Spend,
                        "Profit" = dataset$Profit
      )
      bins <- seq(min(choice), max(choice), length.out = input$bins + 1)
      
      hist(choice, breaks = bins, col = "#75AADB", border = "white",
           xlab = input$selectInput,
           prob = TRUE,
           main = "Histogram of profits of 50 startups")
      lines(density(choice), lwd =3, col="darkblue")
    })
    
    output$dataTable <- DT::renderDataTable(
      dataset,
      options = list(pageLength = 10, autoWidth = TRUE),
      filter = "top"
    )
    
    output$barPlot <- renderPlot({
      stateName = unique(c(as.character(dataset$State)))
      
      state1 = dataset[which(dataset$State == 'New York'),]
      state2 = dataset[which(dataset$State == 'California'),]
      state3 = dataset[which(dataset$State == 'Florida'),]
      
      profitSum = c(sum(state1$Profit),sum(state2$Profit),sum(state3$Profit))
      
      
      barplot(
        height = profitSum,
        main = "Profit Split in State Level",
        xlab = "States",
        ylab = "Profit",
        names = stateName,
        col = "#75AADB",
        ylim = range(pretty(c(0, profitSum))),
        cex.lab=1.5, cex.main=1.5, cex.sub=1.5
      )
    })
    
    output$rdPlot <- renderPlot({
      plot(dataset$R.D.Spend,dataset$Profit, main = "Profit Vs R&D Spend", xlab = "R&D Spend",ylab = "Profit", sub=paste("Correlation Between Profit and R&D Spend:", round(cor(dataset$R.D.Spend,dataset$Profit), 2)),pch = 16,col = "#75AADB",cex.lab=1.5, cex.main=1.5)
      abline(line(dataset$R.D.Spend,dataset$Profit), col = "red")
    })
    
    output$adPlot <- renderPlot({
      plot(dataset$Administration,dataset$Profit, main = "Profit Vs Administration Spend", xlab = "Administration Spend",ylab = "Profit",sub=paste("Correlation Between Profit and Administration Spend:", round(cor(dataset$Administration,dataset$Profit), 2)),pch = 16,col = "#75AADB",cex.lab=1.5, cex.main=1.5)
      abline(line(dataset$Administration,dataset$Profit), col = "red")
    })
    
    output$mtPlot <- renderPlot({
      options(scipen = 999)
      plot(dataset$Marketing.Spend,dataset$Profit, main = "Profit Vs Marketing Spend", xlab = "Marketing Spend",ylab = "Profit",sub=paste("Correlation Between Profit and Marketing Spend:", round(cor(dataset$Marketing.Spend,dataset$Profit), 2)),pch = 16,col = "#75AADB",cex.lab=1.5, cex.main=1.5)
      abline(line(dataset$Marketing.Spend,dataset$Profit), col = "red")
    })
    
    output$profitDist <- renderPlot({
      options(scipen = 999)
      plot(density(dataset$Profit), main="Density Plot: Profit", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(dataset$Profit), 2)))  # density plot for 'Profit'
      polygon(density(dataset$Profit), col="#75AADB")
    })
    
    output$rdDist <- renderPlot({
      options(scipen = 999)
      plot(density(dataset$R.D.Spend), main="Density Plot: R&D Spend", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(dataset$R.D.Spend), 2)))  # density plot for 'Profit'
      polygon(density(dataset$R.D.Spend), col="#75AADB")
    })
    
    output$adDist <- renderPlot({
      options(scipen = 999)
      plot(density(dataset$Administration), main="Density Plot: Administration Spend", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(dataset$Administration), 2)))  # density plot for 'Profit'
      polygon(density(dataset$Administration), col="#75AADB")
    })
    
    output$mtDist <- renderPlot({
      options(scipen = 999)
      plot(density(dataset$Marketing.Spend), main="Density Plot: Marketing Spend", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(dataset$Marketing.Spend), 2)))  # density plot for 'Profit'
      polygon(density(dataset$Marketing.Spend), col="#75AADB")
    })
    
    output$residualPlot <- renderPlot({
      # COLOR UNDER/OVER
      # Color mapped to residual with sign taken into account.
      # i.e., whether actual value is greater or less than predicted
      ggplot(trainset, aes(x = trainset$R.D.Spend, y = trainset$Profit)) +
        xlab("R&D Spend")+
        ylab("Profit")+
        geom_smooth(method = "lm", se = FALSE, color = "lightgrey") +
        geom_segment(aes(xend = trainset$R.D.Spend, yend = predict(regressor,newdata = trainset)), alpha = .2) +
        
        # > Color adjustments made here...
        geom_point(aes(color = residual),size = 4) +  # Color mapped here
        scale_color_gradient2(low = "blue", mid = "white", high = "red") +  # Colors to use here
        guides(color = FALSE) +
        # <
        
        geom_point(aes(y = predict(regressor,newdata = trainset)), shape = 1,size = 3) +
        theme_bw()+
        ggtitle("Residual Plot")+
        theme(axis.title=element_text(size=16,face="bold"),
              plot.title = element_text(size=30,face = "bold",hjust = 0.5))
    })
    
    output$lmSummary <- renderPrint({
      summary(regressor)
    })
    
    
    
  })
  
  
  output$svmSummary <- renderPrint({
    summary(svmFit())
  })
  
  svmFit <- reactive({
    model <- switch (input$svmTypeSelect,
                     "nu-regression" = svm(formula = Profit ~.,
                                           data = dummTest_data,
                                           type = input$svmTypeSelect, 
                                           kernel = input$svmKernelSelect,
                                           cost = input$svmCost,
                                           epsilon = input$svmEspilon,
                                           nu = input$svmNuSelect,
                                           scale = input$svmScaleCB),
                     "eps-regression" = svm(formula = Profit ~.,
                                            data = dummTest_data,
                                            type = input$svmTypeSelect,
                                            cost = input$svmCost,
                                            epsilon = input$svmEspilon,
                                            kernel = input$svmKernelSelect,
                                            scale = input$svmScaleCB
                     )
    )
    model
  })
  
  
  output$actualVsPredictedPlot <- renderPlot({
    req(svmFit())
    
    predictedY <- predict(svmFit(), dummTest_data)
    
    plot(dummTest_data$Profit, predictedY, col = "red", pch=4)
  })
  
  output$actualVSPredictedTable <- renderTable({
    req(svmFit())
    
    data.frame(predicted = predict(svmFit(), dummTest_data), actual = dummTest_data$Profit, error = dummTest_data$Profit - predict(svmFit(), dummTest_data))
    
  })
  
  output$rmseErr <- renderText({
    req(svmFit())
    
    predictedY <- predict(svmFit(), dummTest_data)
    
    error <- dummTest_data$Profit - predictedY
    paste0("The RMSE error is ", rmse(error))
    
  }) 
  
  svmTuning <- reactive({
    # perform a grid search
    tune(svm,
         Profit ~ .,  
         data = dummTrain_data,
         ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9))
    )
  })
  
  output$svmTuningSummary <- renderPrint({
    svmTuning()
  })
  
  output$svmTuningPlot <- renderPlot({
    plot(svmTuning())
  })
  
  treeModel <- reactive({
    rpart(
      formula = Profit ~ .,
      data    = train_data,
      method  = "anova",
      control = list(
        minsplit = input$treeMinSplit, 
        maxdepth = input$treeMaxDepth, 
        xval = input$treeNbCV,
        cp = input$treeCP
      )
    )
  })
  
  output$treeModelSummary <- renderPrint({
    treeModel()
  })
  
  output$treeModelPlot <- renderPlot({
    rpart.plot(treeModel())
  })
  
  output$treeModelSizePlot <- renderPlot({
    plotcp(treeModel())
    
  })
  
  output$treeActualVsPredictedPlot <- renderPlot({
    req(treeModel())
    
    predictedY <- predict(treeModel(), test_data)
    
    plot(test_data$Profit, predictedY, col = "red", pch=4)
  })
  
  output$treeActualVSPredictedTable <- renderTable({
    req(svmFit())
    
    data.frame(predicted = predict(treeModel(), test_data), actual = test_data$Profit, error = test_data$Profit - predict(treeModel(), test_data))
    
  })
  
  output$treeRmseErr <- renderText({
    req(svmFit())
    
    predictedY <- predict(treeModel(), test_data)
    
    error <- test_data$Profit - predictedY
    paste0("The RMSE error is ", rmse(error))
    
  }) 
  


  output$Discrete <- renderPlot({
    
    # Poisson 
    
    if (input$dismodel == 'poisson') { 
      par(mfrow=c(1,2))   
      D=rpois(input$s, input$lam) 
      tab=table(D)  
      barplot(tab,col='blue') 
      x1=0:input$max  
      y1=dpois(x1,input$lam)  
      plot(x1,y1,type='b')  
      
    } 
    
    # Geometric  
    
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
    p1=dpois(input$j1,input$lam)  
    p2=dgeom(input$j2,input$p)  
    c(p1,p2) 
    
  })  

  
  output$Continuous <- renderPlot({ ''
    
    # Normal  
    
    if (input$conmodel == 'normal') { 
      par(mfrow=c(1,2))  
      x=seq(-input$c,input$c,0.01)  
      plot(x,dnorm(x,input$mu,input$sigma),type='l', col='red') 
    } 
    
    # Exponential Model
    
    if (input$conmodel == 'exponential') { 
      par(mfrow=c(1,2)) 
      x=seq(0,input$c,0.01)
      plot(x,dexp(x,input$lam),type='l',col='green') 
    } 
    
    # Unifrom Distribution Model
    
    if (input$conmodel == 'uniform') { 
      
      a <- input$a 
      b <- input$b 
      n1 <- input$c 
      
      rand.unif <- runif(n1, min = a, max = b) 
      
      hist(rand.unif,  
           freq = FALSE,
           xlab = 'x',   
           ylim = c(0, 0.8),
           xlim = c(-5,5), 
           density = 20, 
           main = "Uniform distribution") 
      
      curve(dunif(x, min = a, max = b),  
            
            from = -5, to = 5,  
            n = n1,  
            col = "darkblue",  
            lwd = 2,  
            add = TRUE,  
            yaxt = "n", 
            ylab = 'probability') 
    }
    
    
  })    
}
