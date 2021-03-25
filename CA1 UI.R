library(shiny)
library(DT)
library(shinythemes)
library(shinydashboard) 
fluidPage(
  titlePanel("50 Startups"),
  theme = shinytheme("flatly"),
  navbarPage
  (
    "",
    tabPanel
    (
      "Descriptive Analysis",
      tabsetPanel
      (
        tabPanel
        (
          "DataTable",
          DT::dataTableOutput
          (
            "dataTable"
          )
        ),
        tabPanel
        (
          "Exploratory Data Analysis",
          sidebarLayout
          (
            sidebarPanel
            (
              selectInput(inputId = "selectInput",
                          label = "Choose a variable to display",
                          choices = c("R&D Spend","Administration Spend","Marketing Spend","Profit"),
                          selected = "Profit"
              ),
              sliderInput
              (
                inputId = "bins",
                label = "Number of bins:",
                min = 5,
                max = 20,
                value = 5
              )
            ),
            mainPanel
            (
              plotOutput(outputId = "distPlot")
            )
          )
        ),
        tabPanel
        (
          "Correlation Analysis",
          fluidRow(
            column(
              6,
              style='border: 2px solid #2C3E50',
              plotOutput(outputId = "barPlot")
            ),
            column(
              6,
              style='border: 2px solid #2C3E50',
              plotOutput(outputId = "rdPlot")
            )
          ),
          fluidRow(
            column(
              6,
              style='border: 2px solid #2C3E50',
              plotOutput(outputId = "adPlot")
            ),
            column(
              6,
              style='border: 2px solid #2C3E50',
              plotOutput(outputId = "mtPlot")
            )
          )
        ),
        tabPanel(
          "Density Plots",
          fluidRow(
            column(3,
                   plotOutput(outputId = "profitDist")),
            column(3,
                   plotOutput(outputId = "rdDist")),
            column(3,
                   plotOutput(outputId = "adDist")),
            column(3,
                   plotOutput(outputId = "mtDist"))
          )
        )
      )
    ),
    tabPanel( "Probability Models",
              tabsetPanel
              (
                tabPanel
                ("Discrete Model",
                    sidebarPanel( width = 4,
                              sidebarMenu( 
                                selectInput("dismodel", "select Model",
                                            choices = c("Poisson" = "poisson", 
                                                        "Geometric" = "geometric"), 
                                            selected = "poisson"),
                                numericInput("max", "upper limit for x" , value = 5),  
                                sliderInput("s", "number of simulated data" ,min=1, max=1000, value = 10),
                                conditionalPanel(     
                                  condition = "input.dismodel == 'poisson'", 
                                  numericInput("lam", "parameter lambda in Poisson" , value = 1)
                                ), 
                                conditionalPanel(     
                                  condition = "input.dismodel == 'geometric'", 
                                  numericInput("p", "parameter p in Geometric" , value = 0.5) 
                                ),
                                conditionalPanel( 
                                  condition = "input.dismodel == 'poisson'", 
                                  numericInput("j1", "j for Poisson" , value = 1) 
                                ),
                                conditionalPanel( 
                                  condition = "input.dismodel == 'geometric'", 
                                  numericInput("j2", "j for geometric" , value = 1) 
                                )
                                )), 
                mainPanel(tabsetPanel(
                  tabPanel("Plot", status = "warning", solidHeader = T, mainPanel(  
                    plotOutput("Discrete")
                  )),
                  tabPanel("Table",style ="font-weight:bold; color:gold",mainPanel(
                    tableOutput('tab'))
                  ))
                )
              ),
                tabPanel("Continuous Model", 
                sidebarPanel(width = 4, 
                             sidebarMenu(
                               selectInput("conmodel", "Select Model", 
                                           choices = c("Normal" = "normal", 
                                                       "Exponential" = "exponential", 
                                                       "Uniform" = "uniform"), 
                                           selected = "normal"),
                               numericInput("max", "upper limit for x" , value = 5), 
                               sliderInput("c", "number of simulated data" ,min=1, max=1000, value = 10), 
                               conditionalPanel(     
                                 condition = "input.conmodel == 'exponential'", 
                                 numericInput("lam", "parameter lambda in exponential" , value = 1)
                               ), 
                               conditionalPanel( 
                                 condition = "input.conmodel == 'normal'", 
                                 numericInput("mu", "parameter mu in Normal" , value = 0),
                                 numericInput("sigma", "parameter sigma in Normal" , value = 1) 
                               ),   
                               conditionalPanel(     
                                 condition = "input.conmodel == 'normal'", 
                                 numericInput("j1", "j in Normal" , value = 0) 
                               ), 
                               conditionalPanel(     
                                 condition = "input.conmodel == 'exponential'",  
                                 numericInput("j2", "j in exponential" , value = 0) 
                               ), 
                               conditionalPanel( 
                                 condition = "input.conmodel == 'uniform'", 
                                 numericInput("a", "parameter a in Normal" , value = -2),  
                                 numericInput("b", "parameter b in Normal" , value = 0.8) 
                               ))),
                mainPanel(tabsetPanel(
                  tabPanel("Plot",status = "warning", solidHeader = T, mainPanel(
                    plotOutput("Continuous")
                  )))
                ))
              )
    ),
    tabPanel
    ("Machine Learning",
      tabsetPanel(
        tabPanel(
          "Linear Regression - Residual Analysis",
          fluidRow(
            column(12,
                   sidebarLayout(
                     sidebarPanel(
                       sliderInput
                       (
                         inputId = "ratio",
                         label = "Ratio for Training Set:",
                         min = 20,
                         max = 100,
                         value = 80
                       )
                     ),
                     mainPanel(
                       plotOutput(outputId = "residualPlot")
                     )
                   )
            )
          )
        ),
        tabPanel(
          "Linear Model Summary",
          sidebarLayout(
            sidebarPanel(
              textInput(inputId = "lmCaption",
                        label = "Caption:",
                        value = "Model Summary")
            ),
            mainPanel(
              verbatimTextOutput("lmSummary")
            )
          )
        ),
        tabPanel(
          "Support Vector Regression",
          sidebarLayout(
            sidebarPanel(
              h4("SVM Options"),
              checkboxInput(
                "svmScaleCB",
                label = "Scale",
                value = TRUE
              ),
              selectInput(
                "svmTypeSelect",
                label = "Type",
                choices = c("eps-regression", "nu-regression")
              ),
              selectInput(
                "svmKernelSelect",
                label = "Kernel",
                choices = c("linear", "polynomial", "radial", "sigmoid")
              ),
              numericInput(
                "svmEspilon",
                label = "Epsilon",
                value = 0.1,
                min = 0.1,
                max = 1
              ),
              numericInput(
                "svmCost",
                label = "Cost",
                value = 1,
                min = 1
              ),
              conditionalPanel(
                condition = "input.svmTypeSelect == 'nu-classification' || input.svmTypeSelect == 'nu-regression' || input.svmTypeSelect == 'one-classification'",
                numericInput(
                  "svmNuSelect",
                  label = "Nu",
                  value = 0.1,
                  min = 0.1,
                  max = 0.99
                )
              )
              
            ),
            mainPanel(
              tabsetPanel(
                tabPanel("Summary",
                         verbatimTextOutput("svmSummary")
                ),
                tabPanel("Predicted Values",
                         fluidRow(
                           column(
                             width = 6,
                             align = "center",
                             
                             textOutput("rmseErr"),
                             tableOutput("actualVSPredictedTable")
                           ),
                           column(
                             width = 6,
                             
                             align = "center",
                             plotOutput("actualVsPredictedPlot"),
                             h4("Actual Vs Predicted Values")
                           )
                         )
                ),
                tabPanel("Tuning",
                         
                         fluidRow(
                           column(
                             width = 6,
                             align = "center",
                             
                             withSpinner(verbatimTextOutput("svmTuningSummary"))
                           ),
                           column(
                             width = 6,
                             
                             align = "center",
                             withSpinner(plotOutput("svmTuningPlot"))
                           )
                         )
                         
                )
              )
              
              
            )
          )
        ),
        tabPanel(
          "Decision Tree",
          
          sidebarLayout(
            sidebarPanel(
              "Options",
              
              numericInput(
                "treeMinSplit",
                label = "Min split",
                value = 20,
                min = 5
              ),
              numericInput(
                "treeMaxDepth",
                label = "Max depth",
                value = 30,
                min = 5
              ),
              numericInput(
                "treeNbCV",
                label = "Number of Cross-Validations",
                value = 10,
                min = 5
              ),
              numericInput(
                "treeCP",
                label = "Complexity parameter",
                value = 0.01,
                min = 0.01
              )
              
            ),
            
            mainPanel(
              tabsetPanel(
                tabPanel(
                  "Summary",
                  
                  verbatimTextOutput("treeModelSummary")
                  
                ),
                tabPanel(
                  "Chart",
                  
                  h4("Tree model", align  = "center"),
                  withSpinner(plotOutput("treeModelPlot")),
                  
                  
                  br(),
                  
                  h4("Size of tree", align  = "center"),
                  withSpinner(plotOutput("treeModelSizePlot"))
                ),
                tabPanel(
                  "Predicted Values",
                  fluidRow(
                    column(
                      width = 6,
                      align = "center",
                      
                      textOutput("treeRmseErr"),
                      tableOutput("treeActualVSPredictedTable")
                    ),
                    column(
                      width = 6,
                      
                      align = "center",
                      plotOutput("treeActualVsPredictedPlot"),
                      h4("Actual Vs Predicted Values")
                    )
                  )
                )
              )
              
              
              
            )
          )
        )
      )
    )
  )
)
