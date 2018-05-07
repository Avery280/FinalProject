#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Dan Brown"),
  
  hr(),
  
  # Sidebar with a slider input for number of bins 
  sidebarPanel(
    # select a file 
    fileInput("file", label = h3("Source"), multiple = FALSE),
    
    # add a reset button 
    actionButton("reset", "Reset File"),
    
    # reset fileInput 
    tags$script('
                Shiny.addCustomMessageHandler("resetFileInputHandler", function(x) {      
                
                var id = "#" + x + "_progress";
                
                var idBar = id + " .bar";
                
                $(id).css("visibility", "hidden");
                
                $(idBar).css("width", "0%");
                
                });
                
                '),
    # parameters for each plot
    conditionalPanel(condition="input.conditionedPanels==2",
                     hr(),
                     h3("Parameters"),
                     helpText("Number of Most Frequent Words"),
                     sliderInput("n1", label = "", min = 1, max = 100, 
                                 value = 20, step = 1)),
    
    conditionalPanel(condition="input.conditionedPanels==3",
                     hr(),
                     h3("Parameters"),
                     helpText("Number of Most Frequent Words"),
                     sliderInput("n2", label = "", min = 1, max = 100, 
                                 value = 50, step = 1)),
    
    conditionalPanel(condition="input.conditionedPanels == 4 ||
                     input.conditionedPanels == 5||input.conditionedPanels == 6",
                     hr(),
                     h3("Parameters"),
                     helpText("Sparsity"),
                     sliderInput("sparsity", label = "", min = 0, max = 1, 
                                 value = 0.8, step = 0.01)),
    
    conditionalPanel(condition="input.conditionedPanels==4 || input.conditionedPanels==5",
                     hr(),
                     helpText("Number of clusters"),
                     sliderInput("k", label = "", min = 1, max = 10, 
                                 value = 5, step = 1))     
    
  ),
    
    # Show a plot of the generated distribution
    mainPanel(
      # show plots 
      tabsetPanel(
        tabPanel("Original Text", value = 1, verbatimTextOutput("value")),
        tabPanel("Frequency Table", dataTableOutput("table1")),
        tabPanel("Histogram", value = 2, plotOutput("plot1")), 
        tabPanel("Word Cloud", value = 3, plotOutput("plot2")), 
        tabPanel("Hierarchical Clustering", value = 4, plotOutput("plot3")),
        tabPanel("KMeans", value = 5, plotOutput("plot4")),
        tabPanel("Network", value = 6, plotOutput("plot5")),
        id = "conditionedPanels"
    )
  )
))
