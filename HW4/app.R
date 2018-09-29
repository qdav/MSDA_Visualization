library(shiny)
library(dplyr)
library(ggplot2)
library(here)
library(MASS)
library(shinydashboard)

vecMeasures <- c('Sales' = 'SALEQ', 
                 'Cash' = 'CHEQ', 
                 'Assets' = 'ATQ', 
                 'Profit' = 'OIADPQ', 
                 'R&D' = 'XRDQ', 
                 'SG&A' = 'XSGAQ'
)

fileMissingMsg = 'Please upload a SAS data file (sas7bdat extension)'
fileFormatBad =  '\nMake sure that it has the following variables: \nSALEQ, CHEQ, ATQ, OIADPQ, XRDQ, XSGAQ'
varSameMsg = 'X and Y variables have to be different'

ui <- 
  dashboardPage(
    dashboardHeader(title = "Apple Financials"),
    dashboardSidebar(
      fileInput('fileIn', 
                'Upload SAS Data:', 
                multiple = FALSE,
                accept = c('application/x-sas-data')),
      selectInput('xVar', 
                  'X-Axis Variable:', 
                  vecMeasures, 
                  selected = 'SALEQ'),
      selectInput('yVar', 
                  'Y-Axis Variable:', 
                  vecMeasures,
                  selected = 'XRDQ'),
      selectInput('scaleType', 
                  'Choose the Scale:', 
                  c('Levels', 
                    'Log10'), 
                  selected = 'Levels'), 
      radioButtons('modelType', 
                   'Choose the Model:', 
                   c('Linear Model' = 'lm', 
                     'Loess' = 'loess', 
                     'Robust Linear' = 'rlm', 
                     'None' = 'none'
                     ), 
                   selected = 'loess'
                  ),
 
     checkboxInput('stdErrorRibbon', 'Standard Error Ribbon', value=TRUE), 
     conditionalPanel( condition = "input.modelType == 'loess'",
                       sliderInput('spanLoess', 'Span for LOESS', min=0, max=1, value=.75))
     
    ),
    
    dashboardBody(
      
      plotOutput('applePlot')
    )
)





server <- function(input, output) {

  
  output$applePlot <- renderPlot({

    inFile <- input$fileIn      
     
    # file exists
    validate(
      need(inFile != '', paste(fileMissingMsg, fileFormatBad))
    )
    
    # file is SAS format
    validate(
      need(grepl('.sas7bdat', inFile, ignore.case = TRUE), paste('File Format Error: ', fileMissingMsg))
    )
    
    # measures are not the same
    validate(
      need(input$xVar != input$yVar, varSameMsg)
    )
    
    apple <- haven::read_sas(inFile$datapath )
      
    appleMeasures <- 
      dplyr::select(apple, SALEQ, CHEQ, ATQ, OIADPQ, XRDQ, XSGAQ)  

    
    # build layers common to all options
    p <- ggplot(appleMeasures, aes_string(x = input$xVar, y = input$yVar)) +
      geom_point (na.rm = TRUE) + 
      labs(x = paste(names(vecMeasures)[vecMeasures == input$xVar], '(million $)'), 
           y = paste(names(vecMeasures)[vecMeasures == input$yVar], '(million $)')) +
      theme_bw() 
    
    # add selected line to base plot  
    if (input$modelType != 'none') {
      p <- p + stat_smooth(method=input$modelType,
                    formula = y ~ x, 
                    span = input$spanLoess, 
                    se = input$stdErrorRibbon,
                    na.rm = TRUE)    
    }

    # scale axis if log10 selected
    if (input$scaleType == 'Log10') 
      p <- p + scale_x_log10()
    
    
    print(p)
     
  })

  
}

shinyApp(ui = ui, server = server)
