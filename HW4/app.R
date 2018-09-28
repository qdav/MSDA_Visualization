library(shiny)
library(dplyr)
library(ggplot2)
library(MASS)
library(shinydashboard)


ui <- 
  dashboardPage(
    dashboardHeader(title = "Apple Financials"),
    dashboardSidebar(
      fileInput('fileIn', 'Upload SAS Data'),
      selectInput('xVar', 
                  'X-Axis Variable', 
                  c('Total Sales' = 'SALEQ', 
                      'Cash' = 'CHEQ', 
                      'Assets' = 'ATQ', 
                      'Profit' = 'OIADPQ', 
                      'R&D' = 'XRDQ', 
                      'SG&A' = 'XSGAQ'
                    ), 
                  selected = 'SALEQ'),
      selectInput('yVar', 
                  'Y-Axis Variable', 
                  c('Total Sales' = 'SALEQ', 
                    'Cash' = 'CHEQ', 
                    'Assets' = 'ATQ', 
                    'Profit' = 'OIADPQ', 
                    'R&D' = 'XRDQ', 
                    'SG&A' = 'XSGAQ'
                    ),
                  selected = 'XRDQ'),
      selectInput('scaleType', 
                  'Choose the Scale', 
                  c('Levels', 
                    'Log10')), 
      radioButtons('modelType', 
                   'Choose the Model', 
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
    
    dashboardBody(plotOutput('applePlot')
  )
)



server <- function(input, output) {

  #appleSum <- data.frame()
  apple <- haven::read_sas(
    here::here('Data', 'aapl.sas7bdat')
  )
  
  appleMeasures <- 
      dplyr::select(apple, SALEQ, CHEQ, ATQ, OIADPQ, XRDQ, XSGAQ)  



  output$applePlot <- renderPlot(
    if (input$modelType != 'none') {
      ggplot(appleMeasures, aes_string(x = input$xVar, y = input$yVar)) +
        geom_point (na.rm = TRUE) + 
        stat_smooth(method=input$modelType,
                    formula = y ~ x, 
                    span = input$spanLoess, 
                    se = input$stdErrorRibbon,
                    na.rm = TRUE) +  
        theme_bw()        
    }
    else { 
      ggplot(appleMeasures, aes_string(x = input$xVar, y = input$yVar)) +
        geom_point (na.rm = TRUE) + 
        theme_bw()        
    }
    
    


      
      #if (input$modelType != 'none')
      #  p + stat_smooth(method=input$modelType,formula = y ~ x, span = input$spanLoess, se = stdErrorRibbon) + 

     
              
  )
}

shinyApp(ui = ui, server = server)
