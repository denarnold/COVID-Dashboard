##---------------------------------------------------------------
##                LOAD PACKAGES INTO LIBRARY                   --
##---------------------------------------------------------------

library(shiny)          #used to run the web environment

source("plot_objects.R")	#creates plot objects




##---------------------------------------------------------------
##                      Shiny Part 1: UI                       --
##---------------------------------------------------------------

ui <- fluidPage(
 
  ##                        Title and description
  ##...............................................................
  
  h3("COVID Dashboard"),
  
  p("Case data source: COVID-19 Data Repository by CSSE at Johns Hopkins University (populated daily, pulled live)", br(),
    "Population data source: US Census Bureau",
    style = "color: gray; font-style: italic"),
  
  p("Lines in the charts below are seven-day moving averages. Click on locations in the legend to show/hide their lines."),
  br(),
  
  
  
  
  ##                          Display plots
  ##...............................................................
  
  fluidRow(
    
    column(12,
      
      #wellPanel creates a panel with a slightly inset border and grey background
      wellPanel(plotlyOutput("allPlot")))
  ),
  
  
  fluidRow(
    
    column(6,
      wellPanel(plotlyOutput("radfordPlot"))),
    
    column(6,
      wellPanel(plotlyOutput("montgomeryPlot")))
    
  ),
  
  
  fluidRow(
    
    column(6,
      wellPanel(plotlyOutput("fairfaxPlot"))),
    
    column(6,
      wellPanel(plotlyOutput("durhamPlot")))
  ),
  
  
  fluidRow(
    
    column(6,
           wellPanel(plotlyOutput("salemPlot"))),
  ),
  
  
  
  
  ##                          Footer
  ##...............................................................
  
  p("Developed by Dennis Arnold",
    style = "color: gray; font-style: italic")
  
)




##---------------------------------------------------------------
##                  Shiny Part 2: Server Logic                 --
##---------------------------------------------------------------

server <- function(input, output, session) {
  
  #create outputs from plotly objects
  output$allPlot <- renderPlotly({allPlot})
  
  output$radfordPlot <- renderPlotly({radfordPlot})
  output$montgomeryPlot <- renderPlotly({montgomeryPlot})
  output$fairfaxPlot <- renderPlotly({fairfaxPlot})
  output$durhamPlot <- renderPlotly({durhamPlot})
  output$salemPlot <- renderPlotly({salemPlot})
 
  
  #stop the app when the session ends (browser window closes)
  session$onSessionEnded(function() {
    stopApp()
  })
   
}




##---------------------------------------------------------------
##                  Shiny Part 3: Run Shiny App                --
##---------------------------------------------------------------

shinyApp(ui, server)