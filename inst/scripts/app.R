#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(ConconiAnaerobicThresholdTest)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Conconi Anaerobic Threshold Calculator"),

    fileInput("file1", "Choose TCX File to be analyzed",
              accept = c("text/csv",
                         "text/tcx,
                       .tcx")),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("startminutes", "Start at minute:", value = 0),
            numericInput("endminutes", "End at minute:", value = 1000),
            numericInput("speedmin", "Starting speed", value = 6),
            numericInput("speedstep", "Speed step", value = 1),
            numericInput("timestep", "Time step", value = 1.5),
            checkboxInput(
              "alldata",
              "Use all data from TCX file? If not, only the last 5 heart rate measurements of each step are used",
              value = FALSE
            ),
            checkboxInput(
              "useDeviceSpeed",
              "Use speed data from TCX file? This overrides Starting speed and Speed step.",
              value = FALSE
            ),
            sliderInput("textsize", "Text size for model results", min = 1, max = 10, value = 5)
        ), 
        
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      if (is.null(input$file1)){
        return(NULL)
      }else{
        dat <-
          prepdata(
            fname = input$file1$datapath,
            startminutes = input$startminutes,
            endminutes = input$endminutes,
            speedmin = input$speedmin,
            speedstep = input$speedstep,
            timestep = input$timestep,
            useDeviceSpeed = input$useDeviceSpeed
          )
        fitmodel(
          dat,
          alldata = input$alldata,
          textsize = input$textsize
        )
      }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
