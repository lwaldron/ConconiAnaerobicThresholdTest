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
ui <- fluidPage(tags$html(
  tags$head(tags$title('Conconi Anaerobic Threshold Calculator')),
  tags$body(
    h1('Conconi Anaerobic Threshold Calculator'),
    h2('Instructions'),
    'This calculator estimates anaerobic threshold speed and heart rate from
        a treadmill stepwise exhaustion test, where speed is in constant steps
        regular time intervals. See',
    a(href = "https://waldronlab.io/ConconiAnaerobicThresholdTest/articles/usage.html",
      "instructions for performing the test"),
    " and an ",
    a(
      href = "https://github.com/waldronlab/ConconiAnaerobicThresholdTest/raw/d58d5e9980bf38f07b1d4bb3557ceaee5ff18ccd/inst/extdata/2023-01-16.tcx",
      download = "example",
      target = "_blank",
      "example TCX file"
    ),
    " (pre-loaded). In this example, speed started at 6km/h and was increased by 
    1km/h every 1.5 minutes for 10 steps (15 minutes total), on a treadmill with
    1% incline to compensate for lack of air resistance. For correct analysis of 
    this example dataset, ",
    strong("start"),
    " at minute 0.15, ",
    strong("end"),
    " at minute 15, set ",
    strong("starting speed"),
    " to 6, ",
    strong("speed step"),
    " to 1, and ",
    strong("time step"),
    " to = 1.5. Then try playing with the other settings! (except for option of
    using speed data from TCX file, because there is none in this file)"
  )),

# Application title
helpText(""),
fileInput("file1",
          "Choose TCX File to be analyzed",
          accept = c("text/csv",
                     "text/tcx,
                       .tcx")),

# Sidebar with a slider input for number of bins
sidebarLayout(
  sidebarPanel(
    helpText(
      "Adjust start and end minutes to remove warmup and cooldown from TCX file:"
    ),
    numericInput("startminutes", "Start at minute:", value = 0),
    numericInput("endminutes", "End at minute:", value = 1000),
    helpText(
      "Adjust starting speed and speed step if to match treadmill
                   Speed is printed in output as km/h, but if other units were
                   used, the output units will be the same as whatever you used."
    ),
    numericInput("speedmin", "Starting speed", value = 6),
    numericInput("speedstep", "Speed step", value = 1),
    helpText("Time in minutes of each step in the test:"),
    numericInput("timestep", "Time step", value = 1.5),
    helpText(
      "If selected, all HR data will be used. If not, only the last 5 HR measurements from each step are used:"
    ),
    checkboxInput("alldata",
                  "Use all data from TCX file?",
                  value = FALSE),
    helpText(
      "If selected, device speed overrides Starting speed and Speed step set above. Only appropriate e.g. for outdoor runs or cycling where speed is measured by GPS:"
    ),
    checkboxInput("useDeviceSpeed",
                  "Use speed data from TCX file?",
                  value = FALSE),
    sliderInput(
      "textsize",
      "Text size for model results",
      min = 1,
      max = 10,
      value = 8
    )
  ),
  
  # Show a plot of the generated distribution
  mainPanel(plotOutput("distPlot"))
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    if (is.null(input$file1)) {
      fname = "https://github.com/waldronlab/ConconiAnaerobicThresholdTest/raw/d58d5e9980bf38f07b1d4bb3557ceaee5ff18ccd/inst/extdata/2023-01-16.tcx"
    } else{
      input$file1$datapath
    }
    dat <-
      prepdata(
        fname = fname,
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
      textsize = input$textsize,
      title = "HR vs Speed, intersection of the two red lines is the Anaerobic Threshold"
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
