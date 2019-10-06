library(shiny)
library(ggplot2)

# object of the RC-class 'population' based on the package of lab5group8 that connects to the kolada-api
kolada <- population$new()

# list of all municipalities
list_municip <- kolada$municipalities()

# Define UI for app that plots the population over time of Swedish municipalities
ui <- fluidPage(
  
  # App title
  titlePanel("The population of Swedish Municipalities over time"),
  
  
  sidebarLayout(
    # Sidebar panel with two inputs: the year range slider and the municipality drop down bar
    sidebarPanel(
      # information
      helpText("Create a graph of the population growth"),
      
      # dropdown menu to choose a municipality
      selectInput(inputId = "municip_choice",
                  label = "Select the municipality to display",
                  choices = list_municip,
                  selected = list_municip[1]),
      
      # two-point slider to select the range in years
      sliderInput("range",
                  label = "Range of years:",
                  
                  min = 1970, max = 2018, value = c(1980, 2000))
    ),
    
    # Main panel for displaying outputs
    mainPanel(
      plotOutput("plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  dat <- reactive({
    temp <- as.data.frame(kolada$population_data(input$municip_choice)[(input$range[1]-1969):(input$range[2]-1969),])
    print(input$municip_choice)
    typeof(temp)
    print(temp)
    temp
  })
  
  output$plot <- renderPlot({
    ggplot(dat(), aes(x=period,y=pop))+geom_bar(stat="identity")},height = 400,width = 600)
  }

# Run the app
shinyApp(ui = ui, server = server)


