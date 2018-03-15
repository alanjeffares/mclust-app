# Preliminaries:
# Dimensions of iris -> may need to change later
dimens = seq(4)
# Iris function
species_to_index <- function(species){
  if(species == "Sepal Length") 1
  else if(species == "Sepal Width")  2
  else if(species == "Petal Length") 3
  else 4
}
# Regex on colnames
names(iris) = gsub("\\.", " " , names(iris)) #fixing labels on axes


# Define UI for random distribution app 
ui <- fluidPage(
# App title ----
  titlePanel("Tabsets"),
# Sidebar layout with input and output definitions 
  sidebarLayout(
# Sidebar panel for inputs
    sidebarPanel(
      selectInput('xcol', 'X Variable', names(iris)[1:4]),
      selectInput('ycol', 'Y Variable', names(iris)[1:4], 
                  selected=names(iris)[[2]]),
# Input: Slider for the number of observations to generate
      sliderInput("nclust", "Number of clusters:", 
                  value = c(1,9), min = 1, max = 9),
# Input: Select the random distribution type 
      radioButtons("plotType", "Plot type", 
                   c("Classification"="classification", "BIC"="BIC", 
                     "Uncertainty" = "uncertainty", "Density"="density"))
),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Plot", plotOutput("plot1")),
                  tabPanel("Summary", verbatimTextOutput("summary")))
    )
  )
)




server <- function(input, output) {
  # Defining data (no need to be reactive currently)
  data = iris[, 1:4]
  # Defining model
  model <- reactive({
    mymod <- Mclust(data, G = input$nclust[1]:input$nclust[2])
  })
  
  # The plot
  output$plot1 <- renderPlot({
    x = species_to_index(input$xcol)
    y = species_to_index(input$ycol)
    coordProj(data = data, what = "classification", 
              parameters = model()$parameters, z = model()$z, 
              dimens = dimens[c(x, y)], main = FALSE, 
              xaxt = "n", yaxt = "n")
  })
  # The summary
  output$summary <- renderPrint({
    summary(model())
  })
}

shinyApp(ui = ui, server = server)
