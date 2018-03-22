library(shiny)
library(mclust)
library(shinythemes)

# Preliminaries:
# Dimensions of iris -> may need to change later

# Iris function
data(thyroid)
data(iris)
data(diabetes)

names(iris) = gsub("\\.", " " , names(iris)) #fixing labels on axes

col_to_index <- function(col){
  if(col == "RT3U") 1
  else if(col == "T4")  2
  else if(col == "T3") 3
  else if(col == "TSH") 4
  else if(col == "DTSH") 5
  else if(col == "glucose")  1
  else if(col == "insulin") 2
  else if(col == "sspg") 3
  else if(col == "Sepal Length") 1
  else if(col == "Sepal Width")  2
  else if(col == "Petal Length") 3
  else 4
  
}

ui <- navbarPage("Mclust", fluid =TRUE,theme = shinytheme("flatly"),
                 
                 tabPanel("Clustering",
                          titlePanel("Tabsets"),
                          # Sidebar layout with input and output definitions 
                          sidebarLayout(
                            # Sidebar panel for inputs
                            sidebarPanel(
                              htmlOutput("dataset_selector"),
                              htmlOutput("xcol_selector"),
                              htmlOutput("ycol_selector"),
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
                 ),
                 tabPanel("Classification")
                 
)

server <- function(input, output, session) {
  # Defining data (no need to be reactive currently)
  output$dataset_selector = renderUI({ #creates State select box object called in ui

    selectInput(inputId = "dataset", #name of input
                label = "Dataset:", #label displayed in ui
                choices = c("Iris", "Thyroid", "Diabetes"),
                # calls unique values from the State column in the previously created table
                selected = c("Iris")) #default choice (not required)
  })
  
  datasetInput <- reactive({
    req(input$dataset)
    switch(input$dataset,
           "Iris" = iris[,1:4],
           "Thyroid" = thyroid[,2:6],
           "Diabetes" = diabetes[,2:4])
  })
  
  output$xcol_selector = renderUI({#creates County select box object called in ui
    if (is.null(input$dataset)) return()
    xdata_available = datasetInput()
    #creates a reactive list of available counties based on the State selection made
    
    selectInput(inputId = "xcol", #name of input
                label = "X variable:", #label displayed in ui
                choices = names(xdata_available), #calls list of available columns
                selected = unique(xdata_available)[1])
  })
  
  output$ycol_selector = renderUI({#creates County select box object called in ui
    if (is.null(input$dataset)) return()
    
    ydata_available = datasetInput()
    #creates a reactive list of available counties based on the State selection made
    
    selectInput(inputId = "ycol", #name of input
                label = "Y variable:", #label displayed in ui
                choices = names(ydata_available), #calls list of available columns
                selected = unique(ydata_available)[1])
  })
  
  
  # Defining model
  model <- reactive({
    mymod <- Mclust(datasetInput(), G = input$nclust[1]:input$nclust[2])
  })
  
  # The plot
  output$plot1 <- renderPlot({
    x = col_to_index(input$xcol)
    y = col_to_index(input$ycol)
    coordProj(data = datasetInput(), what = input$plotType, 
              parameters = model()$parameters, z = model()$z, 
              dimens = c(x, y), main = FALSE, 
              xaxt = "n", yaxt = "n")
  })
  # The summary
  output$summary <- renderPrint({
    summary(model())
  })
  

  
}

shinyApp(ui = ui, server = server)