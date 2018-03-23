#library(shiny)
#library(mclust)
# Preliminaries:

# Dimensions of iris -> may need to change later
#dimens = seq(4)
# Iris function
species_to_index <- function(species){
  if(is.null(species)) 2
  else if(species == "Sepal Length") 1
  else if(species == "Sepal Width")  2
  else if(species == "Petal Length") 3
  else 4
}
# Regex on colnames
names(iris) = gsub("\\.", " " , names(iris)) #fixing labels on axes

# Two dimensional density plot
density2d <- function(fit, data, x, y){
  G<-fit$G
  
  ind<-c(x,y)
  param<-list()
  param$pro<-fit$param$pro
  param$mean<-fit$param$mean[ind,]
  param$variance<-list()
  param$variance$modelName<-fit$param$variance$modelName
  param$variance$d<-2
  param$variance$G<-fit$param$variance$G
  
  param$variance$sigma<-array(NA,c(2,2,G))
  for (g in 1:G)
  {
    param$variance$sigma[,,g]<-fit$param$variance$sigma[ind,ind,g]
  }
  
  surfacePlot(data[,ind],parameters=param,type="contour",what="density")
  points(data[,ind],pch=fit$cl,cex=0.7,col=fit$cl)
}



# Main app:

# Define UI for random distribution app 
ui <- fluidPage(
  # App title 
  titlePanel("Mclust Demo"),
  # Sidebar layout with input and output definitions 
  sidebarLayout(
    # Sidebar panel for inputs
    sidebarPanel(
      selectInput('xcol', 'X Variable', names(iris)[1:4], selected=names(iris)[[1]]),
      uiOutput("inputY"),
      # Input: Slider for the number of observations to generate
      sliderInput("nclust", "Number of clusters:", 
                  value = c(1,9), min = 1, max = 9),
      # Input: Select the random distribution type 
      radioButtons("plotType", "Plot type", 
                   c("Classification"="classification", "BIC"="BIC", 
                     "Uncertainty" = "uncertainty", "Density" = "density"))
    ),
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("Plot", plotOutput("plot1")),
                  tabPanel("Summary", verbatimTextOutput("summary")))
    )
  )
)




server <- function(input, output) {
  # Avoiding having the same value in xcol and ycol
  observeEvent(input$xcol, {
    output$inputY = renderUI({
      selectInput('ycol', 'Y Variable', names(iris)[1:4][!names(iris)[1:4] == input$xcol], 
                  selected=names(iris)[[2]])
    })
  })
  
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
    if(input$plotType == "classification" || input$plotType == "uncertainty"){
      coordProj(data = data, what = input$plotType, 
                parameters = model()$parameters, z = model()$z, 
                dimens = dimens[c(x, y)], main = FALSE, 
                xaxt = "n", yaxt = "n")
    }
    else if(input$plotType == "density"){
      if(x != y){
        density2d(model(), data, x, y)
      }
    }
    else if(input$plotType =="BIC"){
      plot.mclustBIC(model()$BIC, G = input$nclust[1]:input$nclust[2])
    }
  })
  
  # The summary
  output$summary <- renderPrint({
    summary(model())
  })
}

shinyApp(ui = ui, server = server)

