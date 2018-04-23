library(shiny)
library(mclust)
library(e1071)

data(thyroid)
data(iris)
data(diabetes)
data(banknote)

names(iris) = gsub("\\.", " " , names(iris))
names(diabetes) = c("Class" , "Glucose", "Insulin" , "SSPG")

summarymclust <- function(object) {
  cat("Gaussian finite mixture model fitted by EM algorithm.\n\nMclust has clustered the", object$n , "observations of the dataset into" , object$G , "clusters(ie.", object$G, 
      "mixture components) where the model chosen was",object$modelName,"with a BIC score of" , 
      object$bic, "\n\n")
  cat("Clustering tabe:" )
  print(table(factor(object$classification, levels = {
    l <- seq_len(object$G)
    if (is.numeric(object$noise)) 
      l <- c(l, 0)
    l
  })))
  
}

summarymclustlab <- function(object, data) {
  cat("Gaussian finite mixture model fitted by EM algorithm.\n\nMclust has clustered the", object$n , "observations of the dataset into" , object$G , "clusters(ie.", object$G, 
      "mixture components) where the model chosen was",object$modelName,"with a BIC score of" , 
      object$bic, "\n\n")
  cat("Clustering tabe:" )
  print(table(factor(object$classification, levels = {
    l <- seq_len(object$G)
    if (is.numeric(object$noise)) 
      l <- c(l, 0)
    l
  })))
  cat("Confusion Matrix:")
  print(table(object$classification, data[,1]))
  a <- classAgreement(table(object$classification, data[,1]))
  cat("with a rand index of:", a$rand, "and an adjusted rand index of:", a$crand)
  
}

demcol_to_ind<- function(coln, col) {
  
  which(coln == col)
}

upcol_to_ind<- function(coln1, col) {
  
  which(coln1 == col)
}

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



ui <- navbarPage("Mclust App", fluid =TRUE,
                 navbarMenu("Clustering", 
                            tabPanel("Demo",
                                     
                                     
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
                            
                            tabPanel("Upload",
                                     
                                     # Sidebar layout with input and output definitions 
                                     sidebarLayout(
                                       # Sidebar panel for inputs
                                       sidebarPanel(
                                         
                                         # Input: Select a file ----
                                         fileInput("file1", "Choose CSV File",
                                                   multiple = TRUE,
                                                   accept = c("text/csv",
                                                              "text/comma-separated-values,text/plain",
                                                              ".csv")),
                                         
                                         # Horizontal line ----
                                         tags$hr(),
                                         
                                         # Input: Checkbox if file has header ----
                                         checkboxInput("header", "Header", TRUE),
                                         
                                         checkboxInput("labels", "Class Labels as 1st Column", FALSE),
                                         
                                         # Input: Select separator ----
                                         radioButtons("sep", "Separator",
                                                      choices = c(Comma = ",",
                                                                  Semicolon = ";",
                                                                  Tab = "\t"),
                                                      selected = ","),
                                         htmlOutput("xcol_selector1"),
                                         htmlOutput("ycol_selector1"),
                                         
                                         # Input: Slider for the number of observations to generate
                                         sliderInput("nclust1", "Number of clusters:", 
                                                     value = c(1,9), min = 1, max = 9),
                                         # Input: Select the random distribution type 
                                         radioButtons("plotType1", "Plot type", 
                                                      c("Classification"="classification", "BIC"="BIC", 
                                                        "Uncertainty" = "uncertainty", "Density"="density"))
                                       ),
                                       mainPanel(
                                         tabsetPanel(type = "tabs", 
                                                     tabPanel("Data", tableOutput("contents")),
                                                     tabPanel("Plot", plotOutput("plot2")),
                                                     tabPanel("Summary", verbatimTextOutput("summary1")))
                                       )
                                     )
                            )
                 ),
                 navbarMenu("Classification",
                            tabPanel("Demo",
                                     
                                     
                                     # Sidebar layout with input and output definitions 
                                     sidebarLayout(
                                       # Sidebar panel for inputs
                                       sidebarPanel(
                                         htmlOutput("dataset_selector3"),
                                         htmlOutput("xcol_selector3"),
                                         htmlOutput("ycol_selector3"),
                                         
                                         # Input: Select the random distribution type 
                                         radioButtons("plotType3", "Plot type", 
                                                      c("Scatterplot"="scatterplot", "Classification"="classification", 
                                                        "Error" = "error"))
                                       ),
                                       mainPanel(
                                         tabsetPanel(type = "tabs", 
                                                     tabPanel("Plot", plotOutput("plot3")),
                                                     tabPanel("Summary", verbatimTextOutput("summary3")))
                                       )
                                     )
                            ),
                            
                            tabPanel("Upload",
                                     
                                     # Sidebar layout with input and output definitions 
                                     sidebarLayout(
                                       # Sidebar panel for inputs
                                       sidebarPanel(
                                         
                                         # Input: Select a file ----
                                         fileInput("file4", "Choose Train CSV File",
                                                   multiple = TRUE,
                                                   accept = c("text/csv",
                                                              "text/comma-separated-values,text/plain",
                                                              ".csv")),
                                         
                                         # Horizontal line ----
                                         tags$hr(),
                                         
                                         # Input: Select a file ----
                                         fileInput("file5", "Choose Test CSV File (Optional)",
                                                   multiple = TRUE,
                                                   accept = c("text/csv",
                                                              "text/comma-separated-values,text/plain",
                                                              ".csv")),
                                         
                                         # Input: Checkbox if file has header ----
                                         checkboxInput("header", "Header", TRUE),
                                         
                                         # Input: Select separator ----
                                         radioButtons("sep", "Separator",
                                                      choices = c(Comma = ",",
                                                                  Semicolon = ";",
                                                                  Tab = "\t"),
                                                      selected = ","),
                                         htmlOutput("xcol_selector4"),
                                         htmlOutput("ycol_selector4"),
                                         
                                         # Input: Select the random distribution type 
                                         radioButtons("plotType4", "Plot type", 
                                                      c("Scatterplot"="scatterplot", "Classification"="classification", 
                                                        "Error" = "error", "Train/Test"="train&test"))
                                       ),
                                       mainPanel(
                                         tabsetPanel(type = "tabs", 
                                                     tabPanel("Data", tableOutput("contents4")),
                                                     tabPanel("Plot", plotOutput("plot4")),
                                                     tabPanel("Summary", verbatimTextOutput("summary4")))
                                       )
                                     )
                            )

                            
                 ),
                 tabPanel("About",
                            includeText("mclustabout.txt"))
                 
)

server <- function(input, output, session) {
  
  
  output$dataset_selector = renderUI({ #creates State select box object called in ui
    
    selectInput(inputId = "dataset", #name of input
                label = "Dataset:", #label displayed in ui
                choices = c("Iris", "Thyroid", "Diabetes", "Banknote"),
                # calls unique values from the State column in the previously created table
                selected = c("Iris")) #default choice (not required)
  })
  
  datasetInput <- reactive({
    req(input$dataset)
    switch(input$dataset,
           "Iris" = iris[,1:4],
           "Thyroid" = thyroid[,2:6],
           "Diabetes" = diabetes[,2:4],
           "Banknote" = banknote[,2:7])
  })
  
  output$xcol_selector = renderUI({#creates County select box object called in ui
    if (is.null(input$dataset)) return()
    xdata_available = datasetInput()
    #creates a reactive list of available counties based on the State selection made
    
    selectInput(inputId = "xcol", #name of input
                label = "X variable:", #label displayed in ui
                choices = names(xdata_available), #calls list of available columns
                selected = unique(names(xdata_available))[1])
  })
  
  output$ycol_selector = renderUI({#creates County select box object called in ui
    if (is.null(input$dataset)) return()
    
    ydata_available = datasetInput()
    #creates a reactive list of available counties based on the State selection made
    
    selectInput(inputId = "ycol", #name of input
                label = "Y variable:", #label displayed in ui
                choices = names(ydata_available), #calls list of available columns
                selected = unique(names(ydata_available))[2])
  })
  
  
  # Defining model
  model <- reactive({
    mymod <- Mclust(datasetInput(), G = input$nclust[1]:input$nclust[2])
  })
  
  coln <- reactive({  colnames(datasetInput()) })
  
  # The plot
  output$plot1 <- renderPlot({
    x = demcol_to_ind(coln() ,input$xcol)
    y = demcol_to_ind(coln(), input$ycol)
    if(input$plotType == "classification" || input$plotType == "uncertainty"){
      req(x && y)
      coordProj(data = datasetInput(), what = input$plotType, 
                parameters = model()$parameters, z = model()$z, 
                dimens = c(x, y), main = FALSE,                    #somehow coorProj runs before x,y defined for split second but default says 1,2
                xaxt = "n", yaxt = "n")
    }
    else if(input$plotType == "density"){
      if(x != y){
        density2d(model(), datasetInput(), x, y)
      }
    }
    else if(input$plotType =="BIC"){
      plot.mclustBIC(model()$BIC, G = input$nclust[1]:input$nclust[2])
    }
  })
  # The summary
  output$summary <- renderPrint({
    summarymclust(model())
  })
  
  #####################################################################################  
  #SERVER FOR CLUSTERING UPLOAD
  #####################################################################################
  
  datasetInput1 <- reactive({
    if(is.null(input$file1))     return(NULL)
    read.csv(input$file1$datapath,
             header = input$header,
             sep = input$sep)
  })
  
  output$contents <- renderTable({
    return(head(datasetInput1()))
    
  })
  
  output$xcol_selector1 = renderUI({
    
    if(input$labels == FALSE) xdata_available1 = datasetInput1()
    else xdata_available1 = datasetInput1()[,-1]
    
    selectInput(inputId = "xcol1", #name of input
                label = "X variable:", #label displayed in ui
                choices = colnames(xdata_available1), #calls list of available columns
                selected = unique(names(xdata_available1))[1])
  })
  
  output$ycol_selector1 = renderUI({
    
    if(input$labels == FALSE) ydata_available1 = datasetInput1()
    else ydata_available1 = datasetInput1()[,-1]
    
    
    selectInput(inputId = "ycol1", #name of input
                label = "Y variable:", #label displayed in ui
                choices = colnames(ydata_available1), #calls list of available columns
                selected = unique(names(ydata_available1))[2])
  })
  
  
  
  # Defining model
  model1 <- reactive({
    
    if(input$labels == FALSE) mymod <- Mclust(datasetInput1(), G = input$nclust1[1]:input$nclust1[2]) 
    else mymod <- Mclust(datasetInput1()[,-1], G = input$nclust1[1]:input$nclust1[2])
  })
  
  coln1 <- reactive({  
    if(input$labels == FALSE) colnames(datasetInput1())  
    else colnames(datasetInput1()[,-1]) }) 

  # The plot
  output$plot2 <- renderPlot({
    if(input$labels == FALSE){
    x1 = upcol_to_ind(coln1(), input$xcol1) 
    y1 = upcol_to_ind(coln1(), input$ycol1)
    if(input$plotType1 == "classification" || input$plotType1 == "uncertainty"){
      coordProj(data = datasetInput1(), what = input$plotType1, 
                parameters = model1()$parameters, z = model1()$z, 
                dimens = c(x1, y1), main = FALSE, 
                xaxt = "n", yaxt = "n")
    }
    else if(input$plotType1 == "density"){
      if(x1 != y1){
        density2d(model1(), datasetInput1(), x1, y1)
      }
    }
    else if(input$plotType1 =="BIC"){
      plot.mclustBIC(model1()$BIC, G = input$nclust1[1]:input$nclust1[2])
    }
    }
    else {
      x1 = upcol_to_ind(coln1(), input$xcol1) 
      y1 = upcol_to_ind(coln1(), input$ycol1)
      if(input$plotType1 == "classification" || input$plotType1 == "uncertainty"){
        coordProj(data = datasetInput1()[,-1], what = input$plotType1, 
                  parameters = model1()$parameters, z = model1()$z, 
                  dimens = c(x1, y1), main = FALSE, 
                  xaxt = "n", yaxt = "n")
      }
      else if(input$plotType1 == "density"){
        if(x1 != y1){
          density2d(model1(), datasetInput1()[,-1], x1, y1)
        }
      }
      else if(input$plotType1 =="BIC"){
        plot.mclustBIC(model1()$BIC, G = input$nclust1[1]:input$nclust1[2])
      }
    }
  })
  # The summary
  output$summary1 <- renderPrint({
    if(input$labels == FALSE) summarymclust(model1())
    else summarymclustlab(model1(), datasetInput1())
  })
  
  #####################################################################################
  #SERVER FOR CLASSIFICATION DEMO
  #####################################################################################
  output$dataset_selector3 = renderUI({ #creates State select box object called in ui
    
    selectInput(inputId = "dataset3", #name of input
                label = "Dataset:", #label displayed in ui
                choices = c("Iris", "Thyroid", "Diabetes", "Banknote"),
                # calls unique values from the State column in the previously created table
                selected = c("Iris")) #default choice (not required)
  })
  
  iris1<- iris[,c(5,1,2,3,4)] # reorder columns so class labels in first column
  
  datasetInput3 <- reactive({
    req(input$dataset3)
    switch(input$dataset3,
           "Iris" = iris1,
           "Thyroid" = thyroid,
           "Diabetes" = diabetes,
           "Banknote" = banknote)
  })
  
  output$xcol_selector3 = renderUI({#creates County select box object called in ui
    if (is.null(input$dataset3)) return()
    xdata_available3 = datasetInput3()[,2:ncol(datasetInput3())]
    #creates a reactive list of available counties based on the State selection made
    
    selectInput(inputId = "xcol3", #name of input
                label = "X variable:", #label displayed in ui
                choices = names(xdata_available3), #calls list of available columns
                selected = unique(names(xdata_available3))[1])
  })
  
  output$ycol_selector3 = renderUI({#creates County select box object called in ui
    if (is.null(input$dataset3)) return()
    
    ydata_available3 = datasetInput3()[,2:ncol(datasetInput3())]
    #creates a reactive list of available counties based on the State selection made
    
    selectInput(inputId = "ycol3", #name of input
                label = "Y variable:", #label displayed in ui
                choices = names(ydata_available3), #calls list of available columns
                selected = unique(names(ydata_available3))[2])
  })
  
  
  # Defining model
  model3 <- reactive({
    mymod3 <- MclustDA(datasetInput3()[,2:ncol(datasetInput3())],datasetInput3()[,1])
  })
  
  coln3 <- reactive({  colnames(datasetInput3()[,2:ncol(datasetInput3())]) })
  
  # The plot
  output$plot3 <- renderPlot({
    
    x3 = demcol_to_ind(coln3() ,input$xcol3)
    y3 = demcol_to_ind(coln3(), input$ycol3)
    
    plot.MclustDA(model3(), what=input$plotType3 ,dimens = c(x3,y3))
  })
  
  # The summary
  output$summary3 <- renderPrint({
    summary.MclustDA(model3())
  })
  
  #################################################################################
  #SERVER FOR CLASSIFICATION UPLOAD
  #################################################################################
  
  datasetInput4 <- reactive({
    if(is.null(input$file4))     return(NULL)
    read.csv(input$file4$datapath,
             header = input$header,
             sep = input$sep)
  })
  
  
  datasetInput5 <- reactive({
    if(is.null(input$file5)) return(NULL)
    read.csv(input$file5$datapath,
             header = input$header,
             sep = input$sep)
  })
  
  output$contents4 <- renderTable({
    return(head(datasetInput4()))
    
  })
  
  output$xcol_selector4 = renderUI({
    if (is.null(datasetInput4())) return()
    xdata_available4 = datasetInput4()[,2:ncol(datasetInput4())]
    
    
    selectInput(inputId = "xcol4", #name of input
                label = "X variable:", #label displayed in ui
                choices = colnames(xdata_available4), #calls list of available columns
                selected = unique(names(xdata_available4))[1])
  })
  
  output$ycol_selector4 = renderUI({
    if (is.null(datasetInput4())) return()
    
    ydata_available4 = datasetInput4()[,2:ncol(datasetInput4())]
    
    
    selectInput(inputId = "ycol4", #name of input
                label = "Y variable:", #label displayed in ui
                choices = colnames(ydata_available4), #calls list of available columns
                selected = unique(names(ydata_available4))[2])
  })
  
  
  
  # Defining model
  model4 <- reactive({
    mymod4 <- MclustDA(datasetInput4()[,2:ncol(datasetInput4())], datasetInput4()[,1])
  })
  
  coln4 <- reactive({  colnames(datasetInput4()) }) 
  
  # The plot
  output$plot4 <- renderPlot({
    
    x4 = upcol_to_ind(coln4(), input$xcol4) 
    y4 = upcol_to_ind(coln4(), input$ycol4)
    
      
      plot.traintest<- reactive({ 
        
        if(is.null(datasetInput5())){
          plot.MclustDA(model4(), what=input$plotType4 ,dimens = c(x4-1,y4-1)) 
        }  
        else plot.MclustDA(model4(), what=input$plotType4 ,dimens = c(x4-1,y4-1), newdata = datasetInput5()[,-1], newclass = datasetInput5()[,1])
        
        })
      
    plot.traintest()
  })

  
  # The summary
  output$summary4 <- renderPrint({
    
    summary.traintest<- reactive({ 
      if(is.null(datasetInput5())){
        summary.MclustDA(model4()) 
      }  
      else summary.MclustDA(model4(), newdata = datasetInput5()[,-1], newclass = datasetInput5()[,1])
      
      })
    return(summary.traintest())
    })
  
  
}

shinyApp(ui = ui, server = server)
