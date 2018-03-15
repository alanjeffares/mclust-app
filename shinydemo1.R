# Define UI for random distribution app ----
ui <- fluidPage(
# App title ----
titlePanel("Tabsets"),
# Sidebar layout with input and output definitions ----
sidebarLayout(
# Sidebar panel for inputs ----
sidebarPanel(
selectInput('xcol', 'X Variable', names(iris)),
selectInput('ycol', 'Y Variable', names(iris),
selected=names(iris)[[2]]),
# Input: Slider for the number of observations to generate ----
sliderInput("nclust",
"Number of clusters:",
value = c(1,9),
min = 1,
max = 9),
# Input: Select the random distribution type ----
radioButtons("plotType", "Plot type",
c("Classification"="classification", "BIC"="BIC"
, "Uncertainty" = "uncertainty", "Density"="density")
)
),
mainPanel(
tabsetPanel(type = "tabs",
tabPanel("Plot", plotOutput("plot1")),
tabPanel("Summary", verbatimTextOutput("summary"))
)
)
)
)
server <- function(input, output) {
selectedData <- reactive({
iris[, c(input$xcol, input$ycol)]
})
output$plot1 <- renderPlot({
plot(Mclust(selectedData(), G = input$nclust[1]:input$nclust[2]) , what = input$plotType)
})
output$summary <- renderPrint({
summary(Mclust(selectedData() , G = input$nclust))
})
}
shinyApp(ui = ui, server = server)
