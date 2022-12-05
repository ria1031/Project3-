# SDS Project 3

library(shiny)


windmill_data <- read.csv("project1_data.csv")


# Question 1


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Windmill Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      #Select box for variable:
      selectInput("selectvar", label = h3("Choose a variable"), 
                  choices=list("Turbine Capacity"=1, "Turbine Hub Height"=2,"Turbine Rotor Diameter"=3, "Turbine Site States"=4,"Turbine Site Counties"=5), 
                  selected = 1),
      
      # Slider input for number of bins
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
      sliderInput("range", "Rotor Diameter Range:",
                  min=0, max=200,
                  value=c(0,200)),
      
      radioButtons("color",label=h3("Choose A Color:"), choices=list("Red"=1, "Blue"=2), selected=1),
      
      #option to show mean
      checkboxInput("checkbox1", label="Display mean", value=FALSE),
      
      
      #option to show sd
      checkboxInput("checkbox2", label="Display standard deviation", value=FALSE),
      
      #option to show table
      checkboxInput("checkbox3", label="Display table", value=FALSE )
      
    
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      img(src = "windmill_photo.jpeg", height = 200, width = 200),
      
      plotOutput("distPlot"),
      hr(),
      p('Mean:'),
      fluidRow(column(5, verbatimTextOutput("mean"))),
      p('Standard deviation:'),
      fluidRow(column(5, verbatimTextOutput("sd"))),
      p('Table:'),
      fluidRow(column(10,verbatimTextOutput("table"))),
  
      
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    if(input$selectvar == 1 & input$color == 1){
      hist(windmill_data$Turbine.Capacity, breaks = input$bins, main='Distribution of Turbine Capacities',xlab='Turbine Capacities', col="red", border = 'darkgrey')
    }
    
    if(input$selectvar == 2 & input$color == 1){
      hist(windmill_data$Turbine.Hub_Height, breaks = input$bins, main='Distribution of Turbine Hub Heights',xlab='Turbine Hub Heights',col = 'red', border = 'darkgrey')
      
    }
    
    if(input$selectvar == 3 & input$color == 1){
      hist(windmill_data$Turbine.Rotor_Diameter, breaks = input$bins, main='Distribution of Turbine Rotor Diameters',xlab='Turbine Rotor Diameters',col = 'red', border = 'darkgrey', xlim=input$range)
      
    }
    if(input$selectvar == 4 & input$color == 1){
      barplot(table(windmill_data$Site.State), breaks = input$bins, main='Distribution of Turbine Site States',xlab='Turbine Site States',col = 'red', border = 'darkgrey')
      
    }
    if(input$selectvar == 5 & input$color == 1){
      barplot(table(windmill_data$Site.County), breaks = input$bins, main='Distribution of Turbine Site Counties',xlab='Turbine Site Counties',col = 'red', border = 'darkgrey')
    }
    
    #second color
    
    if(input$selectvar == 1 & input$color == 2){
      hist(windmill_data$Turbine.Capacity, breaks = input$bins, main='Distribution of Turbine Capacities',xlab='Turbine Capacities', col="blue", border = 'darkgrey')
    }
    
    if(input$selectvar == 2 & input$color == 2){
      hist(windmill_data$Turbine.Hub_Height, breaks = input$bins, main='Distribution of Turbine Hub Heights',xlab='Turbine Hub Heights',col = 'blue', border = 'darkgrey')
      
    }
    if(input$selectvar == 3 & input$color == 2){
      hist(windmill_data$Turbine.Rotor_Diameter, breaks = input$bins, main='Distribution of Turbine Rotor Diameters',xlab='Turbine Rotor Diameters',col = 'blue', border = 'darkgrey', xlim=input$range)
      
    }
    if(input$selectvar == 4 & input$color == 2){
      barplot(table(windmill_data$Site.State), breaks = input$bins, main='Distribution of Turbine Site States',xlab='Turbine Site States',col = 'blue', border = 'darkgrey')
      
    }
    if(input$selectvar == 5 & input$color == 2){
      barplot(table(windmill_data$Site.County), breaks = input$bins, main='Distribution of Turbine Site Counties',xlab='Turbine Site Counties',col = 'blue', border = 'darkgrey')
    }
  })
  
  
  #Display mean if selected
  output$mean <- renderPrint({ 
    if(input$checkbox1 == TRUE & input$selectvar == 1){
      mean(windmill_data$Turbine.Capacity, na.rm=TRUE)}
    else if(input$checkbox1 == TRUE & input$selectvar == 2) {
      mean(windmill_data$Turbine.Hub_Height, na.rm=TRUE)}
    else if(input$checkbox1 == TRUE & input$selectvar == 3) {
      mean(windmill_data$Turbine.Rotor_Diameter, na.rm=TRUE)}
    else if(input$checkbox1 == TRUE & input$selectvar == 4) {
      mean(windmill_data$Site.State, na.rm=TRUE)}
    else if(input$checkbox1 == TRUE & input$selectvar == 5) {
      mean(windmill_data$Site.County, na.rm=TRUE)}
    
    
    
    
  })
  
  #Display sd if selected
  output$sd <- renderPrint({ 
    if(input$checkbox2 == TRUE & input$selectvar == 1){
      sd(windmill_data$Turbine.Capacity, na.rm=TRUE)}
    else if(input$checkbox2 == TRUE & input$selectvar == 2){
      sd(windmill_data$Turbine.Hub_Height, na.rm=TRUE)}
    else if(input$checkbox2 == TRUE & input$selectvar == 3){
      sd(windmill_data$Turbine.Rotor_Diameter, na.rm=TRUE)}
    else if(input$checkbox2 == TRUE & input$selectvar == 4){
      sd(windmill_data$Site.State, na.rm=TRUE)}
    else if(input$checkbox2 == TRUE & input$selectvar == 5){
      sd(windmill_data$Site.County, na.rm=TRUE)}
  })
  
  #Display Table
  
  output$table <- renderPrint({
    if(input$checkbox3 == TRUE & input$selectvar == 4){
      table(windmill_data$Site.State)}
    else if(input$checkbox3 == TRUE & input$selectvar == 5){
      table(windmill_data$Site.County)}
    })

  
}

#Run the application 
shinyApp(ui = ui, server = server)

