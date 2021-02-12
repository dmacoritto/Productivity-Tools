
source("../R_Packages/packages.R")

if (!require(shiny)) install.packages("shiny"); library(shiny)


# Global variables can go here
n <- 200


# Define the UI
ui <- fluidPage(
  headerPanel("Hello Shiny!"),
  numericInput(inputId = 'n', label='Number of obs', value=n),
  plotOutput(outputId = 'plot')
)


# Define the server code
server <- function(input, output) {
  output$plot <- renderPlot({
    hist(runif(input$n))
  })
}

# Return a Shiny app object
shinyApp(ui = ui, server = server)




# k-means only works with numerical variables,
# so don't give the user the option to select
# a categorical variable
vars <- setdiff(names(iris), "Species")

ui <- fluidPage(
  mainPanel(
    plotOutput(outputId = 'plot')
  ),
  sliderInput(inputId = "years", 
              label = "Number of years:", 
              min = 1,
              max = 10, 
              value = 5),
  numericInput(inputId ="rate", label = "Discount rate:", value=1.1),
  numericInput(inputId ="investment", label = "Invesed amount:", value=1000)
)

server <- function(input, output) {
  # Combine the selected variables into a new data frame
  
  output$plot <- renderPlot({
    data.frame(years=c(1:input$years), rate=input$rate^c(1:input$years))%>%
      ggplot(aes(x=years, y=rate*input$investment))+
      geom_line(size=1)+
      scale_y_continuous(limits=c(0,input$rate^input$years*input$investment))

    })
  
}
shinyApp(ui = ui, server = server)
