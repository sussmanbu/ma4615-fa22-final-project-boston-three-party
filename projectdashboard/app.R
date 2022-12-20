#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# # Define UI for application that draws a histogram
# ui <- fluidPage(
# 
#     # Application title
#     titlePanel("Old Faithful Geyser Data"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             sliderInput("bins",
#                         "Number of bins:",
#                         min = 1,
#                         max = 50,
#                         value = 30)
#         ),
# 
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("distPlot")
#         )
#     )
# )

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      
      
      
        # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)
        # 
        # # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white',
        #      xlab = 'Waiting time to next eruption (in mins)',
        #      main = 'Histogram of waiting times')
    })
}

barley <- barley_co2_data
#barley <- read_csv(here::here("dataset", "barley_co2_data"))


g <- ggplot(barley, aes(x = CO2_emissions_kt, y = Production,
                        text = paste("Year:", Time, "<br>", "Production Output (1000s of bushels):", Production, "<br>", "Yield per Acre ( Bushels):", barley$`Yield per acre`))) +
  ylab("Production (1000s of Bushels)") +
  xlab("CO2 Emissions (kilotons)") +
  geom_point(position = "jitter")
ggplotly(g, tooltip = "text")


# Run the application 
shinyApp(ui = ui, server = server)




