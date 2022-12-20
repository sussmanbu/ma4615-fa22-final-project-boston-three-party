#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(plotly)
library(DT)
library(formattable)

ui <- fluidPage(
  titlePanel("Flaxseed Production Data Analysis"),
  sliderInput(inputId = "years",
              label = "Year",
              value=1990, min=1990, max=2021), 
  plotlyOutput ("plot1"),
  plotlyOutput ("plot2"),
  formattableOutput ("table1")
)
server <- function(input, output) { 
  flax <- read_csv("/Users/VS/Desktop/MA415/Project/ma4615-fa22-final-project-boston-three-party/shiny_app/dashboard/flaxseed_data.csv")
  
  output$plot1 <- renderPlotly({
    flax <- flax %>% filter(Time <= input$years)
    gg <- ggplot(flax,
               aes(x = Time, y = Production,
                   color = CO2_emissions_kt.x,
                   size = flax$`Yield per acre`, 
                   text = paste("Year:", Time, "<br>", "CO2 Emissions (kilotons):", flax$CO2_emissions_kt.x, "<br>", "Production Output (1000s of Bushels):", Production, "<br>", "Yield per acre (Bushels):", flax$`Yield per acre`, "<br>", "Value of Production (1000 dollars):", flax$`Value of production`))) +
    geom_point() +
    #geom_point(aes(frame = Time)) +
    scale_x_log10()
  ggplotly(gg, tooltip = "text")
  })
  output$plot2 <- renderPlotly({
  g <- ggplot(flax, aes(x = CO2_emissions_kt.x, y = Production,
                        text = paste("Year:", Time, "<br>", "Production Output (1000s of bushels):", Production, "<br>", "Yield per Acre (Bushels):", flax$`Yield per acre`))) +
    ylab("Production (1000s of Bushels)") +
    xlab("CO2 Emissions (kilotons)") +
    geom_point(position = "jitter")
  ggplotly(g, tooltip = "text")
  })
  
  #add slider range
  #add 2nd slider to get min/max
 
  output$table1 <- renderFormattable({
    
    flax %>% select(Time, CO2_emissions_kt.x, Production) %>%
      #filter(row_number() %% 20 == 0) %>%
      formattable(list(displ = color_tile("white", "orange"),
      area(col = c(CO2_emissions_kt.x)) ~ normalize_bar("chartreuse", 0.2),
        Production = formatter("span",
        style = Production ~ ifelse(Production>=10000, style(color = "green"),
          ifelse(Production>=5000, style(color = "blue"),
        style(color = "red"))))))})  
     
}
shinyApp(ui, server)