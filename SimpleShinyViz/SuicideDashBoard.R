# RShiny - Suicides in India

suicides <- read.csv("Suicides in India 2001-2012.csv")
suicides$Year <- as.factor(suicides$Year)

library(dplyr)
library(ggplot2)
library(shiny)
library(RColorBrewer)

#==================================================================================================================

# using plotly and DT
# ui implies user interface
ui <- fluidPage(
  titlePanel('Suicides in States and UTs'),
  sidebarLayout(
    sidebarPanel(
      selectInput("State","Select State", choices = unique(suicides$State)),
      selectInput("Type_code", "Select Factor:", choices = unique(suicides$Type_code)),
      sliderInput("Year","Select Year",2001, 2012,2001)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plot", plotly::plotlyOutput("plot")),
        tabPanel("Table",DT::DTOutput("table"))
      )
    )
  )
)


server <- function(input, output, session) {
  output$plot <- plotly::renderPlotly({
    suicides %>%
      filter(State == input$State, Type_code == input$Type_code, Year == input$Year) %>%
      group_by(Type) %>%
      summarise(Count = sum(Total)) %>%
      filter(Count != 0) %>%
      ggplot(aes(Count,reorder(Type,Count), fill = Type )) +
      geom_col() +
      theme(legend.position = "none")
  })
  
  output$table <- DT::renderDT({
    suicides %>% filter(Type_code == input$Type_code,State == input$State,Year == input$Year) %>%
      group_by(Type) %>%
      summarise(Count = sum(Total)) %>%
      filter(Count != 0) %>%
      arrange(desc(Count))
  })
}

shinyApp(ui = ui, server = server)

#==================================================================================================================
