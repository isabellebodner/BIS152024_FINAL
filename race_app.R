ui <- dashboardPage(
  dashboardHeader(title = "Location and Race Breakdown 2017-2023"),
  dashboardSidebar(
    selectInput("race", "Select Race", choices = unique(race_app$race)),
    selectInput("location", "Select Location", choices = unique(race_app$location)),
    selectInput("y_axis", "Select Y-axis", choices = c("Total Count", "Average"))
  ),
  dashboardBody(
    fluidRow(
      box(plotOutput("barPlot"), width = 12)
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Filter data based on user input
  filtered_data <- reactive({
    race_app %>%
      mutate(calendar_year=as.factor(calendar_year)) %>%
      filter(race == input$race, location == input$location)
  })
  
  # Create bar plot
  output$barPlot <- renderPlot({
    ggplot(filtered_data(), aes(x = calendar_year, y = if (input$y_axis == "Total Count") total_count else average))+
      geom_col(color="black", fill="steelblue", alpha=0.75) +
      labs(title = paste(input$y_axis, "for", input$race, "in", input$location),
           x = "Year",
           y = input$y_axis)
  })
}

# Run the app
shinyApp(ui, server)