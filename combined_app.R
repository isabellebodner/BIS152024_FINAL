library(tidyverse)
library(dplyr)
library(janitor)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(palmerpenguins)
library(naniar)

race <- read_csv("race.csv",na = c("NA", " ", "*"))
race_homeless<- clean_names(race)

age <- read_csv("age.csv")
age <- clean_names(age)

race_app <- race_homeless %>% 
  mutate(experiencing_homelessness_cnt=as.numeric(experiencing_homelessness_cnt)) %>% 
  filter(experiencing_homelessness_cnt!="NA"|experiencing_homelessness_cnt!="*") %>% 
  group_by(race, location, calendar_year) %>% 
  summarize(total_count=sum(experiencing_homelessness_cnt),
            average=mean(experiencing_homelessness_cnt))

age_homeless <- clean_names(age_grouped)

age_app <- age_homeless %>% 
  mutate(experiencing_homelessness_cnt=as.numeric(experiencing_homelessness_cnt)) %>% 
  filter(experiencing_homelessness_cnt!="NA"|experiencing_homelessness_cnt!="*") %>% 
  group_by(age_group, location, calendar_year) %>% 
  summarize(total_count=sum(experiencing_homelessness_cnt),
            average=mean(experiencing_homelessness_cnt))

ui <- dashboardPage(
  dashboardHeader(title = "Location-Race-Age Breakdown 2017-2023"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Race", tabName = "race"),
      menuItem("Age", tabName = "age")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "race",
              selectInput("race", "Select Race", choices = unique(race_app$race)),
              selectInput("location", "Select Location", choices = unique(race_app$location)),
              selectInput("y_axis", "Select Y-axis", choices = c("Total Count", "Average")),
              fluidRow(
                box(plotOutput("race_barPlot"), width = 12)
              )),
      tabItem(tabName = "age",
              selectInput("age_group", "Select Age Group", choices = unique(age_app$age_group)),
              selectInput("location_age", "Select Location", choices = unique(age_app$location)),
              selectInput("y_axis_age", "Select Y-axis", choices = c("Total Count", "Average")),
              fluidRow(
                box(plotOutput("age_barPlot"), width = 12)
              ))
    )
  )
)

# Define server
server <- function(input, output) {
  
  # Filter data based on user input (race)
  filtered_race_data <- reactive({
    race_app %>%
      filter(race == input$race, location == input$location)
  })
  
  # Filter data based on user input (age)
  filtered_age_data <- reactive({
    age_app %>%
      filter(age_group == input$age_group, location == input$location_age)
  })
  
  # Create bar plot for race app
  output$race_barPlot <- renderPlot({
    ggplot(filtered_race_data(), aes(x = calendar_year, y = if (input$y_axis == "Total Count") total_count else average)) +
      geom_col(color = "black", fill = "steelblue", alpha = 0.75) +
      labs(title = paste(input$y_axis, "for", input$race, "in", input$location),
           x = "Year",
           y = input$y_axis)
  })
  
  # Create bar plot for age app
  output$age_barPlot <- renderPlot({
    ggplot(filtered_age_data(), aes(x = calendar_year, y = if (input$y_axis_age == "Total Count") total_count else average)) +
      geom_col(color = "black", fill = "steelblue", alpha = 0.75) +
      labs(title = paste(input$y_axis_age, "for Age Group", input$age_group, "in", input$location_age),
           x = "Year",
           y = input$y_axis_age)
  })
}

# Run the app
shinyApp(ui, server)