suppressPackageStartupMessages({
  library(shiny)
  library(tidyverse)
})

data <- readRDS("data/long_data.RDS")
group_lookup <- read_csv("data/groups.csv")
vars <- read_csv("data/vars.csv")

ui <- fluidPage(
  fluidRow(
    column(6, selectInput("dio", "Diocese", data$diocese, data$diocese)),
    column(
      6, 
      selectInput(
        "group", 
        "Group",
        setNames(group_lookup$group_id, group_lookup$name),
        multiple = TRUE
      )
    )
  ),
  fluidRow(
    column(12, tableOutput("table"), align = "center")
  )
)

server <- function(input, output, session) {
  
  selected <- reactive(
    data %>% 
      filter(diocese == input$dio) %>%
      filter(group_id %in% input$group) %>%
      inner_join(group_lookup, by = "group_id") %>%
      select(-group_id) %>%
      pivot_longer(
        cols = -all_of(c("diocese", "group_name")),
        names_to = "var",
        values_to = "Count"
      ) %>% 
      inner_join(vars, by = "var") %>%
      pivot_wider(
        id_cols = c("diocese", "var", "grouping", "name"),
        names_from = group_name,
        values_from = Count
      ) %>% 
      select(-diocese, -var)
  )
  
  output$table <- renderTable(
    {
      selected()
    }, digits = 0
  )
}
shinyApp(ui, server)