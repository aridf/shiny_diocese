suppressPackageStartupMessages({
  library(shiny)
  library(tidyverse)
  library(shinythemes)
})

data <- readRDS("data/long_data.RDS")
group_lookup <- read_csv("data/groups.csv")
vars <- read_csv("data/vars.csv")

ui <- fluidPage(theme = shinytheme("slate"),
  titlePanel(
    h1("Title", align = "center")
  ),
  fluidRow(
    column(2, offset = 1, img(src = "logo.png", height = "200px")),
    column(8, offset = -1, 
           h3("Instructions"),
           p(style="text-align: justify;",
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do
             eiusmod tempor incididunt ut labore et dolore magna aliqua. 
             Ut enim ad minim veniam, quis nostrud exercitation ullamco 
             laboris nisi ut aliquip ex ea commodo consequat. 
             Duis aute irure dolor in reprehenderit in voluptate velit esse 
             cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat 
             cupidatat non proident, sunt in culpa qui officia deserunt mollit 
             anim id est laborum."
           )
          )
  ),
  fluidRow(
    column(4, offset = 2, selectInput("dio", "Diocese", data$diocese, data$diocese)),
    column(4, offset = -2,
      selectInput(
        "group", 
        "Group",
        setNames(group_lookup$group_id, group_lookup$group_name),
        multiple = TRUE
      )
    )
  ),
  fluidRow(
    br(),
    column(12, tableOutput("table"), align = "center")
  )
)

server <- function(input, output, session) {
  
  selected <- reactive({
    if (input$dio == "" || nrow(data %>% filter(group_id %in% input$group)) == 0) {
      return(NULL)
    }
    
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
  })
  
  output$table <- renderTable(
    {
      selected()
    }, digits = 0
  )
}
shinyApp(ui, server)