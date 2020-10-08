suppressPackageStartupMessages({
  library(shiny)
  library(tidyverse)
  library(shinythemes)
})

data <- readRDS("data/long_data.RDS")
group_lookup <- read_csv("data/groups.csv")
vars <- read_csv("data/vars.csv")

ui <- fluidPage(
  
  # Set theme
  theme = shinytheme("readable"),
  
  # add custom css
  tags$head(
    tags$style(HTML("
      tr:first-child { 
        font-weight: bold 
      }
      
      caption {
        color: black;
        font-size: 22px 
      }
      
      #gend_tbl tr:first-child { font-weight: normal }
      #med_tbl tr:first-child { font-weight: normal }
      #inc_tbl tr:first-child { font-weight: normal }
      #occ_tbl tr:first-child { font-weight: normal }
      #ind_tbl tr:first-child { font-weight: normal }
      #mor_tbl tr:first-child { font-weight: normal }
                    
    "))
  ),
  
  # Add title
  titlePanel(
    column(12, img(src = "CMSlogo.jpg", width = "60%"), align = "center"),
  ),
  
  # Instructions row
  fluidRow(
    h1("Title", align = "center"),
    column(
      8, offset = 2, 
      h3("Instructions"),
      p(
        style="text-align: justify;",
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
  
  # Options row
  fluidRow(
    column(3, 
      offset = 2, 
      selectInput("dio", "Diocese", data$diocese, data$diocese),
      align = "center"
    ),
    column(3,
      selectInput(
        "group", 
        "Group",
        setNames(group_lookup$group_id, group_lookup$group_name),
        multiple = TRUE
      ),
      align = "center"
    ),
    column(2, downloadButton("downloadData", "Download Data"), align = "center")
  ),
  
  # Output rows
  fluidRow(
    column(
      12, 
      tableOutput("yrs_usa_tbl"), align = "center",
    )
  ),
  fluidRow(
    column(
      12, 
      tableOutput("age_tbl"), align = "center",
    )
  ),
  fluidRow(
    column(
      12, 
      tableOutput("czn_tbl"), align = "center",
    )
  ),
  fluidRow(
    column(
      12, 
      tableOutput("gend_tbl"), align = "center",
    )
  ),
  fluidRow(
    column(
      12, 
      tableOutput("mar_tbl"), align = "center",
    )
  ),
  fluidRow(
    column(
      12, 
      tableOutput("eng_tbl"), align = "center",
    )
  ),
  fluidRow(
    column(
      12, 
      tableOutput("scl_tbl"), align = "center",
    )
  ),
  fluidRow(
    column(
      12, 
      tableOutput("edu_tbl"), align = "center",
    )
  ),
  fluidRow(
    column(
      12, 
      tableOutput("med_tbl"), align = "center",
    )
  ),
  fluidRow(
    column(
      12, 
      tableOutput("emp_tbl"), align = "center",
    )
  ),
  fluidRow(
    column(
      12, 
      tableOutput("pov_tbl"), align = "center",
    )
  ),
  fluidRow(
    column(
      12, 
      tableOutput("inc_tbl"), align = "center",
    )
  ),
  fluidRow(
    column(
      12, 
      tableOutput("occ_tbl"), align = "center",
    )
  ),
  fluidRow(
    column(
      12, 
      tableOutput("ind_tbl"), align = "center",
    )
  ),
  fluidRow(
    column(
      12, 
      tableOutput("mor_tbl"), align = "center",
    )
  )
)

server <- function(input, output, session) {
  
  # Get the data to be returned as a .csv file to the user
  selected <- reactive({
    if (input$dio == "" || nrow(data %>% filter(group_id %in% input$group)) == 0) {
      return(NULL)
    }
    
    data <- data %>% 
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
      select(-diocese, -var) %>%
      rename(
        "Category" = grouping,
        "Variable" = name
      )
    
    to_exclude_ind_occ <- c(
      "Migrant Farmworkers",
      "Peoples of the Sea",
      "Traveling Show Ministeries",
      "Civil Aviation Employees"
    )
    
    if(all(input$group %in% to_exclude_ind_occ)) {
      data <- data %>%
        filter((Category != "Occupation") && (Category != "Industry"))
    }
    return(data)
  })
  
  # Manipulate the data to prepare it for html presentation
  display <- reactive({
    if (is.null(selected())) {
      return(NULL)
    }
    selected() %>%
      mutate_if(is.double, function(x) prettyNum(x, big.mark=",")) %>%
      rename(" " = Variable)
  })
  
  # Get downloadable csv
  output$downloadData <- downloadHandler(
    filename = function() {
      paste(input$dio, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(selected(), file, row.names = TRUE)
    }
  )
  
  # Sub-tables for displaying
  ###########################
  
  # Years in USA
  output$yrs_usa_tbl <- renderTable(
    {
      if (is.null(display())) {
        return(NULL)
      }
      display() %>%
        filter(Category == "Years in USA") %>%
        select(-Category)
    }, 
    width = "80%", 
    align = "r",
    hover = TRUE,
    caption = "Years in the USA",
    caption.placement = getOption(
      "xtable.caption.placement", "top"
    )
  )
  
  # Age
  output$age_tbl <- renderTable(
    {
      if (is.null(display())) {
        return(NULL)
      }
      display() %>%
        filter(Category == "Age") %>%
        select(-Category)
    }, 
    width = "80%", 
    align = "r",
    hover = TRUE,
    caption = "Age",
    caption.placement = getOption(
      "xtable.caption.placement", "top"
    )
  )
  
  # Citizenship
  output$czn_tbl <- renderTable(
    {
      if (is.null(display())) {
        return(NULL)
      }
      display() %>%
        filter(Category == "Citizenship") %>%
        select(-Category)
    }, 
    width = "80%", 
    align = "r",
    hover = TRUE,
    caption = "Citizenship",
    caption.placement = getOption(
      "xtable.caption.placement", "top"
    )
  )
  
  # Technology Access
  output$tech_tbl <- renderTable(
    {
      if (is.null(display())) {
        return(NULL)
      }
      display() %>%
        filter(Category == "Technology Access") %>%
        select(-Category)
    }, 
    width = "80%", 
    align = "r",
    hover = TRUE,
    caption = "Technology Access",
    caption.placement = getOption(
      "xtable.caption.placement", "top"
    )
  )
  
  # Gender
  output$gend_tbl <- renderTable(
    {
      if (is.null(display())) {
        return(NULL)
      }
      display() %>%
        filter(Category == "Gender") %>%
        select(-Category)
    }, 
    width = "80%", 
    align = "r",
    hover = TRUE,
    caption = "Gender",
    caption.placement = getOption(
      "xtable.caption.placement", "top"
    )
  )
  
  # Marital Status
  output$mar_tbl <- renderTable(
    {
      if (is.null(display())) {
        return(NULL)
      }
      display() %>%
        filter(Category == "Marital Status") %>%
        select(-Category)
    }, 
    width = "80%", 
    align = "r",
    hover = TRUE,
    caption = "Marital Status",
    caption.placement = getOption(
      "xtable.caption.placement", "top"
    )
  )
  
  # Level of English proficiency
  output$eng_tbl <- renderTable(
    {
      if (is.null(display())) {
        return(NULL)
      }
      display() %>%
        filter(Category == "Level of English Proficiency") %>%
        select(-Category)
    }, 
    width = "80%", 
    align = "r",
    hover = TRUE,
    caption = "Level of English Proficiency",
    caption.placement = getOption(
      "xtable.caption.placement", "top"
    )
  )
  
  # School Enrollment
  output$scl_tbl <- renderTable(
    {
      if (is.null(display())) {
        return(NULL)
      }
      display() %>%
        filter(Category == "School Enrollment") %>%
        select(-Category)
    }, 
    width = "80%", 
    align = "r",
    hover = TRUE,
    caption = "School Enrollment",
    caption.placement = getOption(
      "xtable.caption.placement", "top"
    )
  )
  
  # Educational Attainment
  output$edu_tbl <- renderTable(
    {
      if (is.null(display())) {
        return(NULL)
      }
      display() %>%
        filter(Category == "Educational Attainment") %>%
        select(-Category)
    }, 
    width = "80%", 
    align = "r",
    hover = TRUE,
    caption = "Educational Attainment",
    caption.placement = getOption(
      "xtable.caption.placement", "top"
    )
  )
  
  # Medicaid
  output$med_tbl <- renderTable(
    {
      if (is.null(display())) {
        return(NULL)
      }
      display() %>%
        filter(Category == "Medicaid") %>%
        select(-Category)
    }, 
    width = "80%", 
    align = "r",
    hover = TRUE,
    caption = "Medicaid",
    caption.placement = getOption(
      "xtable.caption.placement", "top"
    )
  )
  
  # Employment
  output$emp_tbl <- renderTable(
    {
      if (is.null(display())) {
        return(NULL)
      }
      display() %>%
        filter(Category == "Employment") %>%
        select(-Category)
    }, 
    width = "80%", 
    align = "r",
    hover = TRUE,
    caption = "Employment",
    caption.placement = getOption(
      "xtable.caption.placement", "top"
    )
  )
  
  # Poverty
  output$pov_tbl <- renderTable(
    {
      if (is.null(display())) {
        return(NULL)
      }
      display() %>%
        filter(Category == "Poverty") %>%
        select(-Category)
    }, 
    width = "80%", 
    align = "r",
    hover = TRUE,
    caption = "Poverty",
    caption.placement = getOption(
      "xtable.caption.placement", "top"
    )
  )
  
  # Income
  output$inc_tbl <- renderTable(
    {
      if (is.null(display())) {
        return(NULL)
      }
      display() %>%
        filter(Category == "Income") %>%
        select(-Category)
    }, 
    width = "80%", 
    align = "r",
    hover = TRUE,
    caption = "Income",
    caption.placement = getOption(
      "xtable.caption.placement", "top"
    ),
  )
  
  # Occupation
  output$occ_tbl <- renderTable(
    {
      if (is.null(display())) {
        return(NULL)
      }
      display() %>%
        filter(Category == "Occupation") %>%
        select(-Category)
    }, 
    width = "80%", 
    align = "r",
    hover = TRUE,
    caption = "Occupation",
    caption.placement = getOption(
      "xtable.caption.placement", "top"
    )
  )
  
  # Industry
  output$ind_tbl <- renderTable(
    {
      if (is.null(display())) {
        return(NULL)
      }
      display() %>%
        filter(Category == "Industry") %>%
        select(-Category)
    }, 
    width = "80%", 
    align = "r",
    hover = TRUE,
    caption = "Industry",
    caption.placement = getOption(
      "xtable.caption.placement", "top"
    )
  )
  
  # Mortgage
  output$mor_tbl <- renderTable(
    {
      if (is.null(display())) {
        return(NULL)
      }
      display() %>%
        filter(Category == "Mortgage") %>%
        select(-Category)
    }, 
    width = "80%", 
    align = "r",
    hover = TRUE,
    caption = "Mortgage",
    caption.placement = getOption(
      "xtable.caption.placement", "top"
    )
  )
  
}
shinyApp(ui, server)