suppressPackageStartupMessages({
  library(shiny)
  library(tidyverse)
  library(shinythemes)
})

data <- readRDS("data/long_data.RDS")
group_lookup <- read_csv("data/groups.csv")
group_descs <- read_csv("data/group_descs.csv")
vars <- read_csv("data/vars.csv")

ui <- fluidPage(
  
  # Set theme
  theme = shinytheme("readable"),
  
  # add custom css
  tags$head(
    tags$title("CMS Pastoral Care Database"),
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
      #group_descriptions tr:first-child { font-weight: normal }
      
      #group_descriptions caption {
        color: white;
        font-size: 26px
      }              
      
      #group_descriptions {
        border: 0px solid white;
        background-color: #0C5697;
        color: white;
      }
      
      #group_descriptions tbody {
        padding-bottom: 1000px;
        margin-bottom: 1000px
      }
      
    "))
  ),
  
  # Add title
  titlePanel(
    column(12, img(src = "CMSlogo.jpg", width = "50%"), align = "center")
  ),
  
  # Instructions row
  fluidRow(
    column(8, offset = 2,
           h2(
             "Database on Populations of Interest to the USCCB Subcommittee of 
             Migrants, Refugees and Travelers by Archdiocese/Diocese: 2017", 
             align = "center"
           )
          ),
    column(
      8, offset = 2, 
      h3("Instructions"),
      p(
        'Before using this tool, please',
        a(
          href = 'PCMRT Methodology and Description2-5-21.pdf', 
          "click here",
          download = NA,
          target = '_blank'
        ),
        'to download and read a detailed description of the data that it uses 
        and how to interpret it.'
      ),
      p(
        style="text-align: justify;",
        'Please select the Arch/diocese and the immigrant group of 
        interest using the dropdown menus below. A table summarizing the 
        characteristics of the group(s) you selected will be displayed below.
        You can download the data using the "Download Data" button to the right.
        For detailed descriptions of selected groups, see the bottom of the page.'
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
  
  # Notes row
  fluidRow(
    column(8, offset = 2,
      h5("Notes:"),
      HTML(
        "<ul>
        <li>The American Community Survey includes all individuals residing at 
        addresses randomly selected by the Census Bureau.</li>
        <li>Unless otherwise noted, the categories cover foreign-born persons.
        </li>
        <li>Estimates below 1000 may have a large margin of error and should be 
        used with caution. Margins of error may be particularly high in 
        geographies with few respondents to the American Community Survey. 
        Correspondingly, an estimate of zero immigrants in a given diocese 
        does not necessarily mean that there are no immigrants in that 
        diocese.</li>
        </ul>"
      ),
      h5("Source:"),
      p(
        style="text-align: justify;",
        "Center for Migration Studies of New York. Calculations based on 2017 
        1-Year American Community Survey data from the Census Bureau."
      )
    )
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
  ),
  
  # Group descriptions row
  fluidRow(
    column(
      12,
      tableOutput("group_descriptions"),
      align = "center"
    )
  ),
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
  
  # Get group description table
  output$group_descriptions <- renderTable(
    {
    if(is.null(display())) {
      return(NULL)
    }
      gd <- group_descs %>%
        filter(group_name %in% names(display())) %>%
        rename(" " = group_name,
               "Description" = description)
     if(nrow(gd) == 0) {
       return(NULL) 
     }
      return(gd)
    
    },
    width = "80%", 
    hover = TRUE,
    caption = "Group Descriptions:",
    caption.placement = getOption(
      "xtable.caption.placement", "top"
    )
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