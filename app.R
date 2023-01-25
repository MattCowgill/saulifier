library(data.table)
library(dplyr)
library(tidyr)
library(shiny)
library(curl)

options(shiny.maxRequestSize = 30*1024^2)

owid_url <- "https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"
owid <- data.table::fread(owid_url)

default_countries <- c("World", "Australia", "New Zealand", "United States", 
                       "Canada", "China", "Japan", "South Korea", "Taiwan",
                       "Hong Kong", "Singapore", "Malaysia", "Thailand", "Indonesia",
                       "Philippines", "Vietnam", "Pakistan", "Bangladesh", "Iran", "Iraq",
                       "Germany", "France", "Italy", "Spain", "Netherlands",
                       "Switzerland", "United Kingdom", "Ireland", "Denmark", "Sweden",
                       "Norway", "South Africa", "Brazil", "Mexico", "Colombia", "Peru",
                       "Chile", "Russia", "Egypt", "Saudi Arabia", "Qatar", "Israel",
                       "Kazakhstan", "Turkey")

all_series <- colnames(owid)[!colnames(owid) %in% c("iso_code", "continent", "location",
                             "date")]

default_series <- c("total_cases", "new_cases",
                    "total_deaths", "new_deaths",
                    "total_cases_per_million",
                    "total_deaths_per_million")

# Define UI for data upload app ----
ui <- fluidPage(
    
    list(tags$head(tags$style(HTML("
                                 .multicol { 
                                   height: 1150px;
                                   -webkit-column-count: 5; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 5;    /* Firefox */ 
                                   column-count: 5; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 "),
                              HTML("
                                 .multicol2 { 
                                   height: 200px;
                                   -webkit-column-count: 5; /* Chrome, Safari, Opera */ 
                                   -moz-column-count: 5;    /* Firefox */ 
                                   column-count: 5; 
                                   -moz-column-fill: auto;
                                   -column-fill: auto;
                                 } 
                                 ")) 
    )),
    
    # App title ----
    titlePanel("Download wide OWiD COVID data"),
    
    # Main panel for displaying outputs ----
    mainPanel(
        textOutput("summary"),
        
        # Output: Download CSV ----
        h3("Download data"),
        fluidRow(
            downloadButton("download_data", "Download CSV"),
        ),
        br(),
        h3("Preview data"),
        fluidRow(
            # Output: Data file ----
            DT::dataTableOutput("df")
        ),
        h3("Customise data")
    ),
    
    # Sidebar layout with input and output definitions ----
    
    fluidRow(
        column(width = 6,
               wellPanel(
                   checkboxGroupInput("selected_countries",
                                      label = "Select countries to include",
                                      choices = unique(owid$location),
                                      width = '100%',
                                      selected = default_countries)
               )),
        column(width = 6,
               wellPanel(  
                   checkboxGroupInput("selected_series",
                                      label = "Select series to include",
                                      choices = all_series,
                                      selected = default_series)
                   
               ))
    )
    )
#)

# Define server logic to read selected file ----
server <- function(input, output) {
    
    df <- reactive({
        owid %>%
            spread_owid(selected_countries = input$selected_countries,
                        selected_series = input$selected_series)
    })
    
    output$df <- DT::renderDataTable(
        #head(df())
        df(),
        options = list(dom = 't'),
        width = 800
        )
    
    output$summary <- reactive({
        max_date <- max(df()$date)
        
        paste0("This web app pulls the latest COVID-19 data from Our World in Data, ",
        "filters it to the countries and series of interest (customise below), ",
        "then reshapes it from long to wide. Download a CSV of the formatted data ",
        "using the button below. The latest data is from ", max_date, ".")
    })
    
    output$download_data <- downloadHandler(
        filename = "owid.csv",
        content = function(file) {
            to_write <- df()
            to_write <- to_write %>%
                mutate_all(as.character) %>%
                mutate_all(~if_else(is.na(.), "", .))
                
            readr::write_csv(to_write, file)
        }
    )
    
}

# Create Shiny app ----
shinyApp(ui, server)