
setwd("/Users/sebastianjin/Documents/hw_3/R")
options(expressions = 150000)
library(shiny)
library(purrr)
source("ct-util.R")
max_num_studies = 1000
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Clinical Trials Query"),
  # Q3 drop down bar
  sidebarLayout(
    sidebarPanel(
      textInput("brief_title_kw", "Brief title keywords"),
      selectInput("source_class",
                  label = h3("Sponsor Type"), 
                  choices = list(
                    "Federal" = "FED", 
                    "Individual" = "INDIV",
                    "Industry" = "INDUSTRY",
                    "Network" = "NETWORK",
                    "NTH" = "NTH",
                    "Other" = "OTHER",
                    "Other gov" = "OTHER_GOV",
                    "Unknown" = "UNKNOWN"
                  ),
                  selected = "Federal"
      ),
      actionButton("clear_button", "Clear"), # Clear button
      downloadButton("export_data", "Export Data") # Export Data button
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Phase", plotOutput("phase_plot")),
        tabPanel("Concurrent", plotOutput("concurrent_plot")),
        tabPanel("Conditions", plotOutput("conditions_plot"))  
      ),
      dataTableOutput("trial_table")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  get_studies <- reactive({
    if (input$brief_title_kw != "") {
      keywords <- input$brief_title_kw |>
        strsplit(",") |>
        unlist() |>
        trimws()
      studies_query <- query_kwds(studies, keywords, "brief_title", match_all = TRUE)
    } else {
      studies_query <- studies
    }
    if (!is.null(input$source_class)){
        studies_query = studies_query |> filter(source_class %in% !!input$source_class)
    }
   # input$source_class
    studies_query |>
      head(max_num_studies) |>
      collect()
  })
  
  output$phase_plot <- renderPlot({
    get_studies() |>
      plot_phase_histogram()
  })
  
  output$concurrent_plot <- renderPlot({
    get_studies() |>
      select(start_date, completion_date) |>
      get_concurrent_trials() |>
      ggplot(aes(x = date, y = count)) +
      geom_line() +
      xlab("Date") +
      ylab("Count") + 
      theme_bw()
  })
  
  output$trial_table <- renderDataTable({
    get_studies() |> 
      select(nct_id, brief_title, start_date, completion_date) |>
      rename(`NCT ID` = nct_id, `Brief Title` = brief_title,
             `Start Date` = start_date, `Completion Date` = completion_date)
  })
  
  output$conditions_plot <- renderPlot({
     conditions_data <- get_condition_data(con)
     plot_condition_histogram(conditions_data)
   })
  
  observeEvent(input$clear_button, {
    # Clear button logic
    updateTextInput(session, "brief_title_kw", value = "")
    updateCheckboxGroupInput(session, "source_class", selected = 1)
    output$phase_plot <- renderPlot(NULL)
    output$concurrent_plot <- renderPlot(NULL)
    output$conditions_plot <- renderPlot(NULL)
    output$trial_table <- renderDataTable(NULL)
  })
  
  output$export_data <- downloadHandler(
    filename = function() {
      "clinical_trials_data.csv"
    },
    content = function(file) {
      write.csv(get_studies(), file, row.names = FALSE)
    }
  )
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
