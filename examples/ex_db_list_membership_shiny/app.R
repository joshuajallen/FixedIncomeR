
library(dplyr)
library(DT)
library(FIRVr)
library(shiny)
library(shinyjs)

source("db_list_membership_editor_module.R")

ui <- fluidPage(
  fluidRow(
    column(
      12,
      db_list_membership_editor_module_UI("list_editor")
    )
  ),
  useShinyjs()
)

server <- function(input, output) {

  callModule(
    module = db_list_membership_editor_module,
    id = "list_editor"
  )

}

# Run the application
shinyApp(ui = ui, server = server)
