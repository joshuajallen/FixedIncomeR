#
# Shiny module to edit database list membership using FIRVr list
# membership functionality.
#

#
# User interface to be displayed in the webpage
#

db_list_membership_editor_module_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidPage(
      fluidRow(
        column(
          12,
          div(style = "height: 20px;"),
          tabsetPanel(

            #
            # Panel for updating the List table
            #

            tabPanel(
              "Lists",
              column(
                12,
                div(style = "height: 20px;"),
                div(
                  style = "display: inline-block; vertical-align: top; float:right",
                  actionButton(ns("add_row_list"), "Add Row", width = 120),
                  disabled(
                    actionButton(ns("save_changes_list"), "Save Table", width = 120)
                  )
                ),
                br(),
                br(),
                br()
              ),
              column(
                12,
                DTOutput(ns("list_table"))
              )
            ),

            #
            # Panel for updating the Security table
            #

            tabPanel(
              "Securities",
              column(
                12,
                div(style = "height: 20px;"),
                div(
                  style = "display: inline-block; vertical-align: top; float:right",
                  actionButton(ns("add_row_security"), "Add Row", width = 120),
                  disabled(
                    actionButton(ns("save_changes_security"), "Save Table", width = 120)
                  )
                ),
                br(),
                br(),
                br()
              ),
              column(
                12,
                DTOutput(ns("security_table"))
              )
            ),

            #
            # Panel for editing the membership of lists
            #

            tabPanel(
              "Memberships",
              column(
                12,
                div(style = "height: 20px;"),
                div(
                  style = "display: inline-block; vertical-align: top;",
                  selectInput(ns("select_list_name"), NULL, "")
                ),
                div(
                  style = "display: inline-block; vertical-align: top; float:right",
                  actionButton(ns("add_row_membership"), "Add Row", width = 120),
                  disabled(
                    actionButton(ns("save_changes_membership"), "Save Table", width = 120)
                  )
                ),
                div(style = "height: 20px;"),
                div(
                  style = "color: darkred;",
                  "Note that only EndDate is editable for existing records."
                ),
                br()
              ),
              column(
                12,
                DTOutput(ns("membership_table"))
              )
            )
          )
        )
      )
    )
  )
}


#
# Server logic to populate and update the user interface
#

db_list_membership_editor_module <- function(input,
                                             output,
                                             session) {
  #
  # Path to an existing SQLite database file
  #

  database_filename <- "list_membership.db"

  #
  # Reactive data structure to back all of the editable tables
  #

  data <- reactiveValues(
    list_df = NULL,
    security_df = NULL,
    membership_df = NULL
  )


  # Lists tab ---------------------------------------------------------------

  # An editable datatable and its proxy

  output$list_table <- renderDT({
    data$list_df %>% select(-Updated, -Added)
  },
  selection = "none",
  editable = TRUE,
  rownames = FALSE,
  options = list(
    pageLength = 20,
    lengthMenu = c(10, 20, 50, 100, 500)
  ))

  proxy_list <- dataTableProxy(session$ns("list_table"))


  # Populate the list table from the database

  update_list_table <- function() {
    conn <- db_lm_connect(database_filename)
    data$list_df <- db_lm_get_lists(conn) %>%
      mutate(
        Updated = FALSE,
        Added = FALSE
      ) %>%
      as.data.frame() # tibbles not supported by editable DTs
    db_lm_disconnect(conn)
  }

  update_list_table()


  # Event handlers to allow the table and backing data structure to be modified

  observeEvent(input$list_table_cell_edit, {
    info <- input$list_table_cell_edit
    i <- info$row
    j <- info$col + 1  # + 1 since we are hiding row names
    v <- info$value
    tryCatch({
      v <- DT::coerceValue(v, data$list_df[i, j])
      data$list_df[i, j] <<- v
      data$list_df[i, "Updated"] <<- TRUE
      replaceData(proxy_list, data$list_df, resetPaging = FALSE)
      shinyjs::enable("save_changes_list")
    },
    error = function(e) {
      showModal(modalDialog(paste("ERROR: ", e)))
    }
    )
  })

  observeEvent(input$add_row_list, {
    data$list_df <<- bind_rows(
      tibble(
        ListName = "",
        ListDesc = "",
        Updated = FALSE,
        Added = TRUE
      ),
      data$list_df
    ) %>%
      as.data.frame()
    replaceData(proxy_list, data$list_df, resetPaging = FALSE)
    shinyjs::enable("save_changes_list")
  })


  # Logic to add new lists or update existing lists

  observeEvent(input$save_changes_list, {

    conn <- db_lm_connect(database_filename)
    new_df <- data$list_df %>% as_tibble()

    # Do the update
    for (i in seq_len(nrow(new_df))) {
      row <- new_df[i, ]
      result <- ""
      if ( row$Added ) {
        result <- db_lm_create_list(conn, row$ListName, row$ListDesc)
      } else if ( row$Updated ) {
        result <- db_lm_update_list(conn, row$ListName, row$ListDesc)
      }
      if ( tolower(substr(result, start = 1, stop = 3)) == "err" ) {
        showModal(modalDialog(result))
      }
    }

    db_lm_disconnect(conn)

    shinyjs::disable("save_changes_list")

  })


  # Security tab ------------------------------------------------------------

  # An editable datatable and its proxy

  output$security_table <- renderDT({
    data$security_df %>% select(-Updated, -Added)
  },
  selection = "none",
  editable = TRUE,
  rownames = FALSE,
  options = list(
    pageLength = 20,
    lengthMenu = c(10, 20, 50, 100, 500)
  ))

  proxy_security <- dataTableProxy(session$ns("security_table"))

  # Populate the security table from the database

  update_security_table <- function() {
    conn <- db_lm_connect(database_filename)
    data$security_df <- db_lm_get_securities(conn) %>%
      mutate(
        Updated = FALSE,
        Added = FALSE
      ) %>%
      as.data.frame() # tibbles not supported by editable DTs
    db_lm_disconnect(conn)
  }

  update_security_table()


  # Event handlers to allow the table and backing data structure to be modified

  observeEvent(input$security_table_cell_edit, {
    info <- input$security_table_cell_edit
    i <- info$row
    j <- info$col + 1  # + 1 since we are hiding row names
    v <- info$value
    tryCatch({
      v <- DT::coerceValue(v, data$security_df[i, j])
      data$security_df[i, j] <<- v
      data$security_df[i, "Updated"] <<- TRUE
      replaceData(proxy_security, data$security_df, resetPaging = FALSE)
      shinyjs::enable("save_changes_security")
    },
    error = function(e) {
      showModal(modalDialog(paste("ERROR: ", e)))
    }
    )
  })

  observeEvent(input$add_row_security, {
    data$security_df <<- bind_rows(
      tibble(
        ISIN = "",
        SecurityDes = "",
        MaturityDate = as.Date("1970-01-01"),
        Updated = FALSE,
        Added = TRUE
      ),
      data$security_df
    ) %>%
      as.data.frame()
    replaceData(proxy_security, data$security_df, resetPaging = FALSE)
    shinyjs::enable("save_changes_security")
  })


  # Logic to add new lists or update existing lists

  observeEvent(input$save_changes_security, {

    conn <- db_lm_connect(database_filename)
    new_df <- data$security_df %>% as_tibble()

    # Do the update
    for (i in seq_len(nrow(new_df))) {
      row <- new_df[i, ]
      result <- ""
      if ( row$Added ) {
        result <- db_lm_create_security(conn, row$ISIN, row$SecurityDes, row$MaturityDate)
      } else if ( row$Updated ) {
        result <- db_lm_update_security(conn, row$ISIN, row$SecurityDes, row$MaturityDate)
      }
      if ( tolower(substr(result, start = 1, stop = 3)) == "err" ) {
        showModal(modalDialog(result))
      }
    }

    db_lm_disconnect(conn)

    shinyjs::disable("save_changes_security")

  })


  # Membership tab ----------------------------------------------------------


  # Update the database list name selector at startup and when new records are added

  update_list_selector <- function(selected = NULL) {
    conn <- db_lm_connect(database_filename)
    available_lists <- db_lm_get_lists(conn)$ListName
    db_lm_disconnect(conn)
    updateSelectInput(session,
                      inputId = "select_list_name",
                      choices = available_lists,
                      selected = selected)
  }

  update_list_selector()


  # Refresh the table if a new list name is selected

  observeEvent(input$select_list_name, {
    conn <- db_lm_connect(database_filename)
    data$membership_df <- db_lm_get_membership(
      conn,
      list_name = input$select_list_name
    ) %>%
      mutate(
        Updated = FALSE,
        Added = FALSE
      ) %>%
      as.data.frame() # tibbles not supported by editable DTs
    db_lm_disconnect(conn)
  }, ignoreInit = TRUE)


  # An editable datatable and its proxy

  output$membership_table <- renderDT({
    data$membership_df %>% select(-Updated, -Added)
  },
  selection = "none",
  # TODO: In future version the editable parameter will accept a list
  #       to enable us to edit only certain content. When this functionality
  #       is rolled out we should disable the editing of SecurityDes and
  #       maturity date since they are features of the security DB entry.
  editable = TRUE,
  rownames = FALSE,
  options = list(
    pageLength = 20,
    lengthMenu = c(10, 20, 50, 100, 500)
  ))

  proxy_membership <- dataTableProxy(session$ns("membership_table"))


  # Event handlers to allow the table and backing data structure to be modified

  observeEvent(input$membership_table_cell_edit, {
    info <- input$membership_table_cell_edit
    i <- info$row
    j <- info$col + 1  # + 1 since we are hiding row names
    v <- info$value
    tryCatch({
      v <- DT::coerceValue(v, data$membership_df[i, j])
      data$membership_df[i, j] <<- v
      data$membership_df[i, "Updated"] <<- TRUE
      replaceData(proxy_membership, data$membership_df, resetPaging = FALSE)
      shinyjs::enable("save_changes_membership")
    },
    error = function(e) {
        showModal(modalDialog(paste("ERROR: ", e)))
      }
    )
  })

  observeEvent(input$add_row_membership, {
    data$membership_df <<- bind_rows(
      tibble(
        ListName = isolate(input$select_list_name),
        ISIN = "",
        SecurityDes = "",
        MaturityDate = as.Date("1970-01-01"),
        StartDate = as.Date("1970-01-01"),
        EndDate = as.Date("1970-01-01"),
        Updated = FALSE,
        Added = TRUE
      ),
      data$membership_df
    ) %>%
      as.data.frame()
    replaceData(proxy_membership, data$membership_df, resetPaging = FALSE)
    shinyjs::enable("save_changes_membership")
  })


  observeEvent(input$save_changes_membership, {

    conn <- db_lm_connect(database_filename)
    new_df <- data$membership_df %>% as_tibble()

    # Do the update
    for (i in seq_len(nrow(new_df))) {
      row <- new_df[i, ]
      result <- ""
      if ( row$Added ) {
        result <- db_lm_create_membership(
          conn, row$ListName, row$ISIN, row$StartDate, row$EndDate
        )
        data$membership_df[i, "Added"] <<- FALSE
      } else if ( row$Updated ) {
        result <- db_lm_update_membership(
          conn, row$ListName, row$ISIN, row$StartDate, row$EndDate
        )
        data$membership_df[i, "Updated"] <<- FALSE
      }
      if ( tolower(substr(result, start = 1, stop = 3)) == "err" ) {
        showModal(modalDialog(result))
      }
    }

    db_lm_disconnect(conn)

    shinyjs::disable("save_changes_membership")

  })


}
