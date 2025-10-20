#' Select Atom UI Function
#'
#' @description
#' Generates the UI for selecting one or more atoms within a Shiny module.
#'
#' @param id Module namespace identifier string.
#' @param label Label text to display above the selection input.
#' @param multiple Logical; if `TRUE`, allow multiple atoms to be selected.
#'
#' @return
#' A `shiny.tag.list` containing a `selectInput()` control.
#'
#' @details
#' This function is intended to be used inside a Shiny application or module
#' to provide a dropdown menu populated with atom identifiers and names.
#' The choices are expected to be updated dynamically by the corresponding
#' server function.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_select_atom_ui <- function(id, label = "Select Atom", multiple = FALSE) {
  ns <- NS(id)
  tagList(
    selectInput(inputId = ns("in_sel_atom"), label = label, choices = NULL, multiple = multiple)
  )
}

#' select_atom Server Functions
#'
#' @noRd
#' @param atoms a data.frame with eleno and elena columns (1 row per atom)
#' @return a reactive vector of selected element numbers (eleno)
mod_select_atom_server <- function(id, atoms){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    assertions::assert_reactive(atoms)

    observeEvent(atoms(), {
      req(is.data.frame(atoms()), nrow(atoms()) > 0)

      # Validate required columns
      req(all(c("eleno", "elena") %in% names(atoms())),
          message = "atoms() must have columns 'eleno' and 'elena'")

      # Build named choices: names shown, values submitted
      # (values must be character for selectInput)
      vals   <- as.character(atoms()[["eleno"]])
      labs   <- paste0(atoms()[["elena"]], ": ", atoms()[["eleno"]])
      choices <- stats::setNames(vals, labs)

      # Preserve selection if still valid, otherwise choose first
      current <- isolate(input$in_sel_atom)
      selected <- if (!is.null(current) && current %in% vals) current else vals[[1]]

      updateSelectInput(session, "in_sel_atom", choices = choices, selected = selected)
    })

    selected_atom <- reactive({input$in_sel_atom})

    return(selected_atom)

  })
}

## To be copied in the UI
# mod_select_atom_ui("select_atom_1")

## To be copied in the server
# mod_select_atom_server("select_atom_1")
