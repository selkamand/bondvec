#' input_chemical_structure UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_input_chemical_structure_ui <- function(id, label = "Chemical Structure (mol2)", width = "100%") {
  ns <- NS(id)
  tagList(
    fileInput(inputId = ns("in_file_mol2"), label = label, multiple = FALSE, width = width)
  )
}

#' input_chemical_structure Server Functions
#'
#' @noRd
mod_input_chemical_structure_server <- function(id, msg = "Please select a mol2 file"){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    mol2 <- reactive({
      req(input$in_file_mol2)
      path <- input$in_file_mol2$datapath
      validate(need(path, message = msg))
      bio3d::read.mol2(path)
    })
    return(mol2)
  })
}

## To be copied in the UI
# mod_input_chemical_structure_ui("input_chemical_structure_1")

## To be copied in the server
# mod_input_chemical_structure_server("input_chemical_structure_1")
