#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  options(rgl.useNULL = TRUE)

  # Get bio3d mol2 object
  mol2 <- mod_input_chemical_structure_server(id = "mod_fetch_chemstructure")
  atoms <- reactive({mol2()$atom})
  bonds <- reactive({mol2()$bond})

  # Get Atom Selections
  atom_id_d <- mod_select_atom_server(id = "mod_select_atom_d", atoms)
  atom_id_c <- mod_select_atom_server(id = "mod_select_atom_c", atoms)
  atom_id_b <- mod_select_atom_server(id = "mod_select_atom_b", atoms)
  atom_id_a <- mod_select_atom_server(id = "mod_select_atom_a", atoms)


  # Rendering ---------------------------------------------------------------
  # Render Plot
  output$out_rgl_structure <- rgl::renderRglwidget(expr = {
    chemviewR::plotrgl(
      atoms = atoms(),
      bonds = bonds(),
      colour_map_atom = chemviewR::element_colours,
      highlight = atom_id_d(),
      highlight_colour = "green",
      grid = FALSE,
      axes = FALSE
    )
    rgl::rglwidget()
  })

  # Render bond and atom tables
  output$out_dt_atoms <- DT::renderDataTable({atoms()})
  output$out_dt_bonds <- DT::renderDataTable({bonds()})

}
