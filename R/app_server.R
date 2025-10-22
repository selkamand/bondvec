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
  atoms <- reactive({
    df <- mol2()$atom
    df$elena_eleno <- paste0(df$elena, "(", df$eleno, ")")
    return(df)
  })

  bonds <- reactive({mol2()$bond})

  # Get Atom Selections
  atom_id_d <- mod_select_atom_server(id = "mod_select_atom_d", atoms)
  atom_id_c <- mod_select_atom_server(id = "mod_select_atom_c", atoms)
  atom_id_b <- mod_select_atom_server(id = "mod_select_atom_b", atoms)
  atom_id_a <- mod_select_atom_server(id = "mod_select_atom_a", atoms)

  # Define label column
  label_column <- reactive({
    sel_label = input$in_sel_label_column
    if(sel_label == "name") return("elena")
    else if(sel_label == "id") return("eleno")
    else if (sel_label == "name_and_id") return("elena_eleno")
    else return("elena")
  })


  # keep the last known matrix (start with identity)
  lastUM <- shiny::reactiveVal(diag(4))


  # Rendering ---------------------------------------------------------------
  # Render Plot
  output$out_rgl_structure <- rgl::renderRglwidget(expr = {
      # 1) pull the current matrix from the *previous* widget, if available
      # um <- rgl::shinyGetPar3d("userMatrix", session = session)
      # um <- rgl::shinyGetPar3d("userMatrix", session = session)
      #
      # if (!is.null(um)) lastUM(um)

      # 2) (Re)draw the scene
      chemviewR::plotrgl(
        atoms = atoms(),
        bonds = bonds(),
        colour_map_atom = chemviewR::element_colours,
        highlight = atom_id_d(),
        highlight_colour = "green",
        label_mode = input$in_sel_labels,
        col_label = label_column(),
        atom_alpha_when_labelled = 0.2,
        label_colour = NULL,
        grid = FALSE,
        axes = FALSE #,
        # userMatrix = lastUM()   # 3) apply AFTER drawing via par3d()
      )

      # 4) capture to widget
      rgl::rglwidget()

    # if(!is.null(current_userMatrix)) {rgl::par3d(userMatrix = current_userMatrix)}
    # if(!is.null(current_userMatrix)) {rgl::shinySetPar3d(userMatrix = current_userMatrix, session = session)}
  })

  # Render bond and atom tables
  output$out_dt_atoms <- DT::renderDataTable({atoms()})
  output$out_dt_bonds <- DT::renderDataTable({bonds()})
}
