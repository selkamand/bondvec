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

  # TODO: Visualise dummy position on rgl plot (Note could be NAN
  computed_dummy_atom_position <- reactive({
#
#     validate(need(atoms, message = "Please supply atoms"))
#     validate(need(atom_id_a, message = "Please supply atom_id_a"))
#     validate(need(atom_id_b, message = "Please supply atom_id_b"))
#     validate(need(atom_id_c, message = "Please supply atom_id_c"))
#     validate(need(atom_id_d, message = "Please supply atom_id_d"))


    d <- as.numeric(atoms()[atoms()$eleno == atom_id_d(), c("x", "y", "z")])
    c <- as.numeric(atoms()[atoms()$eleno == atom_id_c(), c("x", "y", "z")])
    b <- as.numeric(atoms()[atoms()$eleno == atom_id_b(), c("x", "y", "z")])
    a <- as.numeric(atoms()[atoms()$eleno == atom_id_a(), c("x", "y", "z")])

    stats <- tryCatch({move::compute_abcd_dihedral_stats(a = a, b = b, c = c, d=d)}, error = function(err){NULL})
    if(is.null(stats)) return(NULL)
    d_position <- move::locate_fourth_atom_position(
      a = a, b=b, c=c,
      bond_angle=stats$bond_angle,
      bond_length = stats$bond_length,
      torsion_angle = stats$torsion_angle,
      return_bond_vector = FALSE
    )

    if(anyNA(d_position)) return(NULL)
    return(d_position)
  })

  output$out_dpos <- renderPrint({computed_dummy_atom_position()})

  #

  # observeEvent(compute_dummy_atom_position(), isolate({
  #   draw_or_move_dummy(pos = compute_dummy_atom_position(), radius = 5, color = "purple")
  # }))

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

  # Whenever anything about the plot changes,
  # Request rgl to stash the current usermatrix to input$par3d$userMatrix
  # It may take a moment for this to update
  observeEvent(list(atom_id_d(), bonds(), label_column(), atom_id_d(), computed_dummy_atom_position(), input$in_sel_labels), {
    rgl::shinyGetPar3d("userMatrix", session = session)
    # message("Updating rgl")
    }, priority = 100)

  # Rendering ---------------------------------------------------------------
  # Render Plot whenever input$par3d$userMatrix changes (which happens after any atom specification changes)
  output$out_rgl_structure <- rgl::renderRglwidget(expr = {
      # (Re)draw the scene only when either:
      # 1. new mol2 file is parsed, or
      # 2) userMatrix is requested - which in turn happens whenever any setting changes
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
        axes = FALSE,
        userMatrix = input$par3d$userMatrix
      )

      dummy_pos <- computed_dummy_atom_position()
      if(!is.null(dummy_pos) && !anyNA(dummy_pos)) {
        message("rendering")
        rgl::spheres3d(x = dummy_pos[1], y = dummy_pos[2], z = dummy_pos[3], radius = 0.1, color = "purple")
      }

      # 4) capture to widget
      rgl::rglwidget()
  }) |> shiny::bindEvent(list(input$par3d$userMatrix, mol2()), ignoreNULL = FALSE)

  # Render bond and atom tables
  output$out_dt_atoms <- DT::renderDataTable({atoms()})
  output$out_dt_bonds <- DT::renderDataTable({bonds()})
}
#
# draw_or_move_dummy <- function(pos, radius = 5, color = "red") {
#
#   if(is.null(pos)) return(NULL)
#   # Remove any previous dummy spheres by tag (safe even if none exist)
#   rgl::pop3d(type = "shapes", tag = "dummy")
#
#   # Add the new one if pos is valid
#   if (is.numeric(pos) && length(pos) == 3 && all(is.finite(pos))) {
#     rgl::spheres3d(
#       x = pos[1], y = pos[2], z = pos[3],
#       radius = radius,
#       color  = color,
#       tag    = "dummy"
#     )
#   }
# }

