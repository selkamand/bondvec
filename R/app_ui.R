#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      title = "Calculate Bond Vectors",
      theme = bslib::bs_theme(bootswatch = "minty"),
      sidebar = bslib::sidebar(
        width = "400px",
        mod_input_chemical_structure_ui(id = "mod_fetch_chemstructure", label = "Chemical Structure (mol2)", width = "100%"),
        mod_select_atom_ui(id = "mod_select_atom_d", label = "Select atom D (dummy)"),
        mod_select_atom_ui(id = "mod_select_atom_c", label = "Select atom C"),
        mod_select_atom_ui(id = "mod_select_atom_b", label = "Select atom B"),
        mod_select_atom_ui(id = "mod_select_atom_a", label = "Select atom A"),
        verbatimTextOutput(outputId = "brushinfo")
      ),
      bslib::nav_panel(
        title = "Compute Bond Vector",
        bslib::card(
          bslib::card_header("Structure Viewer"),
          rgl::rglwidgetOutput(outputId = "out_rgl_structure", width = "100%")
        )
      ),
      bslib::nav_panel(
        title = "Underlying Data",
        bslib::card(
          bslib::card_header("Atoms"),
          DT::dataTableOutput(outputId = "out_dt_atoms"),
        ),
        bslib::card(
          bslib::card_header("Bonds"),
          DT::dataTableOutput(outputId = "out_dt_bonds"),
        )
      )

    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "bondvec"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
