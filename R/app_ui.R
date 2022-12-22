#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinydashboard
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    ui <- shinydashboard::dashboardPage(
      skin = "black",
      dashboardHeader(title = "NSPCAview"),
      dashboardSidebar(
        sidebarMenu(
          menuItem("Dataset", tabName = "dataset", icon = icon("fas fa-file-arrow-down")),
          menuItem("NSPCA", tabName = "nspca", icon = icon("fas fa-bar-chart")),
          style = "font-size:18px"
        )
      ),
      dashboardBody(
        fluidRow(
          tabItems(
            tabItem(tabName="dataset",
                    mod_data_loading_ui("data_loading_1")
            ),
            tabItem(tabName="nspca",
                    mod_nspca_ui("nspca_1")
            )
          )
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
      app_title = "nspcaview"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
