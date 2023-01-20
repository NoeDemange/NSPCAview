#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
options(shiny.maxRequestSize = 30 * 1024 ^ 2)
app_server <- function(input, output, session) {
  # Your application server logic
  r <- reactiveValues()
  mod_data_loading_server("data_loading_1",r=r) #Dans r r$df
  mod_nspca_server("nspca_1",r=r)
  mod_information_server("information_1")
}
