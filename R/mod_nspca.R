#' nspca UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_nspca_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      box(title = "NSPCA", status = "primary", solidHeader = TRUE, collapsible = TRUE,
          helpText(h3("Setting")),
          numericInput(ns("nb_comp"), "Nombre de composante :", value = 10, min = 0),
          helpText(h3("Heatmap")),
          selectInput(ns("color"),"Heatmap color",c("magma","inferno","plasma","viridis",
                                                    "cividis","rocket","mako","turbo"),selected="magma"),
          selectInput(ns("bg_color"),"Background color",c("white","black"),selected="white"),
          textInput(ns("legend_name"),"Enter a legend name",value = "legendname"),
          actionButton(ns("val_a1"), "valider"),
          width=12
      ),
      box(title = "Heatmap", status = "primary", solidHeader = TRUE, collapsible = FALSE,
          shinycssloaders::withSpinner(plotOutput(ns("ht_simple"), height = "600px")),
          downloadButton(ns("down"), label = "Download the plot", style="color:#000000; display: block"),
          width=10
      )
    )

  )
}

#' nspca Server Functions
#'
#' @noRd
mod_nspca_server <- function(id,r=r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_nspca_ui("nspca_1")

## To be copied in the server
# mod_nspca_server("nspca_1")
