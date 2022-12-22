#' data_loading UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @useDynLib nspcaview, .registration = TRUE
#' @importFrom shiny NS tagList
mod_data_loading_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      box(title = "Data",status = "primary",solidHeader = TRUE,
          helpText(
            "Choisissez si vous voulez utiliser le dataset demo ou importer votre dataset (format .csv avce Header et nom des lignes en premiere colonne).
            Puis appuyez sur valider"
          ),
          radioButtons(ns("data"),"",choices = c(
            "demo (eaux_forets)",
            "demo (botanique)",
            "Your Dataset (.csv)"),
            selected = "demo (eaux_forets)",inline = TRUE),
          br(),
          fileInput(ns("file"), "Import", accept = ".csv"),
          radioButtons(ns("sep"),"csv separator",choices = c(Comma = ",",Semicolon = ";",Tab = "\t"),
                       selected = ","),
          actionButton(ns("val"), "valider"),
          width = 12
      )
    )
  )
}

#' data_loading Server Functions
#'
#' @noRd
mod_data_loading_server <- function(id,r=r){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    r$df <- eventReactive(input$val,{
      if(input$data == "demo (eaux_forets)"){
        datf <- nspcaview::demo_eaux_foret
        return(datf)
      }else if(input$data == "demo (botanique)"){
        datf <- nspcaview::demo_botanique
        return(datf)
      }else if(input$data == "Your Dataset (.csv)"){
        req(input$file)
        if(tools::file_ext(input$file$name)=="csv"){
          datf <- utils::read.csv(input$file$datapath,
                                  header = TRUE,
                                  sep = input$sep,
                                  row.names =1
          )
          return(datf)
        }else{
          stop("Ce n'est pas un .csv")
        }
      }
    })
  })
}

## To be copied in the UI
# mod_data_loading_ui("data_loading_1")

## To be copied in the server
# mod_data_loading_server("data_loading_1")
