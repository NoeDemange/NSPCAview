#' data_loading UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_data_loading_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      box(title = "Data",status = "primary",solidHeader = TRUE,
          helpText(h3("Dataset")),
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
          column(6,
                 fileInput(ns("file"), "Import", accept = ".csv"),
          ),
          column(6,
            radioButtons(ns("sep"),"csv separator",choices = c(Comma = ",",Semicolon = ";",Tab = "\t"),
                       selected = ",",inline = TRUE),
          ),
          column(12,
            actionButton(ns("val_d1"), "valider"),
          ),
          column(12,
            helpText(h3("Data processing")),
          ),
          column(12,
                 radioButtons(ns("trans"),"Transposition",
                              choiceNames = c("Yes","No"),
                              choiceValues = c(TRUE,FALSE),
                              selected = FALSE,inline = TRUE),
          ),
          column(12,
                 helpText(h4("Remove non-informative lines or columns")),
          ),
          column(6,
                 numericInput(ns("max_zero_row"), "Maximum number of zero by row", value = 10, min = 0),
          ),
          column(6,
                 numericInput(ns("max_zero_col"), "Maximum number of zero by column", value = 10, min = 0),
          ),
          column(12,
                 actionButton(ns("val_d2"), "valider"),
          ),
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
    data_brut <- eventReactive(input$val_d1,{
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

    data <- reactive({
      req(data_brut)
      datamat <- data_brut()
      if(input$trans){
        datamat <- t(datamat)
      }
      return(datamat)
    })

    observeEvent(data(),{
      updateNumericInput(inputId = "max_zero_col", max = nrow(data()), min = 0, value = nrow(data()))
      updateNumericInput(inputId = "max_zero_row", max = ncol(data()), min = 0, value = ncol(data()))
    })

    r$df <- eventReactive(input$val_d2,{
      req(data)
      datamat <- data()
      datamat <- datamat[rowSums(datamat==0)<=input$max_zero_row,]
      datamat <- datamat[,colSums(datamat==0)<=input$max_zero_col]
      return(datamat)
    })

  })
}

## To be copied in the UI
# mod_data_loading_ui("data_loading_1")

## To be copied in the server
# mod_data_loading_server("data_loading_1")
