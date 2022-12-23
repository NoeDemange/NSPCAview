#' nspca UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import nsprcomp
#' @import factoextra
#' @import ComplexHeatmap
#' @import viridis
#' @import DendSer
#' @import dendextend
#' @importFrom shiny NS tagList
mod_nspca_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      box(title = "NSPCA", status = "primary", solidHeader = TRUE, collapsible = TRUE,
          helpText(h3("Setting NSPCA")),
          numericInput(ns("nb_comp"), "Nombre de composante :", value = 10, min = 0),
          actionButton(ns("val_a1"), "valider"),
          helpText(h3("Visualisation contribution individus")),
          numericInput(ns("nb_cont_ind_plot"),
            "Nombre d'individus n que l'on veut observer (de la plus grande contribution à n) :",
            value = 0, min = 0),
          actionButton(ns("val_a2"), "valider"),
          helpText(h3("Plot contribution individus")),
          shinycssloaders::withSpinner(plotOutput(ns("hist_ind"), height = "600px")),
          downloadButton(ns("down_hist_ind"), label = "Download the plot", style="color:#000000; display: block"),
          ###Ajout de possibilité de charger des données textes
          width=12
      ),
      box(title = "Heatmap", status = "primary", solidHeader = TRUE, collapsible = TRUE,
          helpText(h3("Matrice")),
          numericInput(ns("nb_cont_ind_mat"), "Nombre d'individus n que l'on veut observer (de la plus grande contribution à n) :", value = 50, min = 0),
          textInput(ns("ind_retir"), "Numéro des individus à retirer (Format: 1,10,12... )"),

###Ajouter tous les choix pour distance et tout  et pour heatmap et possibilité charger la matrice qu'on crée



          shinycssloaders::withSpinner(plotOutput(ns("ht_simple"), height = "600px")),
          downloadButton(ns("down"), label = "Download the plot", style="color:#000000; display: block"),
          width=12
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

    nspca <- eventReactive(input$val_a1,{
      req(r$df())
      #notification
      id <- showNotification("Running nspca... Wait", duration = NULL, closeButton = FALSE, type = "warning")
      on.exit(removeNotification(id), add = TRUE)
      #pca
      NSPCA <- nsprcomp(r$df(), ncomp =input$nb_comp, nneg=TRUE, scale.=TRUE)
    })

    #met à jour le numeric input
    observeEvent(nspca(),{
      nb <- nrow(nspca()$q)
      updateNumericInput(inputId = "nb_cont_ind_plot", max = nb, value = ifelse(nb<32, 1, nb%/%32))
    })

    matnspca_o <- reactive({
      MatNSPCA <- as.matrix(nspca()$x)
      MatNSPCAord <- MatNSPCA[order(rowSums(MatNSPCA),decreasing=T),]
    })

    plot_hist_ind <- eventReactive(input$val_a2,{
      fviz_contrib(nspca(), choice="ind", top = input$nb_cont_ind_plot)
    })

    ##output
    output$hist_ind <- renderPlot({
      req(plot_hist_ind)
      plot_hist_ind()
    })

    output$down_hist_ind <- downloadHandler(
      filename =  function() {
        paste0("Contribution des ",input$nb_cont_ind_plot," individus.pdf")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        grDevices::pdf(file) # open the pdf device
        plot(plot_hist_ind())
        grDevices::dev.off()  # turn the device off
      })


  })
}

## To be copied in the UI
# mod_nspca_ui("nspca_1")

## To be copied in the server
# mod_nspca_server("nspca_1")
