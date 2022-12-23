#' nspca UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import nsprcomp
#' @import seriation
#' @import stats
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
          numericInput(ns("nb_cont_ind_mat"), "Nombre d'individus n que l'on veut observer (de la plus grande contribution à n) :", value = 0, min = 0),
          textInput(ns("ind_retir"), "Numéro des individus à retirer (Format: 1,10,12... )"),
          helpText(h3("Traitement")),
          column(4,
                 selectInput(ns('inDist'),"Distance", c("euclidean","maximum",
                                                        "manhattan","canberra",
                                                        "binary","minkowski"), selected = "euclidean"),
          ),
          column(4,
                 selectInput(ns('inHC'),"Clustering hierarchique", c("ward.D","ward.D2",
                                                                     "single","complete",
                                                                     "average","mcquitty",
                                                                     "median","centroid","diana")),
          ),
          column(4,
                 selectInput(ns('ser'),"Seriation", choices = c("Oui","Non"), selected="Oui"),
          ),
          helpText(h3("Paramètres Heatmap")),
          column(6,
                 numericInput(ns("K_sp"),
                              "Nombre de groupes :",
                              value = 7, min = 0),
          ),
          column(6,
                 numericInput(ns("C"),
                              "Indice de contraste :",
                              value = 2, min = 0),
          ),
          column(6,
                 selectInput(ns("color"),"Heatmap color",c("magma","inferno","plasma","viridis",
                                                           "cividis","rocket","mako","turbo"),selected="magma")
          ),
          column(6,
                 textInput(ns("legend_name"),"Entrez un nom de légende",value = "legendname"),
                 ),
          column(6,
          actionButton(ns("val_a3"), "valider")
          ),
          helpText(h3("Heatmap")),
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
      nb <- nrow(nspca()$x)
      updateNumericInput(inputId = "nb_cont_ind_plot", max = nb, value = ifelse(nb<32, 1, nb%/%32))
      updateNumericInput(inputId = "nb_cont_ind_mat", max = nb, value = ifelse(nb<32, 1, nb%/%32))
    })

    plot_hist_ind <- eventReactive(input$val_a2,{
      fviz_contrib(nspca(), choice="ind", top = input$nb_cont_ind_plot)
    })

    matnspca_o <- reactive({
      MatNSPCA <- as.matrix(nspca()$x)
      MatNSPCAord <- MatNSPCA[order(rowSums(MatNSPCA),decreasing=T),]
    })

    vec_mat <- reactive({
        v <- seq(1,input$nb_cont_ind_mat)
        sp_st <- strsplit(input$ind_retir,",")
        sp_i <- as.numeric(unlist(sp_st))
        vec <- v[! v %in% sp_i]
        return(vec)
    })

    ###Heatmap

    fun_color <- reactive({
      switch(input$color,
             "magma" = viridis::magma(256),
             "inferno" = viridis::inferno(256),
             "plasma" = viridis::plasma(256),
             "viridis" = viridis::viridis(256),
             "cividis" = viridis::cividis(256),
             "rocket" = viridis::rocket(256),
             "mako" = viridis::mako(256),
             "turbo" = viridis::turbo(256)
      )
    })

    datamat <- reactive({
      mat_o <- matnspca_o()
      mat <- mat_o[vec_mat(),]
    })

    #distance ligne
    distm_ml <- reactive({
      dMat <- stats::dist(datamat(), method = input$inDist_num)
    })

    #distance colonne
    distm_mc <- reactive(
      TdMat <- stats::dist(t(as.matrix(datamat())), method = input$inDist_num)
    })

    HC_l <- reactive({
      if(input$inHC != "diana"){
        HC <- stats::hclust(distm_ml(), method= input$inHC)
      } else{
        HC <- stats::as.hclust(cluster::diana(distm_ml())) #HC avec diana du package cluster
      }
      if(input$ser=="Oui"){
        OrdSer <- DendSer::DendSer(HC, distm_ml(), cost= costARc) #calcul de la seriation avec DendSer du package DendSer
        HC <-  seriation::permute(HC, OrdSer)
      }
      return(HC)
    })

    HC_c <- reactive({
      if(input$inHC != "diana"){
        HC <- stats::hclust(distm_mc(), method= input$inHC)
      } else{
        HC <- stats::as.hclust(cluster::diana(distm_mc())) #HC avec diana du package cluster
      }
      if(input$ser=="Oui"){
        OrdSer <- DendSer::DendSer(HC, distm_mc(), cost= costARc) #calcul de la seriation avec DendSer du package DendSer
        HC <-  seriation::permute(HC, OrdSer)
      }
      return(HC)
    })

    ht <- eventReactive(input$val_a3,{
      req(datamat)
      mat <- datamat()
      colDend <- as.dendrogram(HC_c())
      colDend <- color_branches(colDend, k = input$K_sp, col = c(1:input$K_sp))
      ComplexHeatmap <- Heatmap(as.matrix((mat)^(1/input$C)), name = input$legend_name,
                                cluster_columns = colDend ,
                                cluster_rows = as.dendrogram(HC_l()),
                                show_row_dend = FALSE,
                                show_column_dend = TRUE,
                                col = fun_color(),
                                column_names_max_height = max_text_width(colnames(mat)),
                                row_names_gp = gpar(fontsize = 6.5 + 1/log10(nrow(mat))),
                                column_split = input$K_sp , column_title_gp = gpar(col = c(1:N), font = 1/N),
                                column_names_gp = gpar(col = c(1:N), fontsize = 4+ 1/log10(ncol(mat))))

      draw(ComplexHeatmap)
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

    output$ht_simple <- renderPlot({
      req(ht)
      ht()
    })

    output$down <- downloadHandler(
      filename =  function() {
        paste(input$legend_name,"pdf",sep=".")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        grDevices::pdf(file) # open the pdf device
        plot(ht())
        grDevices::dev.off()  # turn the device off
      })


  })
}

## To be copied in the UI
# mod_nspca_ui("nspca_1")

## To be copied in the server
# mod_nspca_server("nspca_1")
