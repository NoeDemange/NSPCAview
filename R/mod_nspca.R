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
#' @import grid
#' @import cluster
#' @importFrom utils write.csv
#' @importFrom dendextend color_branches
#' @importFrom shiny NS tagList
mod_nspca_ui <- function(id){
  ns <- NS(id)
  tagList(
    fluidPage(
      box(title = "NSPCA", status = "primary", solidHeader = TRUE, collapsible = TRUE,
          ###Setting NSPCA###
          helpText(h3("Setting NSPCA")),
          column(4,
            numericInput(ns("nb_comp"), "Nombre de composante :", value = 10, min = 0),
          ),
          column(4,
                 radioButtons(ns("scale"),"Scale",
                              choiceNames = c("True","False"),
                              choiceValues = c(TRUE,FALSE),
                              selected = TRUE,inline = TRUE),
          ),
          column(4,
                 radioButtons(ns("center"),"Center",
                              choiceNames = c("True","False"),
                              choiceValues = c(TRUE,FALSE),
                              selected = FALSE,inline = TRUE),
          ),
          column(12,
            actionButton(ns("val_a1"), "valider"),
          ),
          ###Visualisation contribution individus###
          helpText(h3("Visualisation contribution individus")),
          column(6,
            numericInput(ns("nb_cont_ind_plot"),
            "Nombre d'individus n que l'on veut observer (de la plus grande contribution à n) :",
            value = 0, min = 0),
          ),
          column(6,
            numericInput(ns("plot_ind_x_font_size"), "Size of label", value = 10, min = 0, max = 100),
          ),
          actionButton(ns("val_a2"), "valider"),
          ###Plot contribution individus###
          helpText(h3("Plot contribution individus")),
          shinycssloaders::withSpinner(plotOutput(ns("hist_ind"), height = "600px")),
          downloadButton(ns("down_hist_ind"), label = "Download the plot", style="color:#000000; display: block"),
          downloadButton(ns("down_histInd"), label = "Download data", style="color:#000000; display: block"),
          ###Ajout de possibilité de charger des donnees textes
          width=12
      ),
      box(title = "Heatmap", status = "primary", solidHeader = TRUE, collapsible = TRUE,
          helpText(h3("Matrice")),
          column(6,
            numericInput(ns("nb_cont_ind_mat"), "Nombre d'individus n que l'on veut observer (de la plus grande contribution à n) :", value = 0, min = 0),
          ),
          column(6,
            textInput(ns("ind_retir"), "Numero des individus a retirer (Format: 1,10,12... )"),
          ),
          column(12,
            downloadButton(ns("down_data"), label = "Download the contribution matrix", style="color:#000000; display: block"),
          ),
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
                              value = 2, min = 0),
          ),
          column(6,
                 numericInput(ns("C"),
                              "Indice de contraste :",
                              value = 2, min = 0),
          ),
          column(6,
                 numericInput(ns("heatmap_fontsize_col"), "Column fontsize", value = 6.5, min = 0, max = 100, step = 0.1),
          ),
          column(6,
                 numericInput(ns("heatmap_fontsize_row"), "Row fontsize", value = 4, min = 0, max = 100, step = 0.1),
          ),
          column(6,
                 textInput(ns("legend_name"),"Entrez un nom de legende",value = "legendname"),
          ),
          column(6,
                 selectInput(ns("color"),"Heatmap color",c("magma","inferno","plasma","viridis",
                                                           "cividis","rocket","mako","turbo"),selected="magma")
          ),
          actionButton(ns("val_a3"), "valider"),
          helpText(h3("Heatmap")),
          shinycssloaders::withSpinner(plotOutput(ns("ht_simple"), height = "600px")),
          downloadButton(ns("down"), label = "Download the plot", style="color:#000000; display: block"),
          width=12
      ),

      box(title = "Visualisation contributions aux axes", status = "primary", solidHeader = TRUE, collapsible = TRUE,
          numericInput(ns("nb_cont_var_plot"),
                       "Nombre de variables n que l'on veut observer (de la plus grande contribution a n) :",
                       value = 0, min = 0),
          column(6,
            textInput(ns("axes"), "Composantes principales a visualiser (Format: 1,10,12... )", value = "1,2,3,4"),
          ),
          column(6,
            numericInput(ns("plot_var_x_font_size"), "Size of label", value = 10, min = 0, max = 100),
          ),
          actionButton(ns("val_a4"), "valider"),
          helpText(h3("Plot contribution variables")),
          shinycssloaders::withSpinner(plotOutput(ns("hist_var"), height = "600px")),
          downloadButton(ns("down_hist_var"), label = "Download the plot", style="color:#000000; display: block"),
          downloadButton(ns("down_histVar"), label = "Download data", style="color:#000000; display: block"),
          ###Ajout de possibilité de charger des données textes
          width=12
      ),
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
      NSPCA <- nsprcomp(r$df(), ncomp =input$nb_comp, nneg=TRUE, scale.= as.logical(input$scale), center=as.logical(input$center))
    })

    #met à jour le numeric input
    observeEvent(nspca(),{
      nb_x <- nrow(nspca()$x)
      nb_r <- nrow(nspca()$rotation)
      updateNumericInput(inputId = "nb_cont_ind_plot", max = nb_x, value = nb_x)
      updateNumericInput(inputId = "nb_cont_ind_mat", max = nb_x, value = nb_x)
      updateNumericInput(inputId = "nb_cont_var_plot", max = nb_r, value = nb_r)
    })

    plot_hist_ind <- eventReactive(input$val_a2,{
      fviz_contrib(nspca(), choice="ind", top = input$nb_cont_ind_plot, axes=seq(input$nb_comp), font.xtickslab=input$plot_ind_x_font_size)
    })

    axe <- reactive({
      sp_st <- strsplit(input$axes,",")
      sp_i <- as.numeric(unlist(sp_st))
      return(sp_i)
    })

    plot_hist_var <- eventReactive(input$val_a4,{
      fviz_contrib(nspca(), choice="var", top = input$nb_cont_var_plot, axes=axe(), font.xtickslab=input$plot_var_x_font_size)
    })


    matnspca_o <- reactive({
      dd <- facto_summarize(nspca(), element = "ind", result = "contrib",
                            axes = seq(input$nb_comp))
      contrib <- dd$contrib
      names(contrib) <- rownames(dd)
      MatNSPCA <- as.matrix(nspca()$x)
      MatNSPCAord <- MatNSPCA[order(contrib,decreasing = T),]
    })

    ###Heatmap
    observeEvent(nspca(),{
      updateNumericInput(inputId = "K_sp", max = input$nb_comp, min = 0, value = input$nb_comp/2)
    })

    ret_ind <- reactive({
      sp_st <- strsplit(input$ind_retir,",")
      sp_i <- as.numeric(unlist(sp_st))
    })

    vec_mat <- reactive({
        v <- seq(1,input$nb_cont_ind_mat)
        vec <- v[! v %in% ret_ind()]
        return(vec)
    })

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
      return(mat)
    })

    #distance ligne
    distm_ml <- reactive({
      dMat <- stats::dist(datamat(), method = input$inDist)
    })

    #distance colonne
    distm_mc <- reactive({
      TdMat <- stats::dist(t(as.matrix(datamat())), method = input$inDist)
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
                                row_names_gp = gpar(fontsize = input$heatmap_fontsize_row),
                                column_split = input$K_sp , column_title_gp = gpar(col = c(1:input$K_sp), font = 1/input$K_sp),
                                column_names_gp = gpar(col = c(1:input$K_sp), fontsize = input$heatmap_fontsize_col))

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

    output$hist_var <- renderPlot({
      req(plot_hist_var)
      plot_hist_var()
    })

    output$down_hist_var <- downloadHandler(
      filename =  function() {
        paste0("Contribution des ",input$nb_cont_var_plot," individus.pdf")
      },
      # content is a function with argument file. content writes the plot to the device
      content = function(file) {
        grDevices::pdf(file) # open the pdf device
        plot(plot_hist_var())
        grDevices::dev.off()  # turn the device off
      })

    output$down_data <- downloadHandler(
      filename = function() {
        paste0(input$nb_cont_ind_mat," ind without ", ret_ind() , ".csv")
      },
      content = function(file) {
        write.csv(datamat(), file)
      }
    )

    output$down_histInd <- downloadHandler(
      filename = function() {
        paste0("Hist_Ind_", input$nb_cont_ind_plot , ".csv")
      },
      content = function(file) {
        #write.csv(matnspca_o()[seq(input$nb_cont_ind_plot),], file)
        dd <- facto_summarize(nspca(), element = "ind", result = "contrib",
                              axes = seq(input$nb_comp))
        contrib <- dd$contrib
        names(contrib) <- rownames(dd)
        contrib <- contrib[order(contrib,decreasing = T)]
        write.csv(contrib[seq(input$nb_cont_ind_plot)],file)
      }
    )

    output$down_histVar <- downloadHandler(
      filename = function() {
        paste0("Hist_Var_", input$nb_cont_var_plot,"_PC_",input$axes, ".csv")
      },
      content = function(file) {
        # MatNSPCAr <- as.matrix(nspca()$rotation)
        # MatNSPCAra <- MatNSPCAr[,axe()]
        # MatNSPCAraord <- MatNSPCAra[order(rowSums(MatNSPCAra),decreasing=T),]
        # write.csv(MatNSPCAraord[seq(input$nb_cont_var_plot),], file)
        dd <- facto_summarize(nspca(), element = "var", result = "contrib",
                              axes = axe())
        contrib <- dd$contrib
        names(contrib) <- rownames(dd)
        contrib <- contrib[order(contrib,decreasing = T)]
        write.csv(contrib[seq(input$nb_cont_var_plot)],file)
      }
    )

  })
}

## To be copied in the UI
# mod_nspca_ui("nspca_1")

## To be copied in the server
# mod_nspca_server("nspca_1")
