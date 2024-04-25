# Imports
library(shiny)
library(ComplexHeatmap)
library(InteractiveComplexHeatmap)
library(circlize)
library(DT)
library(periscope)
library(stringr)
library(cluster)
library(dplyr)
library(bsicons)

#==============================================================================
# Define the server function
#==============================================================================

server <- function(input, output, session) {

  #==============================================================================
  # Read data
  #==============================================================================
  df <- readRDS(file.path("./data", "data.rds"))
  rownames(df) <- make.names(df$MasterKey, unique = TRUE)
  sign_key <- readRDS(file.path("./data/MasterKey.rds"))
  col_sel <- readRDS(file.path("./data/col_sel.rds"))
  min <- -Inf
  max <- Inf
  ordering <- reactiveValues(order = "Start", state = TRUE, df = df)
  
  selected_columns <- reactive({
    if(input$count){
      if(input$proteome){
        c(input$clinic, input$vitro, input$mofa, input$lfc, input$ecu, "total_count", "PROT_SLE_CTRL_CROSS_SECT")
      } else{
        c(input$clinic, input$vitro, input$mofa, input$lfc, input$ecu, "total_count")
      }
    }else{
      if(input$proteome){
        c(input$clinic, input$vitro, input$mofa, input$lfc, input$ecu,  "PROT_SLE_CTRL_CROSS_SECT")
      } else{
        c(input$clinic, input$vitro, input$mofa, input$lfc, input$ecu)
      }
    }
  })
  
  #==============================================================================
  # Render UI elements
  #==============================================================================
  
  output$geneSelector <- renderUI({
    selectInput('Gene', 'Gene', selected = "C5", choices = df$Gene, multiple = TRUE, selectize = TRUE)
  })
  
  output$clinicSelector <- renderUI({
    selectInput('clinic', label = NULL,
                col_sel$clinic, 
                selected = c("SLEDAI", "Age", "CRP", "ALB.Creatinin.mg.g"), 
                multiple=TRUE, 
                selectize=TRUE
    )
  })

  output$lfcSelector <- renderUI({
    selectInput('lfc', 
                label = NULL,
                col_sel$lfc, 
                selected = c("SLE_CTRL_CROSS_SECTIONAL"), 
                multiple=TRUE, 
                selectize=TRUE)
  }) 
  
  output$vitroSelector <- renderUI({
    selectInput('vitro', 
                label = NULL,
                col_sel$vitro, 
                multiple=TRUE, 
                selectize=TRUE)
  })
  
  output$mofaSelector <- renderUI({
    selectInput('mofa', 
                label = NULL,
                col_sel$mofa, 
                multiple=TRUE, 
                selectize=TRUE)
  })  
  
  output$ecuSelector <- renderUI({
    selectInput("ecu", 
                label = NULL,
                choices = c(col_sel$ecu), 
                multiple = TRUE, 
                selectize = TRUE)
  })
  
  output$clusterSelector <- renderUI({
    selectInput("cluster", 
                NULL,
                choices = c(unlist(col_sel, use.names = FALSE)), 
                selected = selected_columns(), 
                multiple = TRUE, 
                selectize = TRUE)
  })
  
  #==============================================================================
  # function to create matrix based on selected columns
  #==============================================================================
  
  create_matrix <- function(cols){
    
    sub_df <- subset_df() %>% dplyr::select(all_of(cols))
    mat <- as.matrix(sub_df)
  
    return(mat)
  }
  
  #==============================================================================
  # Observe event to update select input choices dynamically
  #==============================================================================
  
  observeEvent(ignoreInit = TRUE, list(
    input$mofa, input$vitro, input$proteome, input$lfc, input$clinic, input$count),{
      updateSelectInput(session, "order", choices = c("Start", selected_columns()))
    })
  
  #==============================================================================
  # Observe event to update slider input dynamically
  #==============================================================================

observeEvent(input$Gene,{

    if(isTruthy(input$Gene)){
    min <- df %>% filter(Gene %in% input$Gene) %>%
      dplyr::select(Start) %>%
      min(na.rm = TRUE)

    max <- df %>% filter(Gene %in% input$Gene) %>%
      dplyr::select(Start) %>%
      max(na.rm = TRUE)
  } else{
    min <- df %>%
      dplyr::select(Start) %>%
      min(na.rm = TRUE)

    max <- df %>%
      dplyr::select(Start) %>%
      max(na.rm = TRUE)
  }

  updateSliderInput(session, "slider",
                    min = min -1,
                    max = max +1,
                    value = c(min, max))
})

  #==============================================================================
  # Observe event to reset the slider input
  #==============================================================================

  observeEvent(input$reset, {
    # Reset the order
    updateSliderInput(session, "slider", value = c(min, max))
  })

  observeEvent(input$clusteryn, {
    if (input$clusteryn) {
      ordering$state <- input$order_gene
      shinyjs::disable("order") # Disable the button if "None" is selected
      updateMaterialSwitch(session, "order_gene", value = FALSE)
      shinyjs::disable("order_gene") # Disable the button if "None" is selected

    } else {
      shinyjs::enable("order") # Enable the button otherwise
      shinyjs::enable("order_gene") # Enable the button otherwise
      updateMaterialSwitch(session, "order_gene", value = ordering$state)
    }
  })

  observeEvent(input$reset_cluster, {
    updateSelectInput(session, "cluster", selected = selected_columns())
  })
  

  #==============================================================================
  # Function to subset data frame based on selected genes and slider values
  #==============================================================================
  subset_df <- reactive({
    if(input$naExclude){
      df <- df %>%  filter(if_any(selected_columns(), ~ !is.na(.)))
    } 
    
    df <- df %>%
      filter(Gene %in% input$Gene & input$slider[1] <= Start & input$slider[2] >= Start)
    
    if (input$filter) {
      if(isTruthy(input$vitro)){
        df <- df %>%
          filter(if_any(all_of(input$vitro), ~ !is.na(.)))
      }
    }
    
    if(!input$order == "Start"){
      if(!input$order_gene){
        df <- df %>% arrange(Gene, desc(!!sym(input$order)))
      } else{
        df <- df %>% arrange(desc(!!sym(input$order)))
      }
    } else{
      if(input$order_gene){
        df <- df %>% arrange(Gene, !!sym(input$order))
      } else{
        df <- df %>% arrange(!!sym(input$order))
      }
    }
    
    if(isTruthy(input$count_filter)){
      df <- df %>% filter(total_count >= input$count_filter)
    }
    
    return(df) # Ensure the final df is returned
  })
  
  #==============================================================================
  #Function for Clustering
  #==============================================================================
  
  clustering <- function(x, method = "complete") {
    if(nrow(x) > 2 & ncol(x) > 1){
      dist <- cluster::daisy(x, metric = "gower")
      dist[is.na(dist)] <- max(dist, na.rm = TRUE)
      return(hclust(dist, method = method) %>% as.dendrogram())
    } else{
      return(FALSE)
    }
  }
  
  
  #==============================================================================
  # Function to create custom color scale
  #==============================================================================
  
  
  custom_color_scale <- function(m, lower, mid, upper){
    
    if(all(is.na(m))){
      return(colorRamp2(c(-1, 0, 1), c(lower, mid, upper)))
    }
    
    mea <- mean(m, na.rm =TRUE)
    
    if(min(m, na.rm =TRUE)== max(m, na.rm =TRUE)){
      if(min(m, na.rm = TRUE) <0){
        color <- colorRamp2(c(min(m, na.rm =TRUE) - 1, 0), c(lower, mid))
      } else if (min(m, na.rm =TRUE) > 0){
        color <- colorRamp2(c(0, max(m, na.rm =TRUE) + 1), c(mid, upper))
      } else if (min(m, na.rm =TRUE) == 0){
        color <- colorRamp2(c(-1, 0, 1), c(lower, upper))
      }
    }else if(min(m, na.rm =TRUE) < 0 & max(m, na.rm =TRUE) > 0){
      
      color <- colorRamp2(c(min(m, na.rm =TRUE), 0, max(m, na.rm =TRUE)), c(lower, mid, upper))
      
    } else if(min(m, na.rm =TRUE) < 0){
      
      color <- colorRamp2(c(min(m, na.rm =TRUE), max(m, na.rm =TRUE)), c(lower, mid))
      
    } else{
      color <- colorRamp2(c(0, max(m, na.rm =TRUE)), c(mid, upper))
    }
    
    return(color)
  }
  
  
  #==============================================================================
  # Function to create heatmaps
  #==============================================================================
  
  plot_heatmap <- function(m, clust, column_title, name, lower, mid, upper){
    
    if(nrow(m) > 0){
      colnames(m) <- gsub("(.*)_REMISSION_LONGIT", "\\1\nREMISSION_LONGIT", colnames(m))
      colnames(m) <- gsub("(.*)_NONRESPONDERS_LONGIT", "\\1\nNONRESPONDERS\nLONGIT", colnames(m))
    }

    
    ComplexHeatmap::Heatmap(m, cluster_rows = clust,
                            column_title = column_title, 
                            column_title_gp = gpar(fontsize = 14, fontface = "bold"),
                            column_names_gp = gpar(fontsize = 14, lwd = 100),column_names_max_height = unit(10, "cm"),
                            row_names_gp = gpar(fontsize = 14),
                            row_title = "Gene",cluster_columns = FALSE, 
                            name = name, 
                            col = custom_color_scale(m, lower, mid, upper),
                            na_col = "grey", border = TRUE,
                            heatmap_legend_param = list(labels_gp = gpar(fontsize = 14), title_gp = gpar(fontsize = 14, fontface = "bold"),  legend_height = unit(40, "mm")))
  }
  
  #==============================================================================
  # Reactive value that creates heatmaps based on non-empty inputs
  #==============================================================================

  ht_list <- reactive({
    heatmaps <- HeatmapList()

    # Check if clustering is selected and create a clustered matrix of the selected columns if true
    if(input$clusteryn){
      clust <- clustering(create_matrix(input$cluster))
    } else{
      clust <- FALSE
    }

    # Check if any clinical parameters are selected and create a heatmap if true
    if (length(input$clinic) > 0) {
      m_clinic <- create_matrix(input$clinic)
      hm_clinic <- m_clinic %>% plot_heatmap(clust, "Correlation with clinical parameters", "Clinic", "#88c7fd", "white", "#FFD700")
      heatmaps <- heatmaps + hm_clinic
    }

    # Check if any in-vitro parameters are selected and create a heatmap if true
    if (length(input$vitro) > 0) {
      m_vitro <- create_matrix(input$vitro)
      hm_vitro <- m_vitro %>% plot_heatmap(clust, "In-vitro", "In-vitro", "blue",  "white", "red")

      heatmaps <- heatmaps+ hm_vitro
    }

    # Check if the proteome column is selected and create a heatmap if true
    if (input$proteome) {
            m_proteome <- create_matrix("PROT_SLE_CTRL_CROSS_SECT")
            hm_proteome <- m_proteome %>% plot_heatmap(clust, "Proteome",name = "Proteome", "#006400", "white", "#800080")

            heatmaps <- heatmaps+ hm_proteome
          }

    # Check if any MOFA parameters are selected and create a heatmap if true
    if (length(input$mofa) > 0) {
      m_mofa <- create_matrix(input$mofa)
      hm_mofa <- m_mofa %>% plot_heatmap(clust, "MOFA", "MOFA", "#008B8B", "white", "#FF8C00")

      heatmaps <- heatmaps+ hm_mofa
    }

    # Check if any LFC parameters are selected and create a heatmap if true
    if (length(input$lfc) > 0) {
      m_lfc <- create_matrix(input$lfc)
      hm_lfc <- m_lfc %>% plot_heatmap(clust, "LFC", "LFC", "#00008B", "white", "#8B0000")

      heatmaps <- heatmaps+ hm_lfc
    }
    
    # Check if any ECU parameters are selected and create a heatmap if true
    if (length(input$ecu) > 0) {
      m_ecu <- create_matrix(input$ecu)
      hm_ecu <- m_ecu %>% plot_heatmap(clust, "ECU", "ECU", "#90ed7b", "white", "#f79edd")

      heatmaps <- heatmaps+ hm_ecu
    }
   
    # Check if the count column is selected and create a heatmap if true
    if(input$count){
      m_count <- create_matrix("total_count")
      hm_count <- m_count %>% plot_heatmap(clust, "Count", "count", "white", "white", "#8c07f2")

      heatmaps <- heatmaps+ hm_count
    }
    
    # Return the list of heatmaps
    return(heatmaps)
  })

  #==============================================================================
  # Render the heatmap
  #==============================================================================

  output$heatmap_output <- renderPlot({
    if (!length(ht_list()) == 0) {
      ht <- draw(ht_list())
      ht
    }
  }, height = function() {
    matrix_rows <- nrow(create_matrix(selected_columns()))
    400 + matrix_rows * 15
  })

  #==============================================================================
  # Render the table
  #==============================================================================
  output$table <- renderDT({
    # Assuming df, input$Gene, input$slider, and selected_columns() are defined elsewhere in your server function
    if(isTruthy(input$Gene)){
      selectedData <- df %>% filter(Gene %in% input$Gene & input$slider[1] < Start & input$slider[2] > Start) %>% dplyr::select(Gene, MasterKey, Start, all_of(selected_columns()))
    } else{
      selectedData <- df %>% filter(input$slider[1] < Start & input$slider[2] > Start) %>% dplyr::select(Gene, MasterKey, Start, all_of(selected_columns()))
    }

    datatable(selectedData, rownames = FALSE, filter = "top",
              extensions = 'Buttons',
              selection = "none",
              options = list(scrollX = TRUE,
                             dom = 'frtipB',
                             buttons = c('copy', 'csv', 'excel')),
              escape = FALSE)
  })

  output$htmldata <- renderUI({
    includeHTML("www/about.html")
  })
}