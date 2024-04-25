library(shiny)
library(shinyWidgets)
library(shinyBS)
library(bsicons)

source("tooltips.R")

ui <- fluidPage(
  shinyjs::useShinyjs(),
  

  tags$div(
    style = "height: 100px; display: flex; align-items: center", # Adjust the height as needed
    titlePanel("ProteolySee – the human plasma proteolytotype visualizer")
  ),
  
  #==============================================================================
  # load css
  #==============================================================================
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),

  fluidRow(
    #==============================================================================
    # Create sidebar
    #==============================================================================
    column(3,
       div(style = "min-width:250px;",
             
          #==============================================================================
          # Row selection panel 
          #==============================================================================
           wellPanel(
             
             #==============================================================================
             # Filter N-termini
             #==============================================================================
             h4("Select rows"),
             uiOutput("geneSelector"), # This will be dynamically generated

             # Heading
             tags$span(
               tags$span(id = "slider_tt", 
                         bsicons::bs_icon("info-circle", 
                                          size = "15px")), 
               tags$strong("AA Region"),
             ),
             
             # Slider
             div(class = "flex-container", style = "Margin-top: -10px;",
                 sliderInput("slider", "", min = 0, max = Inf, value = c(1, Inf), width = "80%"),
                 div(id = "reset_region_tt",
                 actionButton(inputId = "reset", label =  icon("refresh"), class = "icon-button")
                 ),
             ),

             #==============================================================================
             # Select columns to order by
             #==============================================================================
             
             # Column selector
             selectInput("order", "Order by", choices = "Start", selected = "Start"),
             
             # Should be ordered by gene
             materialSwitch(inputId = "order_gene", 
                            label = "Order by gene", right = TRUE, value = TRUE, inline = TRUE, status = "primary"),
             
             # Exclude rows with only missing data
             materialSwitch(inputId = "naExclude", 
                            label = tags$span(id = "naExclude_tt", "Exclude missing rows",
                                              bsicons::bs_icon("info-circle", size = "15px")), 
                            right = TRUE, value = TRUE, inline = TRUE, status = "primary"),
             
              ),
           
          #==============================================================================
          # Column selector
          #==============================================================================
           wellPanel(
             # Heading
             h4("Select columns"),
             
             #==============================================================================
             # clinical data
             #==============================================================================
             tags$span(
               tags$span(id = "clinic_tt", 
                         bsicons::bs_icon("info-circle", 
                                          size = "15px")), 
               tags$strong('Pearson correlation with clinical parameters')
             ),
             
             uiOutput("clinicSelector"),  # This will be dynamically generated
             
             #==============================================================================
             # lfc data
             #==============================================================================
             tags$span(
               tags$span(id = "lfc_tt", 
                         bsicons::bs_icon("info-circle", 
                                          size = "15px")), 
               tags$strong('Log2 FoldChanges')
               ),
             
             uiOutput("lfcSelector"),  # This will be dynamically generated
             
             #==============================================================================
             # MOFA data
             #==============================================================================
             tags$span(
               tags$span(id = "mofa_tt", 
                         bsicons::bs_icon("info-circle", 
                                          size = "15px")), 
               tags$strong("MOFA factors")
               ),
          
             uiOutput("mofaSelector"),  # This will be dynamically generated
           
             #==============================================================================
             # ECU data
             #==============================================================================
             tags$span(
               tags$span(id = "ecu_tt", 
                         bsicons::bs_icon("info-circle", 
                                          size = "15px")), 
               tags$strong("Eculizumab treatment cohort")
               ),
             
             uiOutput("ecuSelector"),  # This will be dynamically generated
           
             #==============================================================================
             # In vitro data
             #==============================================================================
             
             # Heading
             tags$span(
             tags$span(id = "vitro_tt", 
                       bsicons::bs_icon("info-circle",size = "15px")
                       ), 
             tags$strong("In vitro protease digests")
             ),
             
             uiOutput("vitroSelector"),  # This will be dynamically generated
           

             # Filter for significance
             tags$div(style = "padding-left:20px",
                    materialSwitch(inputId = "filter", 
                                   label = tags$span(
                                     id = "filter_tt",
                                     bsicons::bs_icon("info-circle", value = FALSE, style = "color: blue; ", size = "15px"), 
                                     "Filter candidates"), 
                                   right = FALSE, value = FALSE, inline = TRUE, status = "primary")
                    ),
           
             #==============================================================================
             # Proteome data
             #==============================================================================
             
             #Title
             tags$span(
               tags$span(id = "proteome_tt",
                         style = "display: inline-block; margin-bottom: 5px;",
                         bsicons::bs_icon("info-circle", size = "15px")
                         ), 
               strong("Proteomic data")
               ),
             
             # Switch
             materialSwitch(inputId = "proteome", 
                            label = tags$span(style = "padding-left:37px;padding-right:10px", "Display column"),
                            right = FALSE, value = FALSE, inline = FALSE, status = "primary"),
           
             #==============================================================================
             # Count data
             #==============================================================================  
             
             #Title
             tags$span(tags$span(id = "count_tt",
                                 style = "display: inline-block; margin-bottom: 5px;",
                                 bsicons::bs_icon("info-circle", size = "15px")
                                 ), 
                       strong("Number of patients")
                       ),
             
             # Switch to select count column
             materialSwitch(inputId = "count", 
                            label = tags$span(style = "padding-left:37px;padding-right:10px", "Display column"),
                            right = FALSE, value = FALSE, inline = FALSE, status = "primary"),
             
             # Filter for minimum count
              tags$div(style = "display: flex; align-items: center; margin-top: -25px; padding-left:37px",
                       tags$span("Minimum number of patients"),
                       div(style = "padding-left: 20px; width:100px",
                           numericInput(
                             "count_filter",
                             "",
                             0,
                             min = 0,
                             max = 166,
                             step = 1
                           ) # This will be dynamically generated
                       )
                      ),
           ),
          
          #==============================================================================
          # Clustering panel
          #==============================================================================
           wellPanel(
             
             # Title
             h4(
               tags$span(
                 tags$span(id = "cluster_rows_tt",
                          bsicons::bs_icon("info-circle", size = "15px")
                          ), 
                 "Cluster rows")
               ),
             
             # Cluster yes/no
             materialSwitch(inputId = "clusteryn", label = "", right = TRUE, value = FALSE, inline = TRUE, status = "primary"),
             
             #==============================================================================
             # Conditional Panel displayed if clustering is on
             #==============================================================================
             conditionalPanel("input.clusteryn == true",
                               strong("By"),
                               div(class = "flex-container",
                                   style = "align-items: flex-start;",
                                   div(style = "width:85%",
                                       uiOutput("clusterSelector")  # This will be dynamically generated
                                   ),
                                   tags$div(id = "reset_cluster_tt",
                                            actionButton(inputId = "reset_cluster", label =  icon("refresh"), class = "icon-button")
                                   )
                                ),
                               
             ),
           ),
       )
    ),
  
  #==============================================================================
  # Colum for output
  #==============================================================================
    column(9,
           tabsetPanel(
             tabPanel("About", htmlOutput("htmldata")),
             tabPanel("Heatmap", plotOutput("heatmap_output", height = "100%")),
             tabPanel("Data", DT::DTOutput("table"))
           )
    )
  ),
 
#==============================================================================
# Tooltip creation
#==============================================================================
shinyBS::bsTooltip(id = "naExclude_tt", title = naExclude_tt_title, placement = "right", trigger = "hover"),
shinyBS::bsTooltip(id = "clinic_tt", title = clinic_tt_title, placement = "right", trigger = "hover"),
shinyBS::bsTooltip(id = "lfc_tt", title = lfc_tt_title, placement = "right", trigger = "hover"),
shinyBS::bsTooltip(id = "proteome_tt", title = proteome_tt_title, placement = "right", trigger = "hover"),
shinyBS::bsTooltip(id = "vitro_tt", title = vitro_tt_title, placement = "right", trigger = "hover"),
shinyBS::bsTooltip(id = "mofa_tt", title = mofa_tt_title, placement = "right", trigger = "hover"),
shinyBS::bsTooltip(id = "ecu_tt", title = ecu_tt_title, placement = "right", trigger = "hover"),
shinyBS::bsTooltip(id = "count_tt", title = count_tt_title, placement = "right", trigger = "hover"),
shinyBS::bsTooltip(id = "cluster_rows_tt", title = cluster_rows_tt_title, placement = "right", trigger = "hover"),
shinyBS::bsTooltip(id = "reset_cluster_tt", title = reset_cluster_tt_title, placement = "bottom", trigger = "hover"),
shinyBS::bsTooltip(id = "reset_region_tt", title = reset_region_tt_title, placement = "bottom", trigger = "hover"),
shinyBS::bsTooltip(id = "order_gene_tt", title = order_gene_tt_title, placement = "right", trigger = "hover"),
shinyBS::bsTooltip(id = "filter_tt", title = filter_tt_title, placement = "right", trigger = "hover"),
shinyBS::bsTooltip(id = "slider_tt", title = slider_tt_title, placement = "top", trigger = "hover")
 
)

