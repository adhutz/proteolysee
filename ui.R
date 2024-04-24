library(shiny)
library(shinyWidgets)
library(shinyBS)
library(bsicons)

source("tooltips.R")

# Function to list files recursively
list_files_recursive <- function(path = ".", pattern = NULL) {
  files <- list.files(path = path, pattern = pattern, full.names = TRUE, recursive = TRUE)
  return(files)
}

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tags$div(
    style = "height: 100px; display: flex; align-items: center", # Adjust the height as needed
    titlePanel("ProteolySee – the human plasma proteolytotype visualizer")
  ),

  tags$style(HTML("
  body{font-size: 14px;}
  h4 {font-size: 1.2em; font-weight: bold;}
    .flex-container {
      display: flex;
      align-items: center;
      justify-content: space-between;
      width = 100%;
    }
    .selectize-input { word-wrap : break-word;}
    .selectize-input { word-break: break-word;}
    .selectize-dropdown {word-wrap : break-word;}
     .icon-button {
      background: none;
      border: none;
      padding: 0;
      font: inherit;
      cursor: pointer;
      outline: inherit;
      padding-top:10px;
    }
    .tooltip-inner { max-width: 300px !important; width: max-content !important; text-align: justify; }
    .tooltip{font-size: 14px;word-wrap : break-word;}"
  )),

  fluidRow(
    column(3,
           div(style = "min-width:250px;",
           wellPanel(
             h4("Select rows"),
             uiOutput("geneSelector"), # This will be dynamically generated

             tags$span(
               tags$span(id = "slider_tt", 
                         bsicons::bs_icon("info-circle", 
                                          size = "15px")), 
               tags$strong("AA Region"),
             ),
             div(class = "flex-container", style = "Margin-top: -10px;",
                 sliderInput("slider", "", min = 0, max = Inf, value = c(1, Inf), width = "80%"),
                 div(id = "reset_region_tt",
                 actionButton(inputId = "reset", label =  icon("refresh"), class = "icon-button")
                 ),
             ),

            selectInput("order", "Order by", choices = "Start", selected = "Start"),
             

             materialSwitch(inputId = "order_gene", 
                            label = "Order by gene", right = TRUE, value = TRUE, inline = TRUE, status = "primary"),
             
             materialSwitch(inputId = "naExclude", 
                            label = tags$span(id = "naExclude_tt", "Exclude missing rows",
                                              bsicons::bs_icon("info-circle", size = "15px")), 
                            right = TRUE, value = TRUE, inline = TRUE, status = "primary"),
             
              ),
           
           #selectInput('Gene', 'Gene', unique(df$Gene), multiple=TRUE, selectize=TRUE),
           wellPanel(
             h4("Select columns"),
             tags$span(
               tags$span(id = "clinic_tt", 
                         bsicons::bs_icon("info-circle", 
                                          size = "15px")), 
               tags$strong('Pearson correlation with clinical parameters')),
             uiOutput("clinicSelector"),  # This will be dynamically generated
             
             tags$span(
               tags$span(id = "lfc_tt", 
                         bsicons::bs_icon("info-circle", 
                                          size = "15px")), 
               tags$strong('Log2 FoldChanges')),
             uiOutput("lfcSelector"),  # This will be dynamically generated
             
             
           tags$span(
             tags$span(id = "mofa_tt", 
                       bsicons::bs_icon("info-circle", 
                                        size = "15px")), 
             tags$strong("MOFA")),
           
           
             uiOutput("mofaSelector"),  # This will be dynamically generated
           
           tags$span(
             tags$span(id = "ecu_tt", 
                       bsicons::bs_icon("info-circle", 
                                        size = "15px")), 
             tags$strong("ECU")),
             uiOutput("ecuSelector"),  # This will be dynamically generated
           
           
           tags$span(
             tags$span(id = "vitro_tt", 
                       bsicons::bs_icon("info-circle", 
                                        size = "15px")), 
             tags$strong("In vitro")),
           uiOutput("vitroSelector"),  # This will be dynamically generated
           
           
           tags$div(style = "padding-left:20px",
                    materialSwitch(inputId = "filter", 
                                   label = tags$span(id = "filter_tt",
                                                     bsicons::bs_icon("info-circle", value = FALSE, style = "color: blue; ", size = "15px"), "Filter candidates"), 
                                   right = FALSE, value = FALSE, inline = TRUE, status = "primary")),
           
           # tags$span(
           #   tags$span(id = "proteome_tt", 
           #             bsicons::bs_icon("info-circle", 
           #                              size = "15px")), 
           #   tags$strong("Proteomic data")),
           # #uiOutput("proteomeSelector"),  # This will be dynamically generated
           # materialSwitch(inputId = "proteome", 
           #                label = "", right = TRUE, value = FALSE, inline = TRUE, status = "primary"),
           # 
           tags$span(tags$span(id = "proteome_tt",
                               style = "display: inline-block; margin-bottom: 5px;",
                               bsicons::bs_icon("info-circle", size = "15px")), strong("Proteomic data")),
           materialSwitch(inputId = "proteome", 
                          label = tags$span(style = "padding-left:37px;padding-right:10px", "Display column"),
                          right = FALSE, value = FALSE, inline = FALSE, status = "primary"),
           
           
           
           tags$span(tags$span(id = "count_tt",
                               style = "display: inline-block; margin-bottom: 5px;",
                                  bsicons::bs_icon("info-circle", size = "15px")), strong("Count")),
           materialSwitch(inputId = "count", 
                          label = tags$span(style = "padding-left:37px;padding-right:10px", "Display column"),
                          right = FALSE, value = FALSE, inline = FALSE, status = "primary"),
           
            tags$div(style = "display: flex; align-items: center; margin-top: -25px; padding-left:37px",
                     tags$span("Minimum count"),
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
           )
           
           
           

           ),
           wellPanel(
             h4(tags$span(tags$span(id = "cluster_rows_tt",
                          bsicons::bs_icon("info-circle", size = "15px")), "Cluster rows")),
             materialSwitch(inputId = "clusteryn", label = "", right = TRUE, value = FALSE, inline = TRUE, status = "primary"),
             
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
             
             # selectInput('clinic', 'Clinical Data', col_sel$clinic, multiple=TRUE, selectize=TRUE),
             #selectInput('lfc', 'Log2 FoldChanges', col_sel$lfc, multiple=TRUE, selectize=TRUE),
             #selectInput('proteome', 'Proteomic data', col_sel$proteome, multiple=TRUE, selectize=TRUE),
             #selectInput('vitro', 'In vitro', col_sel$vitro, multiple=TRUE, selectize=TRUE),
             #selectInput('mofa', 'MOFA', col_sel$mofa, multiple=TRUE, selectize=TRUE)
           ),
           )
    ),
    column(9,
           tabsetPanel(
             tabPanel("About", htmlOutput("htmldata")),
             tabPanel("Heatmap", plotOutput("heatmap_output", height = "100%")),
             tabPanel("Data", DT::DTOutput("table"))
           )
    )
  ),
 
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

