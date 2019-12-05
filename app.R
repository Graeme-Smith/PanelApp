#
# Purpose: Interactive Shiny App that allows the user to select multiple PanelApp panels using the 
# PanelApp API.  Network analysis is then performed to identify 
source("chooser.R")
source("webGestaltAPI.R")
library(shiny)
library(tidyverse)
library(jsonlite)
library(ggplot2)
library(plotly)
library(igraph)
library(WebGestaltR)
library(RCy3)
library(waiter)

# Define functions

getPanelAppList <- function() {
  api_query <- "https://panelapp.genomicsengland.co.uk/WebServices/list_panels/?format=json"
  json_data <- fromJSON(api_query, flatten=TRUE)
  panelApp_panels <- tibble(panel_name = json_data$result$Name,
                            panel_id = json_data$result$Panel_Id,
                            num_of_gene = json_data$result$Number_of_Genes,
                            version = json_data$result$CurrentVersion
                            )
  return(panelApp_panels)
}

panel_list <- getPanelAppList()

getPanelGenes <- function(panel_id){
  api_query <- paste0("https://panelapp.genomicsengland.co.uk/WebServices/get_panel/",
                      panel_id,
                      "/?format=json")
  json_data <- fromJSON(api_query, flatten=TRUE)
  panel_genes <- tibble(gene_symbol = json_data$result$Genes$GeneSymbol,
                        evidence = json_data$result$Genes$LevelOfConfidence)  
  
  return(panel_genes)
}

ui <- navbarPage(
  "PanelApp Pathway Analysis",
  tabPanel(
    "Select Panels",
    titlePanel("Select Panels"),
    chooserInput(
      "mychooser",
      "Available PanelApp Panels",
      "Selected PanelApp Panels",
      panel_list$panel_name,
      c(),
      size = 10,
      multiple = TRUE
    ),
    actionButton("runAll", label="Run Analysis"),
    p("Click the button to analyze panels")
  ),
  tabPanel("Available PanelApp Panels",
           titlePanel("Available Panels"),
           DT::dataTableOutput("panel_table")
  ),
  tabPanel("Imported genes",
           titlePanel("Genes Selected for Analysis"),
           DT::dataTableOutput("gene_table")
  ),
  tabPanel("Network Analysis",
           "Place html help file here"
  ),
  tabPanel("WebGestaltAPI",
           "Place html help file here"
  ),
  tabPanel("WebGestalt Table",
           titlePanel("WebGestalt Output"),
           DT::dataTableOutput("wg_table")
  ),
  tabPanel("HPO Analysis",
           "Place html help file here"
  ),
  tabPanel("GTex Analysis",
           "Place html help file here"
  ),
  tabPanel("Panel Candidates",
           "Place html help file here"
  ),
  tabPanel("Help",
           "Place html help file here"
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # Render selected panel
  #output$selection <- renderPrint({
  #  input$mychooser[1]
  #})
  
  # Display selected genes in table
  output$panel_table <- DT::renderDataTable({
    DT::datatable(as.data.frame(panel_list))
  })

  # Display selected genes in table
  output$gene_table <- DT::renderDataTable({

    selected_panels <- panel_list[panel_list$panel_name %in% unlist(input$mychooser[2]),]
    # Use panel_id from selected panels to get panel genes
    selected_genes <- lapply(selected_panels$panel_id, getPanelGenes)
    # getPanelGenes(panel)
    DT::datatable(as.data.frame(selected_genes))
  })  
    
  # Display WebGestalt output in table
  output$wg_table <- DT::renderDataTable({
    
    selected_panels <- panel_list[panel_list$panel_name %in% unlist(input$mychooser[2]),]
    # Use panel_id from selected panels to get panel genes
    selected_genes <- lapply(selected_panels$panel_id, getPanelGenes)
    outputDirectory <- getwd()
    DT::datatable(callWebGestalt(unlist(selected_genes), outputDirectory))
  })  

}

# Currently hardcoded output directory (file written over every time app is run)
html_temp_file <- paste0(outputDirectory, "/Project_temp_webGestalt/Report_temp_webGestalt.html")

# Run the application 
shinyApp(ui = ui, server = server)

