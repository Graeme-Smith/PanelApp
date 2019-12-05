#
# Purpose: Interactive Shiny App that allows the user to select multiple PanelApp panels using the 
# PanelApp API.  Network analysis is then performed to identify 
source("chooser.R")

##### Define functions #####

# Get required packages
GetPackages <- function(required.packages) {
  packages.not.installed <- required.packages[!(required.packages %in% installed.packages()[, "Package"])]
  if(length(packages.not.installed)){install.packages(packages.not.installed, dependencies = T)}
  suppressMessages(lapply(required.packages, require, character.only = T))
}

GetPackages(c("BiocManager", "shiny", "tidyverse", "jsonlite", "ggplot2", "plotly", "igraph", "WebGestaltR",
              "png"))

# One package needed from BiocManager
BiocManager::install("RCy3")
Non.CRAN.packages <- c("RCy3", "igraph")
lapply(Non.CRAN.packages, library, character.only = T)

# Test if Cytoscape has successfully loaded
test.message <- cytoscapePing ()
if(test.message == "You are connected to Cytoscape!"){
  print("You are connected to Cytoscape!")
} else {
  print("Connection failed - make sure Cytoscape is open locally")
}

# List of cytoscape apps to install
CytoscapeAppInstallation <- function(App.List){
  # List of app to install
  installation_responses <- c()
  cytoscape_version <- unlist(strsplit( cytoscapeVersionInfo()['cytoscapeVersion'],split = "\\."))
  if(length(cytoscape_version) == 3 && as.numeric(cytoscape_version[1]>=3) && as.numeric(cytoscape_version[2]>=7)) {
    for(i in 1:length(App.List)){
      # Check if the app is installed, only install if it hasn't been already
      if(!grep(commandsGET(paste("apps status app=\"", App.List[i],"\"", sep="")), pattern = "status: Installed")) {
        installation_response <- commandsGET(paste("apps install app=\"", App.List[i],"\"", sep=""))
        installation_responses <- c(installation_responses,installation_response)
      } else {
        installation_responses <- c(installation_responses,"already installed")
      }
    } 
    installation_summary <- data.frame(name = App.List, status = installation_responses)
    knitr::kable(list(installation_summary), booktabs = TRUE, caption = 'A Summary of automated app installation') 
  }
}

App.List <- c("clustermaker2", "enrichmentmap", "autoannotate", "wordcloud", "stringapp", "aMatReader")
CytoscapeAppInstallation(App.List)

# This one didn't work for some unknown reason, so do it manually
installApp("stringApp")

# This needs to be passed from the PanelApp list
# panel.genes <- read.csv(file = "../../Documents/genelist_example.csv")

StringGeneExpansion <- function(panel.genes){
  # This determines the query sent to String to expand the gene list
  string_interaction_cmd <- paste('string protein query taxonID=9606 limit=1000 cutoff=0.8 query="',
                                  paste(panel.genes, collapse = ","), '"', sep = "")
  commandsGET(string_interaction_cmd)
  node.attribute.table <- getTableColumns(table="node")
  return(node.attribute.table)
}

# Create force directed layout
NetworkImage <- function(){
  layoutNetwork('force-directed')
  getLayoutPropertyNames(layout.name = 'force-directed')
  layoutNetwork('force-directed defaultSpringCoefficient=0.0000008 defaultSpringLength=70')
}

# NetworkImage()

SaveNetworkImage <- function(){
  string_network_png_file_name <<- file.path(getwd(), "string_network.png")
  if(file.exists(string_network_png_file_name)) {
    # cytoscape hangs waiting for user response if file already exists, so remove if present
    response <- file.remove(string_network_png_file_name) }
  response <- exportImage(string_network_png_file_name, type = "png")
}

# SaveNetworkImage()

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
      multiple = T
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
           DT::dataTableOutput("gene_table"),
           DT::dataTableOutput("expanded_gene_table")
  ),
  tabPanel("Network Analysis",
           titlePanel("Network Analysis for selected PanelApp gene panel"),
           # DT::dataTableOutput("network_analysis"),
           imageOutput("network_analysis_image"),
           "Place html help file here"
  ),
  tabPanel("WebGestaltAPI",
           "Place html help file here"
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
  # output$selection <- renderPrint({
  #  input$mychooser[1]
  #})
  
  # Display selected genes in table
  output$panel_table <- DT::renderDataTable({
    DT::datatable(as.data.frame(panel_list))
  })
  
  # Network analysis picture
  output$network_analysis_image <- grid::grid.raster(readPNG(source = file.path(getwd(), "string_network.png")))

  output$network_analysis_image <- renderImage({
    # Return a list containing the filename
    list(src = file.path(getwd(), "string_network.png"),
         contentType = 'image/png',
         width = 1200,
         height = 900,
         alt = "This is alternate text")
  }, deleteFile = F)

  # Expand the selected gene list, make a network, make an image of the network
  output$expanded_gene_table <- DT::renderDataTable({
    selected_panels <- panel_list[panel_list$panel_name %in% unlist(input$mychooser[2]),]
    selected_genes <- lapply(selected_panels$panel_id, getPanelGenes)
    panel.genes <- as.data.frame(selected_genes)
    node.attribute.table <- StringGeneExpansion(panel.genes)
    expanded.gene.list <- node.attribute.table$`display name`
    
    NetworkImage()
    SaveNetworkImage()
    
    return(DT::datatable(as.data.frame(expanded.gene.list)))
    
  })
  
  # output$network_analysis <- DT::renderDataTable({
  #   selected_panels <- panel_list[panel_list$panel_name %in% unlist(input$mychooser[2]),]
  #   selected_genes <- lapply(selected_panels$panel_id, getPanelGenes)
  #   panel.genes <- as.data.frame(selected_genes)
  #   node.attribute.table <- StringGeneExpansion(panel.genes)
  #   
  #   NetworkImage()
  #   SaveNetworkImage()
  # 
  # })
  
  # NetworkImage()
  # SaveNetworkImage()
  
  # Display selected genes in table
  output$gene_table <- DT::renderDataTable({

    selected_panels <- panel_list[panel_list$panel_name %in% unlist(input$mychooser[2]),]
    # Use panel_id from selected panels to get panel genes
    selected_genes <- lapply(selected_panels$panel_id, getPanelGenes)
    # getPanelGenes(panel)
    DT::datatable(as.data.frame(selected_genes))
  })  
    
}

# Run the application 
shinyApp(ui = ui, server = server)









# # Create network style
# style.name = "TestStyle"
# defaults.list <- list(NODE_SHAPE = "ellipse", NODE_SIZE = 10,
#                       NODE_FILL_COLOR = "#AAAAAA", EDGE_TRANSPARENCY = 120)
# # p for passthrough; nothing else needed
# node.label.map <- mapVisualProperty('node label', 'display name', 'p')
# createVisualStyle(style.name, defaults.list, list(node.label.map))
# setVisualStyle(style.name = style.name)
