# Get required packages
GetPackages <- function(required.packages) {
  packages.not.installed <- required.packages[!(required.packages %in% installed.packages()[, "Package"])]
  if(length(packages.not.installed)){install.packages(packages.not.installed, dependencies = T)}
  suppressMessages(lapply(required.packages, require, character.only = T))
}

GetPackages(c("BiocManager"))

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

# Can use cytoscapeVersionInfo () to get version information

#list of cytoscape apps to install
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
panel.genes <- read.csv(file = "../../Documents/genelist_example.csv")

StringGeneExpansion <- function(panel.genes){
  # This determines the query sent to String to expand the gene list
  string_interaction_cmd <- paste('string protein query taxonID=9606 limit=150 cutoff=0.9 query="',
                                  paste(panel.genes$geneid, collapse = ","), '"', sep = "")
  commandsGET(string_interaction_cmd)
  node.attribute.table <- getTableColumns(table="node")
  return(node.attribute.table)
}

node.attribute.table <- StringGeneExpansion(panel.genes)

# Get the gene names from the network
expanded.gene.list <- node.attribute.table$`display name`

# Create network style
style.name = "TestStyle"
defaults.list <- list(NODE_SHAPE = "ellipse", NODE_SIZE = 10, 
                      NODE_FILL_COLOR = "#AAAAAA", EDGE_TRANSPARENCY = 120)
# p for passthrough; nothing else needed
node.label.map <- mapVisualProperty('node label', 'display name', 'p')
createVisualStyle(style.name, defaults.list, list(node.label.map))
setVisualStyle(style.name = style.name)

# Create force directed layout
NetworkImage <- function(){
  layoutNetwork('force-directed')
  getLayoutPropertyNames(layout.name = 'force-directed')
  layoutNetwork('force-directed defaultSpringCoefficient=0.0000008 defaultSpringLength=70')
}

NetworkImage()
  
SaveNetworkImage <- function(){
  string_network_png_file_name <- file.path(getwd(), "string_network.png")
  if(file.exists(string_network_png_file_name)) {
  # cytoscape hangs waiting for user response if file already exists, so remove if present
  response <- file.remove(string_network_png_file_name) }
  response <- exportImage(string_network_png_file_name, type = "png")
}

SaveNetworkImage()







