install.packages("BiocManager")
BiocManager::install("RCy3")
library(RCy3)
library(igraph)

cytoscapePing ()

cytoscapeVersionInfo ()

#list of cytoscape apps to install
installation_responses <- c()

#list of app to install
installation_responses <- c()
cyto_app_toinstall <- c("clustermaker2", "enrichmentmap", "autoannotate", "wordcloud", "stringapp", "aMatReader")
cytoscape_version <- unlist(strsplit( cytoscapeVersionInfo()['cytoscapeVersion'],split = "\\."))

if(length(cytoscape_version) == 3 && as.numeric(cytoscape_version[1]>=3) 
   && as.numeric(cytoscape_version[2]>=7)){
  for(i in 1:length(cyto_app_toinstall)){
    #check to see if the app is installed.  Only install it if it hasn't been installed
    if(!grep(commandsGET(paste("apps status app=\"", cyto_app_toinstall[i],"\"", sep="")), 
             pattern = "status: Installed")){
      installation_response <-commandsGET(paste("apps install app=\"", 
                                                cyto_app_toinstall[i],"\"", sep=""))
      installation_responses <- c(installation_responses,installation_response)
    } else{
      installation_responses <- c(installation_responses,"already installed")
    }
  }
  installation_summary <- data.frame(name = cyto_app_toinstall, 
                                     status = installation_responses)
  
  knitr::kable(list(installation_summary),
               booktabs = TRUE, caption = 'A Summary of automated app installation'
  )
}

installApp("stringApp")

commandsHelp("help")

genes <- read.csv(file = "../../Documents/genelist_example.csv")

mesen_string_interaction_cmd <- paste('string protein query taxonID=9606 limit=150 cutoff=0.9 query="',
                                      paste(genes$geneid, collapse = ","), '"', sep = "")

commandsGET(mesen_string_interaction_cmd)

node_attribute_table_topmesen <- getTableColumns(table="node")
node_attribute_table_topmesen$`display name`

# mesen_string_interaction_cmd_2 <- paste('string protein query taxonID=9606 limit=150 cutoff=0.9 query="',
#                                       paste(node_attribute_table_topmesen$`display name`, collapse = ","), 
#                                       '"', sep = "")
# 
# commandsGET(mesen_string_interaction_cmd_2)
# 
# node_attribute_table_topmesen <- getTableColumns(table="node")
# node_attribute_table_topmesen$`display name`

# Create network style
style.name = "TestStyle"
defaults.list <- list(NODE_SHAPE = "ellipse",
                      NODE_SIZE = 10,
                      NODE_FILL_COLOR = "#AAAAAA",
                      EDGE_TRANSPARENCY = 120)
node.label.map <- mapVisualProperty('node label', 'display name', 'p') # p for passthrough; nothing else needed
createVisualStyle(style.name, defaults.list, list(node.label.map))
setVisualStyle(style.name=style.name)

# Create force directed layout
layoutNetwork('force-directed')
getLayoutPropertyNames(layout.name = 'force-directed')
layoutNetwork('force-directed defaultSpringCoefficient=0.0000008 defaultSpringLength=70')

# Save it
relayout_string_network_png_file_name <- file.path(
  getwd(), "relayout_string_network.png")

if(file.exists(relayout_string_network_png_file_name)){
  #cytoscape hangs waiting for user response if file already exists.  Remove it first
  response<- file.remove(relayout_string_network_png_file_name)
} 

response <- exportImage(relayout_string_network_png_file_name, type = "png")


