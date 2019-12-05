library(WebGestaltR)
# Interface with the WebGestalt API - uses the ORA enrichment method and "genome" reference set

# Load in test data
geneFile <- system.file("extdata",
                        "interestingGenes.txt",
                        package="WebGestaltR")
myGeneList <-unlist(read.csv(geneFile, stringsAsFactors = FALSE))

# Input: Gene list vector using gene symbols
# Output: Data frame and HTML report 

outputDirectory <- getwd()
#TODO: auto generate projectName based upon input panel id 
callWebGestalt <- function(myGeneList, outputDirectory) {
enrichResult <- WebGestaltR(enrichMethod="ORA",
                            organism="hsapiens",
                            enrichDatabase="geneontology_Biological_Process",
                            interestGene = myGeneList,
                            interestGeneType="genesymbol",
                            referenceSet= "genome",
                            referenceGeneType="genesymbol",
                            isOutput=TRUE,
                            outputDirectory=outputDirectory,
                            projectName="temp_webGestalt")
  return(enrichResult)
}
# Currently hardcoded output directory (file written over every time app is run)
html_temp_file <- paste0(outputDirectory, "/Project_temp_webGestalt/Report_temp_webGestalt.html")