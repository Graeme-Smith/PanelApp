library(WebGestaltR)
# Interface with the WebGestalt API - uses the ORA enrichment method and "genome" reference set

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
