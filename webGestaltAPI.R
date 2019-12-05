library(WebGestaltR)
# Interface with the WebGestalt API - uses the ORA enrichment method and "genome" reference set

geneFile <- system.file("extdata",
                        "interestingGenes.txt",
                        package="WebGestaltR")

outputDirectory <- getwd()
enrichResult <- WebGestaltR(enrichMethod="ORA",
                            organism="hsapiens",
                            enrichDatabase="pathway_KEGG",
                            interestGeneFile=geneFile,
                            interestGeneType="genesymbol",
                            referenceSet= "genome",
                            referenceGeneType="genesymbol",
                            isOutput=TRUE,
                            outputDirectory=outputDirectory,
                            projectName=NULL)