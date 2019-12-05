library(WebGestaltR)
# Interface with the WebGestalt API

geneFile <- system.file("extdata",
                        "interestingGenes.txt",
                        package="WebGestaltR")
refFile <- system.file("extdata",
                       "referenceGenes.txt",
                       package="WebGestaltR")
outputDirectory <- getwd()
enrichResult <- WebGestaltR(enrichMethod="ORA",
                            organism="hsapiens",
                            enrichDatabase="pathway_KEGG",
                            interestGeneFile=geneFile,
                            interestGeneType="genesymbol",
                            referenceGeneFile=refFile,
                            referenceGeneType="genesymbol",
                            isOutput=TRUE,
                            outputDirectory=outputDirectory,
                            projectName=NULL)