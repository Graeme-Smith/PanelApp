

create_network <- function(gene_list){
  network_list$BioGRID <- readSIF( '/home/graeme/Desktop/panelapp_network/panelNet/BIOGRID-ORGANISM-Homo_sapiens-3.5.179.tab2.txt',
                                   p1=8, p2=9, et=12, score=19, quote='', skip=1, comment.char='' )
  network_list$BioGRID$score <- as.numeric(network_list$BioGRID$score)
  '/home/graeme/Desktop/panelapp_network/panelNet/BIOGRID-ORGANISM-Homo_sapiens-3.5.179.tab2.txt'
my_network <- network_overlap( gene_list,
                               resources = c('BioGRID'),
                               include_neighbors = TRUE,
                               dedup = TRUE )
  return(my_network)
}

create_network_plot <- function(my_network){
  edges <- as.matrix( my_network[, c(1,3)] )
  G <- igraph::make_undirected_graph( c(t(edges)) )
  par(mar=rep(0,4))
  p <- plot(G, vertex.size = 20, vertex.frame.color = 'white' )
  return(p)
}