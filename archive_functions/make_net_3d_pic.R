#' make_net_3d_pic
#'
#' Input flow table to create a 3d diagram of flow routing
#' @param flw Flow table list
#' @author Daniel Nash
#'
make_net_3d_pic<-function(flw){

  ntp<-make_flow_table(flw)
  coords<-make_coord_table(flw)
  dtp = igraph::graph.adjacency(ntp,mode="directed",weighted = TRUE,diag = FALSE)
  igraph::rglplot(dtp,layout=coords[,1:3],edge.width=igraph::E(dtp)$weight*2,edge.arrow.size=0.2,vertex.size=5)
}
