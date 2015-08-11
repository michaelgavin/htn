#' Build graph
#' 
#' Converts a \code{docNetwork} object into an \code{igraph} object. This step
#' 
#' @param dnet A docNetwork object
#' 
#' @param draw Logical. If TRUE, will also draw the sociogram.
#' 
#' @return g An igraph object.
#' 
#' @examples
#' 
#' dnet@graph = buildGraph(dnet, draw=T)
#' dnet@graph = buildGraph(dnet)
#' 
#' @export
buildGraph <- function(dnet, draw = F) {
  tcps = dnet@index$TCP
#   edges = data(tcpEdges)
#   dnet@edges = edges[which(edges$TCP %in% tcps),]
  if(length(dnet@persons) == 0){
    g <- graph.data.frame(dnet@edges, directed = F)
  } else {
    g <- graph.data.frame(dnet@edges, dnet@persons, directed = F)
  }
  if(!draw){
    return(g)
  } else {
    drawGraph(dnet)
  }
}