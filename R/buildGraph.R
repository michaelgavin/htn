#' Build graph
#' 
#' Converts a \code{docNetwork} object into an \code{igraph} object. This step
#' 
#' @slot dnet A docNetwork object
#' 
#' @slot draw Logical. If TRUE, will also draw the sociogram.
#' 
#' @export
buildGraph <- function(dnet, draw = F) {
  if(length(dnet@persons) == 0){
    g <- graph.data.frame(dnet@edges, directed = F)
  } else {
    g <- graph.data.frame(dnet@edges, dnet@persons, directed = F)
  }
  dnet@graph = g
  if(!plot){
    return(g)
  } else {
    drawGraph(dnet)
  }
}