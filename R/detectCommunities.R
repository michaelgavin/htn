#' Find communities in a graph
#' 
#' @slot dnet A document network
#' 
#' @slot type The algorithm to be used: options include "walktrap" and "betweenness".
#' 
#' @export
detectCommunities <- function(g, type="walktrap") {
  if(type == "walktrap") {
    comms = walktrap.community(g)
  } else if(type == "edge.betweenness") {
    comms = edge.betweenness.community(g)
  } else if(type == "betweenness") {
    comms = betweenness(g)
  }
  dnet@communities = comms
  viewCommunities(dnet)
  return(comms)
}