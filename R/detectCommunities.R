#' Find communities in a graph
#' 
#' @slot dnet A document network
#' 
#' @slot type The algorithm to be used: options include "walktrap" and "betweenness".
#' 
#' @export
detectCommunities <- function(dnet, type="walktrap", view=T) {
  if(type == "walktrap") {
    comms = walktrap.community(dnet@graph)
  } else if(type == "edge.betweenness") {
    comms = edge.betweenness.community(dnet@graph)
  } else if(type == "betweenness") {
    comms = betweenness(dnet@graph)
  }
  dnet@communities = comms
  if(view == T) {
    viewCommunities(dnet)
  }
  return(comms)
}