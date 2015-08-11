#' Find communities in a graph
#' 
#' @param dnet A document network
#' 
#' @param type The algorithm to be used: options include "walktrap" and "betweenness".
#' 
#' @param view A logical that indicates whether you want to view the communities
#'             after detecting them or not. Default is True.
#' 
#' @return comms The \code{communities} object that holds the communities that
#'               were detected in \code{g}
#' 
#' @examples
#' communities = detectCommunities(dnet, type="edge.betweenness")
#' communities = detectCommunities(dnet, view=F)
#' comms = detectCommunities(dnet)
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