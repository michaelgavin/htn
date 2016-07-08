#' Find communities in a graph
#' 
#' @param dnet A document network
#' 
#' @param type The algorithm to be used: options include "walktrap" and "betweenness".
#' 
#' @param view A logical that indicates whether you want to view the communities
#'             after detecting them or not. Default is True.
#' 
#' @return dnet@communities A \code{communities} object added into the slot
#'              that holds the community data detected.
#' 
#' @examples
#' communities = detectCommunities(dnet, type="edge.betweenness")
#' dnet@communities = detectCommunities(dnet, view=F)
#' 
#' @section Note:
#' This function includes only the "walktrap" and "edge.betweenness" options. For a
#' wider selection of community detection algorithms, consult the \code{igraph} package
#' directly. Communities identified using alternative methods can be added to a
#' \code{dnet} object by assigning them to the \code{dnet@communities} slot manually.
#' \strong{If you want options added, email the developer for requests.}
#' 
#' @examples
#' dnet@communities <- detectCommunities(dnet)
#' 
#' # For alternative detection algorithms
#' fg <- cluster_fast_greedy(simplify(dnet@graph))
#' dnet@communities <- fg
#' 
#' @export
detectCommunities <- function(dnet, type="walktrap") {
  if(type == "walktrap") {
    comms = walktrap.community(dnet@graph)
  } else if(type == "edge.betweenness") {
    comms = edge.betweenness.community(dnet@graph)
  } else if(type == "betweenness") {
    comms = betweenness(dnet@graph)
  }
  dnet@communities = comms
  return(comms)
}