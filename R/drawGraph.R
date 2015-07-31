#' Draw a social network graph
#' 
#' Creates a two-dimensional plot of a social-network graph.
#' 
#' @slot dnet A docNetwork object
#' 
#' @slot sub Logical. If true, only a subset of the graph will be printed.
#' 
#' @slot by A numeric or character vector. If only a subset of the graph will
#'          be drawn, \code{by} controls which part of the graph will be chosen
#'          for plotting. This can either take the form of an integer (in which
#'          case it represents the number of a sub-community within the graph)
#'          or it can be a vector of TCP ids (in which case it selects only a 
#'          subset of the document collection for graphing).
#'          
#' @slot color
#' 
#' @slot labelSize
#' 
#' @section How to use it:
#' \code{drawGraph} offers only a few options regarding layout. It's designed
#' to generate easy-to-read results quickly. More advanced plotting
#' options are available directly through \code{igraph}.
#' 
#' @export
drawGraph = function(dnet, sub=F, by=0, color='blue', labelSize=.75) {
  if(sub == T) {
    if(class(by) == "character") {
      subg = subgraph.edges(dnet@graph, eids=E(dnet@graph)[which(E(dnet@graph)$TCP %in% by)])
    } else {
      subg = subgraph(g, v=V(dnet@graph)[which(dnet@communities$membership == by)])
    }
    V(subg)$color = color
    V(subg)$label.cex = labelSize
    plot(subg)
    return(subg)
  } else {
    V(dnet@graph)$color = color
    V(dnet@graph)$label.cex = labelSize
    plot(dnet@graph)
  }
}

