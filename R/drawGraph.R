#' Draw a social network graph
#' 
#' Creates a two-dimensional plot of a social-network graph.
#' 
#' @param dnet A docNetwork object
#' 
#' @param sub Logical. If true, only a subset of the graph will be printed.
#' 
#' @param   by A numeric or character vector. If only a subset of the graph will
#'          be drawn, \code{by} controls which part of the graph will be chosen
#'          for plotting. This can either take the form of an integer (in which
#'          case it represents the number of a sub-community within the graph)
#'          or it can be a vector of TCP ids (in which case it selects only a 
#'          subset of the document collection for graphing).
#'          
#' @param color  A string that defines the color of the nodes in the graph.
#' 
#' @param labelSize  A numeric value that sets the size of the labels for the nodes.
#' 
#' @param nodeSize   A numeric value that sets the size of the nodes.
#' 
#' @section How to use it:
#' \code{drawGraph} offers only a few options regarding layout. It's designed
#' to generate easy-to-read results quickly. More advanced plotting
#' options are available directly through \code{igraph}.
#' 
#' @export
drawGraph = function(g, comms = NULL, sub=F, by=0, color='blue', labelSize=.75, nodeSize=5) {
  if(sub == T) {
    if(class(by) == "character") {
      subg = subgraph.edges(g, eids=E(g)[which(E(g)$TCP %in% by)])
    } else {
      # If we're only passing in g, this is potentially problematic.
      # Changed this from dnet@communities.  Will require communities
      # to be passed in here if want to sub by communities.  Won't
      # affect code for graphing whole graph with comms.
      subg = subgraph(g, v=V(g)[which(membership(comms) == by)])
    }
    V(subg)$color = color
    V(subg)$label.cex = labelSize
    V(subg)$size = nodeSize
    plot(subg)
    return(subg)
  } else {
    # Comms colors still not quite working.  Having trouble with 
    # weird error: communities$membership '$ not valid for atomic..'
    # communities is not atomic and I'm not using '$' sign.
    if(!is.null(comms)) {
      print(is.atomic(comms))
      #mems = communities["membership"]
      palette = rainbow(max(membership(comms)))
      V(g)$color = palette[membership(comms)]
    } else {
      V(g)$color = color
    }
    V(g)$label.cex = labelSize
    V(g)$size = nodeSize
    #g$layout = layout.graphopt(g)
    plot(g)
  }
}

