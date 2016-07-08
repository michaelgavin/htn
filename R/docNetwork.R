#' A network graph with corresponding bibliographical index
#'
#'  An S4 class that represents the network of a collection of
#'  documents.  This object stores the meta-data for the
#'  social/print network of a corpus.
#'  
#' @slot directory   A string that holds the full path to the corpus'
#'                  files.
#'                  
#' @slot index      A \code{data.frame} with the corpus' metadata.
#' 
#' @slot edges      A \code{data.frame} that stores the connections
#'                  between documents and people. In the format
#'                  \code{name},\code{TCP}.  Can include more fields.
#'                  
#' @slot persons    A \code{data.frame} that stores the authoritative
#'                  names and roles for each person in the network.
#'                  \code{ID}, \code{Role}.
#'                
#' @slot graph      An \code{igraph} object that holds the graph
#'                  of the corpus that is derived from \code{edges}.
#'                  
#' @slot communities A \code{communities} object that stores the
#'                   community data derived from \code{graph}.
#'
#' @export
docNetwork <- setClass("docNetwork",
                       slots = c(directory    = "character", 
                                 index        = "data.frame",
                                 edges        = "data.frame",
                                 persons      = "data.frame",
                                 graph        = "igraph",
                                 communities  = "communities"
                       )
)

print.communities <- function(comms) {
  n = comms$names
  m = comms$membership
  df = data.frame(n,m)[order(m),]
  row.names(df) = NULL
  View(df)
}


