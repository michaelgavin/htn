#' Convert publication data to network data
#' 
#' This function constructs a \code{docNetwork} object from
#' a \code{docList} object.  It will extract the edges and
#' persons, build the graph, and detect the communities 
#' within that graph.  The default type of community
#' detection is \code{walktrap}.
#' 
#' @param dl    A \code{docList} object that contains the \code{index}
#'              and other meta-data for the corpus that is to be
#'              networked.
#'              
#' @param edges A string that holds the full path to a csv file with edge 
#'              data in it. Note that, by default, this parameter is set
#'              to NULL, which tells \code{htn} to infer connections from
#'              bibliographic data in the \code{docList} index. \strong{If
#'              you want to supply your own data in .csv, it must be in a
#'              format \code{igraph} can read, with "source" and "target"
#'              columns clearly labeled.} See \code{igraph} documentation
#'              for using your own data.
#'              
#' @param persons A \code{data.frame} or string that holds the full path to
#'                a csv file with persons data in it. Must be in the form:
#'                \code{ID}, \code{Role}. Optional.
#'                
#' 
#' @section Warning:
#' Generally, this function is meant to be used with \code{tei2r} and the
#' EEBO-TCP data, from which \code{htn} includes data about co-publication. 
#' It's very good at taking the EEBO-TCP catalogue or some subset
#' of that catalogue and performing network analysis. Using \code{htn} for
#' any other collection would require compiling an appropriate
#' edge list and personography.
#'
#' @examples
#' dnet = buildNetwork(dl)
#' dnet = buildNetwork(dl, edges="path/to/edges.csv", persons="path/to/persons.csv")
#'  
#' @export
buildNetwork <- function(dl, edges = NULL, persons = NULL) {
  dnet <- docNetwork()
  dnet@directory <- dl@directory
  dnet@index <- dl@index
  
  if (class(edges) == "character" & length(edges) == 1) {
    edges <- read.csv(edges)
  } else {
    dnet@edges <- extractEdges(dnet)
  }
  
  if (class(persons) == "character" & length(persons) == 1) {
    persons <- read.csv(persons)
  } else {
    data(tcpPersons)
    dnet@persons <- extractPersons(dnet)
  }
  dnet@graph <- buildGraph(dnet = dnet)
  dnet <- detectCommunities(dnet)
  return(dnet)
}
