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
#' @param edges A \code{data.frame} or string that holds the full path to
#'              a csv file with edge data in it. Must be in the form:
#'              \code{name}, \code{TCP} or other document identifier.
#'              Optional.
#'              
#' @param persons A \code{data.frame} or string that holds the full path to
#'                a csv file with persons data in it. Must be in the form:
#'                \code{ID}, \code{Role}. Optional.
#' 
#' @section How to use it:
#' For each of the parameters (other than the docList), the user can either 
#' provide a data.frame, a character string that connects to a .csv file, 
#' or let the default function extract the names.
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
    dnet@persons <- data.frame()#tcpPersons
  }
  dnet@graph <- buildGraph(dnet = dnet)
  dnet@communities <- detectCommunities(dnet, view=T)
  return(dnet)
}
