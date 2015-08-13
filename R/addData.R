#' A function that allows a user to append data to
#' either the \code{edges} or \code{persons} of a
#' \code{docNetwork} object and rebiulds the graph.
#' 
#' @param dnet     The \code{docNetwork} object to modify.
#' @param data     The data to add to the \code{docNetwork}
#'                 object.
#' @param edges    A logical that tells whether the user is
#'                 modifying the edges list or not. Default
#'                 is True.  Accepts a \code{data.frame},
#'                 a character vector of information, or
#'                 a path to a file.
#' @param persons  A logical that tells whether the user is
#'                 modifying the persons list or not. Default
#'                 is False. Accepts a \code{data.frame},
#'                 a character vector of information, or
#'                 a path to a file.
#'              
#' @return dnet    The modified \code{docNetwork} object.
#' 
#' @examples
#' dnet = addData(dnet, data)
#' dnet = addData(dnet, data, edges=F, persons=T)
#' dnet = addData(dnet, "/path/to/file.csv")
#' 
#' @export
addData <- function(dnet, data, edges=T, persons=F) {
  # Passes data from a data.frame to either dnet@edges or dnet@persons
  # For example, distribute results from MALLET across the edgelist
  if(edges == T){
    if(class(data) == "data.frame") {
      dnet@edges = cbind(dnet@edges, data)
    } else if(class(data) == "character") {
      if(file.exists(data)) { # If data is a path, read file.
        data = read.csv(data)
      }                    # Else, assume a data vector and add to edges
      dnet@edges = cbind(dnet@edges, data)
    }
  } else {
    if(class(data) == "data.frame") {
      dnet@persons = cbind(dnet@persons, data)
    } else if(class(data) == "character") {
      if(file.exists(data)) {
        data = read.csv(data)
      }
      dnet@persons = cbind(dnet@persons, data)
    }
  }
  dnet@graph = buildGraph(dnet, byVar=F)
  return(dnet)
}