#' A function that allows a user to extract data from
#' the \code{edges} of a \code{docNetwork} object and 
#' saves it to a file.
#' 
#' @param dnet     The \code{docNetwork} object to modify.
#' @param file     The full path to the file to write the
#'                 data to.
#' @param append   A logical that indicates whether the
#'                 data should be added to an existing
#'                 file or not.  Default is False.
#' @param sub      A logical that indicates whether the
#'                 desired data is a subset of the overall
#'                 network.
#' @param comms    The community data from a \code{docNetwork}
#'                 object to be used in subsetting the data.
#' @param by       The community number to use to subset the
#'                 data with.
#'              
#' @return df    A subset of \code{dnet}'s \code{index} for all nodes.
#' @return data  The vector of TCP numbers for a subset of the
#'               network's edges.
#' 
#' @examples
#' df = extractData(dnet, file="/path/to/file.csv")
#' tcps = extractData(dnet, file="/path/to/file.csv", sub=T, comms=dnet[at]communities, by=2)
#' dnet = extractData(dnet, "/path/to/file.csv", append=T)
#' 
#' @export
extractData <- function(dnet, file="", append=F, sub=F, comms=NULL, by=0) {
  # This will take edgelist data and reduce it down to dl@index format, so 
  # users can filter books by network data, or authors / persons.
  if(sub == F) {
    tcps = unique(dnet@edges$TCP)
    df = dnet@index[which(dnet@index$TCP %in% tcps),]
    if(append == T) {
      current = read.csv(file)
      if(file != ""){
        write.csv(df, file, append=T)
      }
    } else {
      if(file != ""){
        write.csv(df, file)
      }
    }
    return(df)
  } else {
    if(!is.null(comms)) {
      #Tried using incident, but it seems to only grab *some* of the TCPs whereas
      #making a subgraph gets all of them.
      subg = subgraph(dnet@graph, v=V(dnet@graph)[which(membership(comms) == by)])
      data = unique(E(subg)$TCP)
      if(file != ""){
        write.csv(data, file, append=append)
      }
      return(data)
    }
  }
}