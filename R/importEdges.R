#' Import edge data into a \code{docNetwork} object from a csv
#' file.  This is most useful for adding attributes to exported
#' edges or for adding new nodes.  It can also be used to import
#' a new set of network data by setting \code{append=F}.
#' 
#' @param dnet    The \code{docNetwork} object to modify.
#' 
#' @param file    The full path to the csv file that contains
#'                the new data. Should be in the format: name; tcp--or other id number.
#'                
#' @param append  A boolean that sets whether the data should be
#'                added to the \code{docNetwork} object's \code{edges}
#'                or if it should replace the existing--or non-existent-
#'                data.
#'
#' @param delim   A character string that shows how the names in your
#'                data are separated.
importEdges <- function(dnet, file, append = T, delim = ";") {
  edgeData <- read.csv(file, sep=delim)
  browser()
#   newNames <- list()
#   for (i in 1:nrow(nameData)) {
#     newNames[[i]] <- unlist(strsplit(nameData[i,1], sep = delim))
#   }
#   if (append == T) {
#     dnet@names <- c(dnet@names, newNames)
#   } else {
#     dnet@names <- newNames
#   }
  if(append == T) {
    dnet@edges <- rbind(dnet@edges, edgeData)
  } else {
    dnet@edges <- edgeData
  }
  return(dnet)
}