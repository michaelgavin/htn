#' Data - Edge list for EEBO-TCP data
#' 
#' A dataframe containing co-publication data for the EEBO-TCP
#' collection.
#' 
#' @format A dataframe with 57,752 observations and 5 variables. 
#' 
#' @section What it holds:
#' The \code{tcpEdges} data was generated algorithmically but was
#' also involved significant hand-correction. This correction is
#' ongoing and users who find errors are invited to bring them to
#' the attention to the developer.
#' 
#' The column names for the \code{tcpEdges} data are SOURCE, TARGET,
#' TYPE, TCP, and WEIGHT.
#' 
#' @examples
#' # Load the collection, then setup on your computer
#' data(tcpEdges)
#' View(tcpEdges)
"tcpEdges"
