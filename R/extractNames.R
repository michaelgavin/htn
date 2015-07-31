#' Get co-author names
#' 
#' @export
extractNames <- function(dl, colNum = 0) {
  index   <- dl@index
  if(colNum == 0) {
    print("Column number not defined, trying dl@index$Author for authors.")
    authors <- index$Author # May need to create a condition. Needs warning message.
  } else {
    authors <- index[colNum]
  }
  names   <- data.frame() # Create df
  for (i in 1:length(authors)) {
    if(i > 1) {
      names = rbind(names, buildAuths(authors[i], index$TCP[i]))
    } else {
      names = buildAuths(authors[i], index$TCP[i])
    }
  }
  return(names)
}