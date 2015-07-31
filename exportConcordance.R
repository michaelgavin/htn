exportConcordance <- function(dc, filepath = NULL) {
  mat <- matrix("", 0, context * 2 + 2)
  for (i in 1:length(dc@concordance)) {
    doc <- names(dc@concordance)[i]
    kwics <- dc@concordance[doc][[1]]
    for (j in 1:length(kwics)) {
      if (length(kwics) == 1) {
        words <- rep(",", context * 2 + 1)
        words <- c(doc, words)
      } else {
        words <- c(doc,kwics[[j]])
      }
      mat <- rbind(mat, words)
    }
  }
  rownames(mat) <- NULL
  colnames(mat) <- c("Document", paste("Pos.", -context:context))
  if (class(filepath) == "character") {
    write.csv(mat, filepath, row.names = F)
  }
  return(mat)
}
