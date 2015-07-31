importEdges <- function(dnet, file, append = T, delim = "; ") {
  nameData <- read.csv(file)
  newNames <- list()
  for (i in 1:nrow(nameData)) {
    newNames[[i]] <- unlist(strsplit(nameData[i,1], sep = delim))
  }
  if (append == T) {
    dnet@names <- c(dnet@names, newNames)
  } else {
    dnet@names <- newNames
  }
  return(dnet)
}