importPersonography <- function(dnet,path ="") {
  dnet@persons = read.csv(path)
}