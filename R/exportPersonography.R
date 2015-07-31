exportPersonography <- function(g, filename="personography.csv") {
  persons = V(g)
  names = c()
  for(i in 1:length(persons)){
    names[i] = persons[i]$name
  }
  write.csv(names, paste(dnet@directory, filename, sep="/"))
  names= data.frame(names)
  return(names)
}