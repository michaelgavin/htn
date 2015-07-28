# For each of the parameters (other than the docList), the user can either provide a data.frame, a character
# string that connects to a .csv file, or let the default function extract the names.

buildNetwork <- function(dl, names = extractNames(dl), edges = extractEdges(dnet), persons = extractPersons(dnet)) {
  dnet <- docNetwork()
  dnet@directory <- dl@directory
  dnet@index <- dl@index
  
  if (class(names) == "character" & length(names) == 1) { # If the parameter is set to a string, read csv
    names <- read.csv(names)
  } else {
    dnet@names <- names # otherwise just pass the value
  }
  
  if (class(edges) == "character" & length(edges) == 1) {
    edges <- read.csv(edges)
  } else {
    dnet@edges <- extractEdges(dnet)
  }
  
  if (class(persons) == "character" & length(persons) == 1) {
    persons <- read.csv(persons)
  } else {
    dnet@persons <- extractPersons(dnet)
  }
  return(dnet)
}

# Not sure how you'd like to pass parameters into extractNames(). 
extractNames <- function(dl, colNum = 0) {
  index   <- dl@index
  if(colNum == 0) {
    print("Column number not defined, trying dl@index$Author for authors.")
    authors <- index$Author # May need to create a condition. Needs warning message.
  } else {
    authors <- index[colNum]
  }
  names   <- list() # Create empty list
  for (i in 1:length(authors)) {
    names[[i]] <- buildAuths(authors[i], index$TCP[i])
  }
  return(names)
}

buildAuths = function(auths, tcp) {
  returnAuths = c()
  if(auths != "") {
    for(i in 1:length(auths)) {
      newAuths = as.character(auths[i])
      newAuths = strsplit(newAuths, split=';')
      for(j in 1:length(newAuths[[1]])) {
        auth = newAuths[[1]][j]
        auth = strsplit(auth, split=',')
        auth = unlist(auth)
        auth = trim.leading(auth)
        #print(auth)
        #print(auth[3])
        auth = c(paste(auth[2], auth[1], paste('(', auth[3], ')', sep=""), sep=" "), tcp)
        #print(auth)
        if(length(auth) >0) {
          returnAuths[[j]] = auth
        } else {
          returnAuths[j] = NA
        }
      }
    }
    names(returnAuths) = tcp
    return(returnAuths)
  } else {
    return(NA)
  }
}


extractEdges <- function(dnet) {
  # For each item in dnet@names, distribute into undirected edgelist
  if(class(dnet) == "docList") {
    names = extractNames(dnet)
    dnet = docNetwork()
    dnet@names = names
  }
  el = list()
  for(i in 1:length(dnet@names)){
    sorce = c()
    target = c()
    type = c()
    TCP = c()
    peeps = dnet@names[[i]]
    for(j in 1:length(peeps)){
      noti = peeps[-j]
      #browser()
      sorce = c(sorce,rep(peeps[[j]][1],length(noti)))
      for(k in 1:length(noti)){
        if(length(noti) >= 1){
          target = c(target,noti[[k]][1])
        } else {
          target = "NA"
        }
      }
      type = c(type, rep("undirected", length(noti)))
      TCP = c(TCP, rep(as.character(peeps[[j]][2]), length(noti)))
    }
    if(!(length(sorce) < 1)) {
      mat = cbind(sorce,target,type,TCP)
      el[[i]] = mat
    }
    
  }
  
  el = do.call(rbind, el)
  el = data.frame(el)
  
  el$weight = 1
  
  el = aggregate(el$weight,by=as.list(el[,1:4]),FUN=sum)
  names(el) = c("source","target","type", "TCP", "weight")
  browser()
  return(el)
}

extractPersons <- function(dnet) {
  if(class(dnet) == "docList") {
    names = extractNames(dnet)
    dnet = docNetwork()
    dnet@names = names
    edges = extractEdges(dnet)
    dnet@edges = edges
  }
  person = c()
  tcps   = c()
  for(i in 1:length(dnet@edges)) {
    name = dnet@edges$source[i]
    if(!(name %in% person)){
      person[i] = name
      browser()
      for(j in 1:length(dnet@edges)) {
        if(dnet@edges$source[[j]] == name && !(dnet@edges$TCP[j] %in% tcps[[i]])) {
          tcps[i][[j]] = dnet@edges$TCP[j]
        }
      }
    }
  }
  df = data.frame(person, tcps)
  names(df) = c("Person", "Tcps")
  return(df)
  # Pull the personography from the edgelist
}

addNames <- function(dnet, file, append = T, delim = "; ") {
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

# Adding and removing data from objs


addData <- function() {
  # Passes data from a data.frame to either dnet@edges or dnet@persons
  # For example, distribute results from MALLET across the edgelist
}


extractData <- function() {
  # This will take edgelist data and reduce it down to dl@index format, so 
  # users can filter books by network data, or authors / persons.
}

buildGraph <- function(dnet) {
  g <- graph.data.frame(dnet@edges, dnet@persons)
}


# QUESTION: We probably want some way of helping our users keep track of
# their network analysis objects (community objects, degree, etc). Perhaps
# another S4 class that holds them? What do you think?

