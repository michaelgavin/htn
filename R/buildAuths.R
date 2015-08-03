# Creates a data frame of names and there associated TCP numbers
buildAuths = function(auths, tcp) {
  trim.leading <- function (x)  sub("^\\s+", "", x)
  names = c()
  tcps = c()
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
        sub = regexpr("[0-9]{2,}[.|\\?]{1}", auth[3])
        sub = substr(auth[3], sub, sub + attr(sub, "match.length"))
        name = paste(paste(auth[1], ",", sep=''), auth[2], sub, sep=" ")
        #print(auth)
        #browser()
        if(length(name) >0) {
          names[[j]] = name
        } else {
          names[j] = NA
        }
      }
    }
    #names(names) = tcp
    tcps = rep(tcp, length(names))
    df = data.frame(names, tcps)
    names(df) = c("name", "tcp")
    ##browser()
    return(df)
  } else {
    names = NA
    tcps = tcp
    df = data.frame(names, tcp)
    names(df) = c("name", "tcp")
    ##browser()
    return(df)
  }
}