docNetwork <- setClass("docNetwork",
                       slots = c(directory    = "character", 
                                 index        = "data.frame",
                                 edges        = "data.frame",
                                 persons      = "data.frame",
                                 graph        = "igraph",
                                 communities  = "communities"
                       )
)

