docModel <- setClass("docModel",
                            slots = c(directory    = "character",
                                      index        = "data.frame",
                                      topics       = "data.frame",
                                      frequencies  = "matrix",
                                      weights      = "list",
                                      termMatrix   = "matrix",
                                      malletObj    = "jobjRef"
                            )
                     )

docNetwork <- setClass("docNetwork",
                       slots = c(directory  = "character", 
                                 index      = "data.frame",
                                 names      = "list",
                                 edges      = "data.frame",
                                 persons    = "data.frame")
                       )

