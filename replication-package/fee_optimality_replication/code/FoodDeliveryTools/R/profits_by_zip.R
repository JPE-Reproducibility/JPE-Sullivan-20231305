profits.by.zip <- function(profits, J.G, K, zips0, keep.platforms = NULL, G.mat = NULL){

    if (is.null(keep.platforms)){
        keep.G <- 1:length(J.G[[1]])
    } else {
        keep.G <- determine.portfolios(G.mat, keep.platforms)
    }

    nportfolios <- length(J.G[[1]])

    pi.Jc <- c()
    pi.Ji <- c()
    J.c <- c()
    J.i <- c()
    for (z0 in zips0){
        pi.Jc[z0] <- 0
        pi.Ji[z0] <- 0
        J.c[z0]   <- 0
        J.i[z0]   <- 0
    }

    for (z0 in zips0){
        z.c <- paste0(z0, 'c')
        z.i <- paste0(z0, 'i')

        for (g in keep.G){
            if (z.c %in% names(J.G)){
                pi.zg <- profits[[z.c]][g]
                J.zg  <- J.G[[z.c]][g]
                pi.Jc[z0] <- pi.Jc[z0] + J.zg*(pi.zg  - K[[z.c]][g])
                J.c[z0] <- J.c[z0] + J.zg
            }
            if (z.i %in% names(J.G)){
                pi.zg <- profits[[z.i]][g]
                J.zg  <- J.G[[z.i]][g]
                pi.Ji[z0] <- pi.Ji[z0] + J.zg*(pi.zg  - K[[z.i]][g])
                J.i[z0] <- J.i[z0] + J.zg
            }
        }
    }
    pi.Jc.sum <- sum(pi.Jc)
    pi.Ji.sum <- sum(pi.Ji)
    outputs <- list(pi.Jc = pi.Jc, pi.Ji = pi.Ji,
                    J.i = J.i, J.c = J.c,
                    pi.Jc.sum = pi.Jc.sum,
                    pi.Ji.sum = pi.Ji.sum)
    return(outputs)
}

