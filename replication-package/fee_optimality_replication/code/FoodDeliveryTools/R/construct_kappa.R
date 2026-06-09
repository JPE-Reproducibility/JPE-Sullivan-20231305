construct.kappa <- function(est, market, zips.m){
    # Construct a kappa list of fixed cost parameters
    kappa <- list()
    ## Sigmas
    if (length(est$sigma.hat) == 1){
        kappa$sigma.o  <- est$sigma.hat
        kappa$sigma.rc <- NULL
    } else {
        kappa$sigma.o  <- est$sigma.hat[1]
        kappa$sigma.rc <- est$sigma.hat[2]
    }

    ## Ks
    ### Determine whether there is restaurant heterogeneity
    K.base <- est$K.vals[[market]]
    if (length(K.base) == 2){
        if ('chain' %in% names(K.base)){
            rest.het <- TRUE
        } else {
            rest.het <- FALSE
        }
    } else {
        rest.het <- FALSE
    }
    if (rest.het){
        K <- list()
        for (z in zips.m){
            is.chain <- grepl('c$', z)
            if (is.chain){
                K[[z]] <- K.base$chain
            } else {
                K[[z]] <- K.base$indep
            }
        }
    } else {
        K <- lapply(zips.m, function(z) K.base)
        names(K) <- zips.m
    }
    kappa$K      <- K
    kappa$K.base <- K.base

    return(kappa)
}
