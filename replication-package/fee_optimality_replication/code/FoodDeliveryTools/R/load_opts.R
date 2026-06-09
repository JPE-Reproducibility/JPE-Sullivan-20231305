load.opts <- function(...){
    # Load various default options/settings used in model computations
    opts <- list()
    opts$p.CF            <- NULL
    opts$verbose         <- FALSE
    opts$resto.param     <- list(rho = 30, mc = 20)

    # Override some defaults with user-provided values
    x <- list(...)
    for (nm in names(x)){
        opts[[nm]] <- x[[nm]]
    }

    return(opts)
}
