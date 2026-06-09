estimate.restaurant.FCs.v2 <- function(dat, EPi, outpath, prob.spec = 'ccp',
                                       minimal = TRUE, outpath.plot = NULL,
                                       sigma.start = c(0.006, 0.003)){

    # Restaurant fixed cost estimation using a CCP-GMM estimator
    #
    # Inputs
    #   minimal: do not produce a plot?
    #   sigma.start: starting value for estimation algorithm for random
    #       coefficients model

    # Load data
    inpaths <- collect.inpaths()
    demo0   <- readRDS(inpaths$inpath.demo)
    geo     <- EconTools::read.dat(inpaths$inpath.geo,
                                   colClasses = c('zip' = 'character'))
    markets <- names(EPi)
    for (market in markets){
        EPi.m <- EPi[[market]]
        ## Make sure there are no NULL entries
        not.NULL <- which(!sapply(EPi.m, is.null))
        not.NULL.zips <- names(EPi.m)[not.NULL]
        # Remove missing EPis (these have no restaurants)
        nonempty.zips <- sapply(names(EPi.m), function(z) length(EPi.m[[z]]) > 0)
        nonempty.zips <- names(nonempty.zips)[which(nonempty.zips)]

        keep.zips <- intersect(not.NULL.zips, nonempty.zips)
        EPi.m <- lapply(not.NULL.zips, function(z) EPi.m[[z]])
        names(EPi.m) <- not.NULL.zips
        EPi[[market]] <- EPi.m
    }

    # Add variables to demo data.frames
    demo <- prepare.demo.data.v2(demo0, dat, EPi, geo, drop.na = TRUE)

    # Draw fixed cost random coefficients
    set.seed(1)
    Mu.f <- construct.FC.mu(dat[[1]]$G.mat, n.mu = 100)


    which.covs <- c(1, 2)
    fn <- function(sigma) RC.GMM.objective.v2(sigma, dat, EPi,
                                              demo, Mu.f,
                                              which.covs = which.covs)
    est <- optim(sigma.start, fn = fn,
                 control = list(maxit = 1000, abstol = 0.0005))

    sigma.hat <- est$par
    sigma.o   <- sigma.hat[1]
    sigma.rc  <- sigma.hat[2]

    K.vals <- FC.GMM.v2(sigma.o, dat, EPi, demo,
                        return.K = TRUE, verbose = FALSE, sigma.rc = sigma.rc,
                        Mu.f = Mu.f)

    # Sample size (number of restaurants)
    n <- sum(sapply(demo, function(x) sum(x$J_total)))

    # Collate estimates
    K.vec <- lapply(K.vals, function(x) x[2:length(x)])
    K.vec <- Reduce(c, K.vec)
    kappa <- c(K.vec, sigma.hat)

    # Save the outputs
    outputs <- list(K.vals = K.vals, sigma.hat = sigma.hat,
                    kappa = kappa, n = n)

    saveRDS(outputs, file = outpath)
}

