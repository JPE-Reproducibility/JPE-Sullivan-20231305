RC.GMM.objective.v2 <- function(sigma, dat, EPi, demo, Mu.f, which.covs = c(1, 2)){
    # GMM objective for the restaurant-platform-adoption random-coefficients
    # model.
    # FC.GMM.v2 now returns exactly two covariance pairs (see its header);
    # which.covs selects which to include in the objective. The live FC
    # estimators pass c(1, 2) and so use both.
    sigma.o  <- sigma[1]
    sigma.rc <- sigma[2]

    Covs <- FC.GMM.v2(sigma.o, dat, EPi, demo,
                      return.covs = TRUE, verbose = FALSE,
                      sigma.rc = sigma.rc, Mu.f = Mu.f, delta.tol = 0.02)

    dists <- c()
    for (k in seq_along(which.covs)){
        cov.dat <- Covs[which.covs[k], 'data']
        cov.sim <- Covs[which.covs[k], 'sim']
        dists[k] <- cov.sim - cov.dat
    }

    fval <- sqrt(sum(dists^2))
    pracma::fprintf('\nsigma = (%f, %f): %f\n', sigma.o, sigma.rc, fval)
    print(dists)
    print(Covs[which.covs, , drop = FALSE])
    return(fval)
}
