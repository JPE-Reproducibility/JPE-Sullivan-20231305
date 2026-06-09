compute.logit.profits <- function(resto.profits, J.G, dat.m, kappa,
                                  keep.G = NULL){
    # Compute restaurant profits that include random coefficients and
    # the logit shock
    #
    # Inputs
    #   resto.profits: list of zip-specific restaurant variable profits
    #   J.G: restaurant location decisions
    #   dat.m: market-specific data
    #   kappa: fixed cost parameters

    if (is.null(keep.G)){
        keep.G <- 1:dat.m$nportfolios
    }

    # Extract parameters
    K        <- kappa$K
    sigma.rc <- kappa$sigma.rc
    sigma.o  <- kappa$sigma.o
    # Extract random coefficient deviates
    Mu.f <- dat.m$Mu.f[, keep.G]
    n.mu <- nrow(Mu.f)

    logit.profit <- 0
    zips <- names(resto.profits)
    for (z in zips){
        profits.mat <- pracma::repmat(resto.profits[[z]]- K[[z]], n.mu, 1)
        profits.mat <- profits.mat[, keep.G]
        profits.adj  <- profits.mat - sigma.rc*Mu.f

        expected.profit <- mean(sigma.o*log(rowSums(exp(profits.adj/sigma.o))))
        logit.profit  <- logit.profit + sum(J.G[[z]])*expected.profit
    }
    return(logit.profit)
}
