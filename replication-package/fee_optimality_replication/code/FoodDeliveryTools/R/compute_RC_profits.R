compute.RC.profits <- function(resto.profits, J.G, dat.m, kappa,
                               keep.G = NULL){
    # Compute restaurant profits that include random coefficients but
    # do not include the logit shock
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

    RC.r.profits <- 0
    RC.c.profits <- 0 # Chain
    RC.i.profits <- 0 # Indep

    zips <- names(resto.profits)
    for (z in zips){
        profits.mat <- pracma::repmat(resto.profits[[z]]- K[[z]], n.mu, 1)
        profits.mat <- profits.mat[, keep.G]
        profits.adj  <- profits.mat - sigma.rc*Mu.f

        # Compute choice probabilities
        W <- profits.adj/sigma.o
        W.max <- apply(W, MARGIN = 1, max)
        W.max.mat <- pracma::repmat(matrix(W.max, ncol = 1), 1, ncol(W))
        W.adj <- W - W.max
        choice.probs <- exp(W.adj)/rowSums(exp(W.adj))

        expected.profit <- mean(rowSums(profits.adj*choice.probs))
        expected.tot <- sum(J.G[[z]])*expected.profit
        RC.r.profits <- RC.r.profits + expected.tot
        if (grepl('c$', z)){
            RC.c.profits <- RC.c.profits + expected.tot
        } else if (grepl('i$', z)){
            RC.i.profits <- RC.i.profits + expected.tot
        }
    }
    output <- c(all   = RC.r.profits,
                chain = RC.c.profits,
                indep = RC.i.profits)
    return(output)
}
