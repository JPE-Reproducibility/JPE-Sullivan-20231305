compute.market.G.shares <- function(profits, kappa, Mu.f = NULL, return.deviates = FALSE){
    # For each zipcode in a market, compute the share of restaurants that chooses
    # each portfolio G when the variable profits from G are as given by profit.G.
    #
    # Other inputs
    #   kappa: fixed cost parameters
    #   return.deviates: return the choice probabilities for each Mu.f draw?

    # Extract parameters
    K        <- kappa$K
    sigma.o  <- kappa$sigma.o
    sigma.rc <- kappa$sigma.rc

    n.mu <- nrow(Mu.f)

    zips <- names(profits)
    G.shares <- list()
    for (zip in zips){
        profits.mat <- pracma::repmat(profits[[zip]] - K[[zip]], n.mu, 1)
        W <- (profits.mat - sigma.rc*Mu.f)/sigma.o
        W.max <- apply(W, MARGIN = 1, max)
        W.max <- pracma::repmat(matrix(W.max, ncol = 1), 1, ncol(W))
        W.recentre <- W - W.max # To prevent e^W = Inf
        G.share.sim <- exp(W.recentre)/rowSums(exp(W.recentre))
        if (return.deviates){
            G.shares[[zip]] <- G.share.sim
        } else {
            G.shares[[zip]] <- colMeans(G.share.sim)
        }
        names(G.shares[[zip]]) <- names(K[[zip]])
    }
    return(G.shares)
}

