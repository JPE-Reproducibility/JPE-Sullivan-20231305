construct.FC.mu <- function(G.mat, n.mu = 100){
    # Take draws of mu random coefficient deviates

    ## Draws for each platform
    mu.f <- list()
    nplatforms <- ncol(G.mat) - 1
    nportfolios <- nrow(G.mat)
    ## Platform-specific deviates
    mu.f <- pracma::Reshape(rnorm(n.mu*nplatforms), n.mu, nplatforms)
    ## Portfolio-specific deviates
    Mu <- matrix(data = 0, nrow = n.mu, ncol = nportfolios)
    for (g in 1:nportfolios){
        idx <- which(G.mat[g, 2:(nplatforms + 1)] == 1)
        if (length(idx) == 1){
            Mu[, g] <- mu.f[, idx]
        } else if (length(idx) > 1){
            Mu[, g] <- rowSums(mu.f[, idx])
        }
    }
    return(Mu)
}

