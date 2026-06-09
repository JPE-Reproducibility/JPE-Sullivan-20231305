

compute.pl.sales <- function(Sales, exclude.off = TRUE){
    s <- rowSums(sapply(Sales, function(x) apply(x, 2, sum)))
    if (exclude.off){
        s <- s[2:length(s)]
    }
    return(s)
}

compute.rho.bar <- function(FP, dat.m){
    rho.bar <- weight.rhos.v2(FP$Sales.tots, FP$Rhos, dat.m)
    rho.bar <- colMeans(rho.bar)
    rho.bar <- rho.bar[2:length(rho.bar)]
    return(rho.bar)
}


compute.kappa.bar <- function(eqm.objs.j, eqm){
    # weight kappa by online sales

    dat.m <- eqm.objs.j$dat.m
    Stot  <- eqm$FP$Sales.tots
    NF    <- length(eqm$C)
    mc.on <- eqm.objs.j$opts$resto.param$mc$on

    weights <- c()
    costs   <- c()
    for (j in 1:length(Stot)){
        # sales by ZIP
        S.j <- Stot[[j]][, 2:(NF + 1), ]
        if (length(dim(S.j)) == 3){
            s.by.z <- apply(S.j, 3, sum)
        } else {
            s.by.z <- sum(S.j)
            names(s.by.z) <- dimnames(Stot[[j]])[[3]]
        }
        # obtain kappas
        k.by.z <- mc.on[names(s.by.z)]

        weights <- c(weights, s.by.z)
        costs   <- c(costs,   k.by.z)
    }
    kbar <- weighted.mean(costs, weights)
    return(kbar)
}
