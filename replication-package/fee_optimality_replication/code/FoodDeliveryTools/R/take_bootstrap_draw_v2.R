take.bootstrap.draw.v2 <- function(dat.m, return.dat = FALSE){
    # Take a nonparametric bootstrap draw of restaurants

    ## Restaurant data.frames
    resto.m <- dat.m$resto.m
    resto.c <- dat.m$resto.c.m
    resto.i <- dat.m$resto.i.m

    resto.boot   <- resto.m
    resto.c.boot <- resto.c
    resto.i.boot <- resto.i

    G.vars <- grep('^G', colnames(resto.m), value = TRUE)
    NG <- length(G.vars)
    zips <- resto.m$zip
    for (z in zips){
        ## Chains
        idx.c <- which(resto.c$zip == z)
        if (length(idx.c) == 0){
            N.k.c     <- 0
            G.probs.c <- rep(0, times = NG)
        } else {
            G.vals.c  <- resto.c[idx.c, G.vars]
            N.k.c     <- sum(G.vals.c)
            G.probs.c <- G.vals.c/N.k.c
        }
        ## Independents
        idx.i <- which(resto.i$zip == z)
        if (length(idx.i) == 0){
            N.k.i     <- 0
            G.probs.i <- rep(0, times = NG)
        } else {
            G.vals.i <- resto.i[idx.i, G.vars]
            N.k.i     <- sum(G.vals.i)
            G.probs.i <- G.vals.i/N.k.i
        }

        if (N.k.c > 0){
            G.sample.c <- sample(1:NG, size = N.k.c, replace = TRUE,
                                 prob = G.probs.c)
            G.sample.c <- sapply(1:NG, function(g) sum(G.sample.c == g))
        } else {
            G.sample.c <- rep(0, times = NG)
        }

        if (N.k.i > 0){
            G.sample.i <- sample(1:NG, size = N.k.i, replace = TRUE,
                                 prob = G.probs.i)
            G.sample.i <- sapply(1:NG, function(g) sum(G.sample.i == g))
        } else {
            G.sample.i <- rep(0, times = NG)
        }

        ## Aggregate
        G.sample <- G.sample.c + G.sample.i

        ## Store results
        idx.agg <- which(resto.boot$zip == z)
        resto.boot[idx.agg, G.vars]   <- G.sample
        resto.c.boot[idx.c, G.vars] <- G.sample.c
        resto.i.boot[idx.i, G.vars] <- G.sample.i
    }

    # Update other fields of dat.m
    J.G.m <- generate.J.G(resto.boot)
    J.G.c <- generate.J.G(resto.c.boot)
    J.G.i <- generate.J.G(resto.i.boot)
    names(J.G.c) <- paste0(names(J.G.c), 'c')
    names(J.G.i) <- paste0(names(J.G.i), 'i')
    J.G.1 <- list()
    for (z in names(J.G.c)){
        J.G.1[[z]] <- J.G.c[[z]]
    }
    for (z in names(J.G.i)){
        J.G.1[[z]] <- J.G.i[[z]]
    }
    J.G.1 <- sort.list(J.G.1)

    Jp.G.m   <- map.J.to.Jp(J.G.m, zip.mat.m = dat.m$zip.mat.m)
    Jp.G.1.m <- map.J.to.Jp(J.G.1, zip.mat.m = dat.m$zip.mat.1.m)

    if (return.dat){
        dat.m$resto.m   <- resto.boot
        dat.m$resto.c.m <- resto.c.boot
        dat.m$resto.i.m <- resto.i.boot

        dat.m$J.G.m   <- J.G.m
        dat.m$J.G.1.m <- J.G.1

        dat.m$Jp.G.m   <- Jp.G.m
        dat.m$Jp.G.1.m <- Jp.G.1.m

        output <- dat.m
    } else {
        output <- list(J.G.m     = J.G.m,
                       J.G.1.m   = J.G.1.m,
                       Jp.G.m    = Jp.G.m,
                       Jp.G.1.m  = Jp.G.1.m,
                       resto.m   = resto.boot,
                       resto.c.m = resto.c.boot,
                       resto.i.m = resto.i.boot)
    }

    return(output)
}
