# Construct the pairwise consumer-multihoming matrix
# (OA Table D1a in optimal_fees_OA.tex).

library(doBy)
library(EconTools)


main <- function(){
    inpath.dat <- 'data/est_dat/combined_sample_ig2021_v3.csv'
    outpath.pairwise <- 'output/explore_numerator/pairwise.csv'

    dat <- read.dat(inpath.dat)
    dat$id <- dat$USER_ID
    dat <- dat[, c('m', 'f', 'id', 't')]
    dat <- dat[which(dat$f != 0), ]

    # Recompute t
    dat <- dat[order(dat$id, dat$t), ]
    for (x in unique(dat$id)){
        idx <- which(dat$id == x)
        dat$t[idx] <- 1:length(idx)
    }

    dat$L.f <- c(NA, dat$f[1:(nrow(dat) - 1)])
    dat$L.f[which(dat$t == 1)] <- NA

    platforms <- 1:4
    platform.names <- c('DD', 'Uber', 'GH', 'PM')

    construct.pairwise.matrix(dat, platforms, platform.names, outpath.pairwise)
}


construct.pairwise.matrix <- function(dat, platforms, platform.names, outpath.pairwise){
    # Construct a matrix that reports the share of pairs
    # of adjacent orders in which g is present when f
    # is also present.

    # Construct dataset of pairs
    dat.pairs <- dat[which(!is.na(dat$L.f)), ]

    # Initialize outputs
    platform.shares <- c() ## Share of pairs including the platform
    pairwise <- list()     ## Share of pairs with each other platform
    for (f in platforms){
        idx.f <- which((dat.pairs$f == f) | (dat.pairs$L.f == f))
        platform.shares[f] <- length(idx.f)/nrow(dat.pairs)
        pairwise[[f]] <- sapply(platforms, function(g) mean((dat.pairs$f[idx.f]   == g) |
                                                            (dat.pairs$L.f[idx.f] == g)))
    }
    pairwise.mat <- as.data.frame(Reduce(rbind, pairwise))
    rownames(pairwise.mat) <- platform.names
    colnames(pairwise.mat) <- platform.names
    for (x in colnames(pairwise.mat)){
        pairwise.mat[[x]] <- sprintf('%0.2f', pairwise.mat[[x]])
    }

    pairwise.mat$shares <- sprintf('%0.2f', platform.shares)
    cnames <- colnames(pairwise.mat)
    pairwise.mat$platform <- platform.names
    pairwise.mat <- pairwise.mat[, c('platform', cnames)]

    write.dat(pairwise.mat, file = outpath.pairwise)
}


main()
