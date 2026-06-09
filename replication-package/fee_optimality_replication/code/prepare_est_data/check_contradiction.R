# Check for "impossible" orders in the estimation sample
# (i.e., orders from platforms with no restaurants in range of the consumer)
library(EconTools)


main <- function(){

    months <- c('april', 'may', 'june')
    price.version <- '_v3'
    partner.only <- TRUE
    menu.price.type <- 'did'
    subsetting <- TRUE

    check.contradiction(months, price.version, partner.only,
                        menu.price.type, subsetting)
}


check.contradiction <- function(months, price.version, partner.only,
                                menu.price.type, subsetting, ig.year = '2021'){


    # Preliminaries
    nplatforms <- 4
    path.root <- 'data/est_dat/estimation_sample-yipitdata'
    if (ig.year != ''){
        path.root <- paste0(path.root, '_ig', ig.year)
    }
    rho.suffix <- paste0('_', menu.price.type)
    ssuffix <- ifelse(subsetting, '_sset', '')

    # Process each month
    for (month in months){
        if (partner.only){
            inpath  <- sprintf('%s%s_partnered%s%s_%s.csv', path.root,
                               price.version, rho.suffix, ssuffix, month)
            outpath <- sprintf('%s%s_possible%s%s_%s.csv',  path.root,
                               price.version, rho.suffix, ssuffix, month)
        } else {
            inpath  <- sprintf('%s%s%s%s_%s.csv',  path.root,
                               price.version, rho.suffix, ssuffix, month)
            outpath <- sprintf('%s%s%s%s_possible_nonp_%s.csv',  path.root,
                               price.version, rho.suffix, ssuffix, month)
        }

        dat <- read.dat(inpath, colClasses = c('zip' = 'character'))

        g.cols   <- grep('^G[0-9]{4}$',       colnames(dat), value = TRUE)
        g.cols.c <- grep('^G[0-9]{4}_chain$', colnames(dat), value = TRUE)
        g.cols.i <- grep('^G[0-9]{4}_indep$', colnames(dat), value = TRUE)

        # Construct the G.mat
        G.mat <- sub('G', '', g.cols)
        G.mat <- strsplit(G.mat, '')
        G.mat <- lapply(G.mat, as.numeric)
        G.mat <- Reduce(rbind, G.mat)

        impossible <- list()
        for (f in 1:nplatforms){
            cols <- g.cols[which(G.mat[, f] == 1)]
            nf <- rowSums(dat[, cols])
            f.var <- paste0('f', f)
            impossible[[f]] <- which((nf == 0) & (dat[, f.var] > 0))
        }

        # Impossibility of offline purchases
        impossible[[nplatforms + 1]] <- which(dat$J_total == 0)

        # Delete the impossible observations
        impossible <- Reduce(c, impossible)
        print(length(impossible))
        dat <- dat[setdiff(1:nrow(dat), impossible), ]

        ## Chain-specific
        impossible.c <- list()
        for (f in 1:nplatforms){
            cols <- g.cols.c[which(G.mat[, f] == 1)]
            nf <- rowSums(dat[, cols])
            f.var <- paste0('f', f, 'c')
            impossible.c[[f]] <- which((nf == 0) & (dat[, f.var] > 0))
        }
        impossible.c[[nplatforms + 1]] <- which(dat$J_total_chain == 0 &
                                                dat$f0c > 0)

        # Delete the impossible observations
        impossible.c <- Reduce(c, impossible.c)
        print(length(impossible.c))
        dat <- dat[setdiff(1:nrow(dat), impossible.c), ]

        ## Independent-specific
        impossible.i <- list()
        for (f in 1:nplatforms){
            cols <- g.cols.i[which(G.mat[, f] == 1)]
            nf <- rowSums(dat[, cols])
            f.var <- paste0('f', f, 'i')
            impossible.i[[f]] <- which((nf == 0) & (dat[, f.var] > 0))
        }
        impossible.i[[nplatforms + 1]] <- which(dat$J_total_indep == 0 &
                                                dat$f0i > 0)

        # Delete the impossible observations
        impossible.i <- Reduce(c, impossible.i)
        print(length(impossible.i))
        dat <- dat[setdiff(1:nrow(dat), impossible.i), ]

        cat('\n')
        write.dat(dat, outpath)
    }

}

main()
