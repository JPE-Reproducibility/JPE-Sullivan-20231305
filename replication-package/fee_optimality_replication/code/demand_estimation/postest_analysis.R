# Analyze the demand estimates
library(Matrix)
library(EconTools)
library(FoodDeliveryTools)

set.seed(1)

main <- function(){

    inpath.dat <- collect.inpaths(nsim.suffix = '_nsim50')$inpath.dat
    dat <- readRDS(inpath.dat)
    M <- length(dat)
    for (m in 1:M){
        run.postestimation.analysis(dat, m = m)
    }
    
    # All markets simultaneously
    base.dir <- 'output/demand_estimation/postest_analysis'
    subdirs <- setdiff(list.dirs(base.dir), base.dir)    
    Net.tabs <- list()
    Div.tabs <- list()
    NM <- length(subdirs)
    for (m in 1:NM){
        inpath.net <- sprintf('%s/elas-net_ext.csv', subdirs[m])
        inpath.div <- sprintf('%s/diversion-price.csv', subdirs[m])
        Net.tabs[[m]] <- read.dat(inpath.net)
        Div.tabs[[m]] <- read.dat(inpath.div)
    }
    
    # Average over tables
    Net.base <- Net.tabs[[1]]
    Div.base <- Div.tabs[[1]]
    
    
    avg.cols.net <- setdiff(colnames(Net.base), 'platform')
    avg.cols.div <- setdiff(colnames(Div.base), 'platform')
    for (k in avg.cols.net){
        Net.base[, k] <- as.numeric(Net.base[, k])/NM
    }
    for (k in avg.cols.div){
        Div.base[, k] <- as.numeric(Div.base[, k])/NM
    } 
    for (m in 2:NM){
        Net.m <- Net.tabs[[m]]
        Div.m <- Div.tabs[[m]]
        for (k in avg.cols.net){
            Net.base[, k] <- Net.base[, k] + Net.m[, k]/NM
        }
        for (k in avg.cols.div){
            Div.base[, k] <- Div.base[, k] + Div.m[, k]/NM
        }
    }
    
    for (k in avg.cols.net){
        Net.base[, k] <- sprintf('%0.2f', Net.base[, k])
    }
    for (k in avg.cols.div){
        Div.base[, k] <- sprintf('%0.2f', Div.base[, k])
    } 
    
    outpath.net <- sprintf('%s/net_ext_average.csv', base.dir)
    outpath.div <- sprintf('%s/div_average.csv', base.dir)
    # The paper's table reports only the platform columns
    write.dat(Net.base[, setdiff(colnames(Net.base), 'Offline')], outpath.net)
    write.dat(Div.base, outpath.div)
    
}

run.postestimation.analysis <- function(dat, m = 3){
    # Load data

    market <- names(dat)[m]

    # Specify paths to outputs
    outdir <- 'output/demand_estimation/postest_analysis'
    create.dir(outdir)
    market.label <- tolower(gsub(' ', '', sub('\\-.*$', '', market)))
    outdir <- sprintf('%s/%s', outdir, market.label)
    create.dir(outdir)

    inpath.dat <- collect.inpaths(nsim.suffix = '_nsim50')$inpath.dat
    print(inpath.dat)
    inpath.menu <- 'output/numerator_menu_pricing/disagg_results/price_indices.csv'

    outpath.elas.p    <- paste0(outdir, '/elas-price.csv')
    outpath.elas.J    <- paste0(outdir, '/elas-net_ext.csv')
    outpath.div.p     <- paste0(outdir, '/diversion-price.csv')
    outpath.div.J     <- paste0(outdir, '/diversion-net_ext.csv')
    outpath.choice.tab <- paste0(outdir, '/choice_tab.csv')
    outpath.next.plot <- paste0(outdir, '/next_plot.pdf')
    outpath.diversion <- paste0(outdir, '/diversion-%d.csv')
    outpath.outside   <- paste0(outdir, '/diversion_to_outside.csv')

    # Read in and process the price index
    Rho.dat <- read.dat(inpath.menu)

    ## Select a particular market
    dat.m <- dat[[market]]
    demand.param <- dat.m$demand.param
    ## Baseline prices: the r = 0.30 row of the consolidation price index.
    ## The online columns (dd/uber/gh/pm) are identical by construction,
    ## so dd stands in for the pooled online price.
    idx.30 <- which(Rho.dat$r == 0.30)
    Rhos.m <- as.numeric(Rho.dat[idx.30, c('dd', 'direct')])

    # Compute Rhos from data
    Rhos.base <- list()
    for (g in 1:dat.m$nportfolios){
        ng <- length(dat.m$G.indices[[g]])
        Rhos.base[[g]] <- matrix(rep(Rhos.m[1], times = ng), ncol = 1)
        Rhos.base[[g]][1] <- Rhos.m[2]
    }
    Rhos <- list()
    for (z in names(dat.m$J.G.1.m)){
        Rhos[[z]] <- Rhos.base
    }

    platform.names <- c('DD', 'Uber', 'GH', 'PM')

    # Compute and save elasticities to file
    elas.out <- compute.market.elasticities(dat.m, Rhos)
    elas.p <- elas.out$elas.p
    elas.J <- elas.out$elas.J
    div.p  <- elas.out$div.p
    div.J  <- elas.out$div.J
    elas.p.tab <- make.elas.table(elas.p, platform.names)
    elas.J.tab <- make.elas.table(elas.J, platform.names)
    div.p.tab  <- make.elas.table(div.p, platform.names)
    div.J.tab  <- make.elas.table(div.J, platform.names)
    write.dat(elas.p.tab, outpath.elas.p)
    write.dat(elas.J.tab, outpath.elas.J)
    write.dat(div.p.tab, outpath.div.p)
    write.dat(div.J.tab, outpath.div.J)

    # Also write the choice shares
    choice.tab <- as.data.frame(matrix(sprintf('%0.3f', elas.out$choice.shares), nrow = 1),
                                stringsAsFactors = FALSE)
    colnames(choice.tab) <- c('NoPurchase', 'Direct', 'Platform')
    write.dat(choice.tab, file = outpath.choice.tab)

    # Within-retaurant diversion ratios
    compute.diversion.ratios(dat.m, Rhos, outpath.diversion, platform.names)

    # Diversion to the outside restaurant
    compute.outside.ratios(dat.m, Rhos, outpath.outside, platform.names)
}

compute.market.elasticities <- function(dat.m, Rhos){
    # Platforms' elasticities, holding fixed portfolio allocations

    # Extract data
    G.mat <- dat.m$G.mat
    demand.param <- dat.m$demand.param

    fees       <- dat.m$fees
    buy.m      <- dat.m$buy.m
    zip.mat.m  <- dat.m$zip.mat.m
    J.G.m      <- dat.m$J.G.1.m
    nplatforms <- dat.m$nplatforms

    # Initialize outputs
    elas.p <- list() # Price elasticities
    div.p  <- list() # Price diversion ratios
    elas.J <- list() # Network externality elasticities
    div.J  <- list() # Network externality diversion ratios
    # Set numerical parameters
    p.eps <- 1e-8
    J.eps <- 1

    # Compute baseline sales
    opts <- list()
    sales.dat0 <- compute.zip.sales(dat.m, more.outputs = TRUE, Rhos = Rhos,
                                    opts = opts)
    q0 <- sales.dat0$Sales.platform
    Q0 <- c(sales.dat0$Sales.np.tot, q0)
    choice.shares <- Q0/sum(Q0)
    choice.shares <- c(choice.shares[1:2], sum(choice.shares[3:length(choice.shares)]))

    # Average prices
    avg.p <- sapply(1:(nplatforms - 1),
                    function(f) mean(sapply(names(fees), function(z) fees[[z]][f])))
    # Total number of restaurants on each platform
    nresto <- c()
    for (f in 1:nplatforms){
        idx.f <- which(G.mat[, f] == 1)
        nresto[f] <- sum(sapply(names(J.G.m), function(z) sum(J.G.m[[z]][idx.f])))
    }
    # Amount by which we will change the number of restaurants
    J.chg <- length(J.G.m)

    for (f in 2:nplatforms){
        # Compute price elasticities
        fees.1 <- fees
        for (z in names(fees)){
            fees.1[[z]][f - 1] <- fees.1[[z]][f - 1] + p.eps
        }
        dat.m1 <- dat.m
        dat.m1$fees <- fees.1

        sales.dat1 <- compute.zip.sales(dat.m1, more.outputs = TRUE, Rhos = Rhos,
                                        opts = opts)
        q1 <- sales.dat1$Sales.platform

        chg.q <- (q1 - q0)/q0
        chg.p <- p.eps/avg.p[f - 1]
        elas.p[[f - 1]] <- chg.q/chg.p

        ## Diversion ratios
        Q1 <- c(sales.dat1$Sales.np.tot, q1)
        diff.Q <- Q1 - Q0
        loss.f <- diff.Q[f + 1]
        div.p[[f - 1]] <- -diff.Q/loss.f

        # Compute network externality elasticities
        ## Portfolio with only the offline platform and f
        idx <- which(G.mat[, f] == 1 & rowSums(G.mat) == 2)
        ## Add a restaurant in every zip code
        J.G.m.adj <- J.G.m
        for (z in names(J.G.m)){
            J.G.m.adj[[z]][idx] <- J.G.m.adj[[z]][idx] + J.eps
        }

        # Create perturbed data structure
        dat.m1 <- dat.m
        dat.m1$J.G.1.m <- J.G.m.adj

        # Compute sales under perturbed data structure
        sales.dat1 <- compute.zip.sales(dat.m1, more.outputs = TRUE, Rhos = Rhos,
                                        opts = opts)

        q1 <- sales.dat1$Sales.platform
        chg.q <- (q1 - q0)/q0
        chg.JG <- J.chg/nresto[f]
        elas.J[[f - 1]] <- chg.q/chg.JG

        ## Diversion ratios
        Q1 <- c(sales.dat1$Sales.np.tot, q1)
        diff.Q <- Q1 - Q0
        loss.f <- diff.Q[f + 1]
        div.J[[f - 1]] <- diff.Q/loss.f
    }

    elas.out <- list(elas.J = elas.J, elas.p = elas.p,
                     div.J  = div.J,  div.p  = div.p,
                     choice.shares = choice.shares)
    return(elas.out)
}

make.elas.table <- function(elas, platform.names){
    # Construct a table reporting elasticities
    #
    # Inputs
    #   elas: List of elasticities
    #   platform.names: Names of platforms for generating row/column names

    elas.mat <- Reduce(rbind, elas)
    rownames(elas.mat) <- platform.names
    if (ncol(elas.mat) > 5){
        colnames(elas.mat) <- c('NoPurchase', 'Direct', platform.names)
    } else {
        colnames(elas.mat) <- c('Offline', platform.names)
    }
    elas.tab <- as.data.frame(elas.mat)
    cnames <- colnames(elas.tab)
    # Formatting
    for (cn in cnames){
        elas.tab[[cn]] <- sprintf('%0.2f', elas.tab[[cn]])
    }
    elas.tab$platform <- rownames(elas.tab)
    elas.tab  <- elas.tab[, c('platform', cnames)]
    return(elas.tab)
}

compute.diversion.ratios <- function(dat.m, Rhos, outpath.diversion, platform.names){
    # Compute within-restaurant diversion ratios

    ## Compute partial derivatives
    sales.dat <- restaurant.sales(dat.m, Rhos = Rhos)
    Deriv <- sales.dat$Deriv
    G.mat <- dat.m$G.mat

    ## Specify which diversion ratios to compute
    g.set <- c(2, 16)
    f.chg.set <- list()
    f.chg.set[['2']] <- c(1, 2)
    f.chg.set[['16']] <- c(1, 2, 3, 4, 5)

    diversion <- list()
    for (g in g.set){
        # Initialize output
        diversion[[as.character(g)]] <- list()

        for (f.chg in f.chg.set[[as.character(g)]]){
            DR <- list()
            for (z in names(Deriv)){
                if (is.null(dim(Deriv[[z]][[1]]))){
                    # Skip the zip code if there are no restaurants in it
                    next
                }
                # Compute diversion ratios
                own.deriv   <- Deriv[[z]][[g]][f.chg, f.chg]
                cross.deriv <- Deriv[[z]][[g]][, f.chg]
                DR[[z]] <- cross.deriv/(-own.deriv)
            }
            # Construct table
            DR.mat <- Reduce(rbind, DR)
            # Compute weights
            weights.DR <- sapply(names(DR), function(z) dat.m$J.G.1.m[[z]][g])
            # Compute mean
            diversion.g <- rep(0, times = ncol(DR.mat))
            for (r in 1:ncol(DR.mat)){
                diversion.g[r] <- weighted.mean(DR.mat[, r], w = weights.DR)
            }
            diversion[[as.character(g)]][[as.character(f.chg)]] <- diversion.g
        }
        diversion[[as.character(g)]] <- t(round(Reduce(rbind, diversion[[as.character(g)]]), 2))
    }

    # Produce table with results
    for (g in g.set){
        g.tab <- diversion[[as.character(g)]]
        overall <- rowSums(g.tab)
        g.tab <- as.data.frame(g.tab)
        for (k in 1:ncol(g.tab)){
            g.tab[, k] <- sprintf('%0.2f', g.tab[, k])
        }
        idx.g <- which(G.mat[g, ] == 1)
        g.names <- c('Offline', platform.names)[idx.g]
        colnames(g.tab) <- g.names
        g.tab$platform <- g.names
        g.tab <- g.tab[, c('platform', g.names)]

        outpath.g <- sprintf(outpath.diversion, g)
        write.dat(g.tab, file = outpath.g)
    }
}

compute.outside.ratios <- function(dat.m, Rhos, outpath.outside, platform.names){
    # Compute within-restaurant diversion ratios

    ## Compute partial derivatives
    sales.dat <- restaurant.sales(dat.m, Rhos = Rhos, outside = TRUE)
    Deriv <- sales.dat$Deriv
    Out   <- sales.dat$Outside
    G.mat <- dat.m$G.mat

    ## Specify which diversion ratios to compute
    g.set <- c(1, 16)
    f.chg.set <- list()
    f.chg.set[['1']] <- c(1)
    f.chg.set[['16']] <- c(1, 2, 3, 4, 5)

    diversion <- list()
    for (g in g.set){
        # Initialize output
        diversion[[as.character(g)]] <- list()

        for (f.chg in f.chg.set[[as.character(g)]]){
            DR <- list()
            for (z in names(Deriv)){
                if (is.null(dim(Deriv[[z]][[1]]))){
                    # Skip the zip code if there are no restaurants in it
                    next
                }
                # Compute diversion ratios
                own.deriv <- Deriv[[z]][[g]][f.chg, f.chg]
                if (own.deriv == 0){
                    # No restaurants on platform portfolio
                    next
                }
                out.deriv <- Out[[z]][[g]][f.chg]
                DR[[z]] <- out.deriv/(-own.deriv)
            }
            # Construct table
            DR.mat <- Reduce(rbind, DR)
            # Compute weights
            weights.DR <- sapply(names(DR), function(z) dat.m$J.G.1.m[[z]][g])
            # Compute mean
            diversion.g <- rep(0, times = ncol(DR.mat))
            for (r in 1:ncol(DR.mat)){
                diversion.g[r] <- weighted.mean(DR.mat[, r], w = weights.DR)
            }
            diversion[[as.character(g)]][[as.character(f.chg)]] <- diversion.g
        }
        diversion[[as.character(g)]] <- t(Reduce(rbind, diversion[[as.character(g)]]))
    }

    # Add additional rows
    ncols <- sapply(f.chg.set, length)
    ncols <- max(ncols)
    for (k in names(diversion)){
        div.k <- sprintf('%0.2f', as.numeric(diversion[[k]]))
        div.k <- c(div.k, rep('-', times = ncols - length(div.k)))
        diversion[[k]] <- div.k
    }

    # Produce table with results
    g.tab <- as.data.frame(Reduce(rbind, diversion), stringsAsFactors = FALSE)

    g.names <- c('Offline', platform.names)
    colnames(g.tab) <- g.names
    g.tab$platform <- c('No platform', 'All platforms')
    g.tab <- g.tab[, c('platform', g.names)]

    write.dat(g.tab, file = outpath.outside)

}


main()

