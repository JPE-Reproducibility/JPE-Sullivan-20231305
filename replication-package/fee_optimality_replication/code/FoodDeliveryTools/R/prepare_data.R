prepare.data <- function(data.opts){
    # Prepare data used in the counterfactuals and restaurant-side estimation
    #
    # Inputs
    #   nsim: Number of simulations per consumer

    # Extract options
    nsim                <- data.opts$nsim
    cap.month           <- data.opts$cap.month
    importance.sampling <- data.opts$importance.sampling

    # Specify paths to data on restaurants' portfolio choices
    ## Number of restaurants in range of each zip code
    base.path <- 'data/est_dat/zcta_code_portfolios-yipitdata_w_infogroup2021_partnered_apr2021'
    inpath.ranges   <- sprintf('%s.csv',       base.path)
    inpath.ranges.c <- sprintf('%s_chain.csv', base.path)
    inpath.ranges.i <- sprintf('%s_indep.csv', base.path)

    ## Number of restaurants located within each zip code
    base.path <- 'data/est_dat/zcta_code_counts-yipitdata_w_infogroup2021_partnered_apr2021'
    inpath.locs    <- sprintf('%s.csv', base.path)
    inpath.locs.c  <- sprintf('%s_chain.csv', base.path)
    inpath.locs.i  <- sprintf('%s_indep.csv', base.path)

    # Specify paths to other data
    inpath.buy   <- 'data/est_dat/connect_sample_v3_hedonic_sset_0s.csv'
    inpath.fee <- 'data/prices/price_indices_v3.csv'
    inpath.geo   <- 'data/geo/geo.csv'
    inpath.zipmap <- 'data/geo/zcta_map.rds'
    est.dir <- 'output/demand_estimation'
    inpath.param <- sprintf('%s/est_table-yipitdata.csv', est.dir)
    inpath.cbsa.id <- 'data/est_dat/cbsa_ids.csv'
    inpath.caps.df <- 'data/fee_caps/monthly_fee_caps.csv'
    inpath.excl.chain <- 'data/fee_caps/zip_fee_caps.rds'

    ## Restaurant prices
    inpath.price <- 'output/numerator_menu_pricing/disagg_results/price_indices.csv'

    # For rescaling the overall level of sales
    inpath.rescale <- 'data/yipitdata/order_estimates_apr.csv'

    # Population distribution in the ACS
    inpath.dist <- 'data/eqm_data/pop_dist.csv'

    # Read in data
    ## Portfolio choice
    ### Nearby
    range.counts   <- EconTools::read.dat(inpath.ranges)
    range.counts.c <- EconTools::read.dat(inpath.ranges.c)
    range.counts.i <- EconTools::read.dat(inpath.ranges.i)

    range.counts$zip   <- EconTools::fix.zip.codes(range.counts$zip)
    range.counts.c$zip <- EconTools::fix.zip.codes(range.counts.c$zip)
    range.counts.i$zip <- EconTools::fix.zip.codes(range.counts.i$zip)

    ## Make a combined version for the case of restaurant heterogeneity
    rc.c <- range.counts.c
    rc.i <- range.counts.i
    idx <- grep('^[GJ]', colnames(rc.c))
    colnames(rc.c)[idx] <- paste0(colnames(rc.c)[idx], 'c')
    idx <- grep('^[GJ]', colnames(rc.i))
    colnames(rc.i)[idx] <- paste0(colnames(rc.i)[idx], 'i')
    range.counts.1 <- dplyr::full_join(rc.c, rc.i, by = 'zip')
    for (x in setdiff(colnames(range.counts), 'zip')){
        x.i <- paste0(x, 'i')
        x.c <- paste0(x, 'c')
        range.counts.1[, x] <- range.counts.1[, x.i] + range.counts.1[, x.c]
        range.counts.1[, x.i] <- NULL
        range.counts.1[, x.c] <- NULL
    }
    range.counts.11 <- range.counts.1
    range.counts.1$zip  <- paste0(range.counts.1$zip, 'c')
    range.counts.11$zip <- paste0(range.counts.11$zip, 'i')
    range.counts.1 <- dplyr::bind_rows(range.counts.1, range.counts.11)

    ### Within-ZCTA
    resto   <- EconTools::read.dat(inpath.locs)
    resto.c <- EconTools::read.dat(inpath.locs.c)
    resto.i <- EconTools::read.dat(inpath.locs.i)

    resto$zip   <- EconTools::fix.zip.codes(resto$zip)
    resto.c$zip <- EconTools::fix.zip.codes(resto.c$zip)
    resto.i$zip <- EconTools::fix.zip.codes(resto.i$zip)

    ### Version with alternative ZIPs that depend on type, e.g., 02138c and 02138i
    resto.c1 <- resto.c
    resto.i1 <- resto.i
    resto.c1$zip <- paste0(resto.c1$zip, 'c')
    resto.i1$zip <- paste0(resto.i1$zip, 'i')
    resto.1 <- dplyr::bind_rows(resto.c1, resto.i1)

    ## Other data
    buy     <- EconTools::read.dat(inpath.buy)
    geo     <- EconTools::read.dat(inpath.geo)
    fees  <- EconTools::read.dat(inpath.fee)
    zip.map <- readRDS(inpath.zipmap)
    ## Version with restaurant heterogeneity
    zip.map.1 <- expand.zip.map(zip.map)

    caps.df <- EconTools::read.dat(inpath.caps.df, colClasses = c('zip' = 'character'))
    cbsa.id <- EconTools::read.dat(inpath.cbsa.id, sep = '|')

    # Limit months included
    buy <- buy[which(buy$month == 4), ]

    # Demographic distribution in the ACS
    pop.dist <- EconTools::read.dat(inpath.dist, colClasses = c('zip' = 'character'))

    # Maximum number of orders per consumer/month (T in the paper)
    T.i <- 10

    orders.yd <- EconTools::read.dat(inpath.rescale, colClasses = c('zip' = 'character'))
    pop.unit <- 100000

    # Fix zip code variables
    buy$zip    <- EconTools::fix.zip.codes(buy$zip)
    geo$zip    <- EconTools::fix.zip.codes(geo$zip)
    fees$zip <- EconTools::fix.zip.codes(fees$zip)

    # Process data on counts of restaurants within range of various zip codes
    ## Overall
    zip.range.dat <- process.zip.range(cbsa.id, geo, zip.map, resto, range.counts)
    zip.mats      <- zip.range.dat$zip.mats
    ## Overall with ZIP/type pairs
    zip.range.dat.1 <- process.zip.range(cbsa.id, geo, zip.map.1, resto.1, range.counts.1)
    zip.mats.1      <- zip.range.dat.1$zip.mats

    ## Chain restaurants
    zip.range.dat.c <- process.zip.range(cbsa.id, geo, zip.map, resto.c, range.counts.c)
    zip.mats.c      <- zip.range.dat.c$zip.mats
    ## Independent restaurants
    zip.range.dat.i <- process.zip.range(cbsa.id, geo, zip.map, resto.i, range.counts.i)
    zip.mats.i      <- zip.range.dat.i$zip.mats

    resto   <- dplyr::left_join(resto,   geo, by = 'zip')
    resto.c <- dplyr::left_join(resto.c, geo, by = 'zip')
    resto.i <- dplyr::left_join(resto.i, geo, by = 'zip')
    ## In order to merge geo into the overall/separate ZIPs data,
    ## we need to add an "original ZIP" variable (e.g., 02138c -> 02138)
    geo$zip0 <- geo$zip
    resto.1$zip0 <- substr(resto.1$zip, 1, 5)
    resto.1  <- dplyr::left_join(resto.1, geo[, setdiff(colnames(geo), 'zip')],
                                 by = 'zip0')

    # Process data on commission caps
    caps.df <- caps.df[which(caps.df$month == cap.month), ]
    caps.df <- caps.df[, c('cap', 'zip')]
    caps.df <- EconTools::rename.var(caps.df, 'cap', 'caps')

    # Process data on exclusion of chain restaurants from commission caps
    excl.chain.df <- readRDS(inpath.excl.chain)
    excl.chain.df <- excl.chain.df$zip.df
    excl.chain.df <- excl.chain.df[, c('zip', 'date', 'excl_chains')]
    ## Fix dates in excl.chain (which is weekly, whereas cap.month is a month)
    excl.chain.df$month <- sub('-[0-9]{2}$', '', excl.chain.df$date)
    excl.chain.df <- doBy::summaryBy(excl_chains ~ zip + month,
                                     data = excl.chain.df, FUN = max,
                                     keep.names = TRUE)

    cap.month0 <-  sub('-[0-9]{2}$', '', cap.month)
    excl.chain.df <- excl.chain.df[which(excl.chain.df$month == cap.month0), ]
    excl.chain <- c()
    for (z0 in excl.chain.df$zip){
        excl.chain[z0] <- excl.chain.df$excl_chains[which(excl.chain.df$zip == z0)]
    }

    # Consumer choice parameters
    est <- EconTools::read.dat(inpath.param)
    demand.param <- load.param(est, cbsa.id)

    buy0 <- buy

    # Intended for incl.np mode
    pop.dist$pop_over_15 <- NULL
    pop.dist <- pop.dist[which(pop.dist$count > 0), ]
    buy <- pop.dist

    # Draw zetas and etas
    buy <- add.sim.dat(buy, nsim, demand.param = demand.param,
                       importance.sampling = importance.sampling)

    # Obtain market-specific datasets to avoid repeated subsetting
    CBSAs <- cbsa.id$CBSA_name
    M <- length(CBSAs)

    ## Initialize outputs
    ### Restaurants' portfolios
    Resto   <- list() # All
    Resto.1 <- list() # All, with separate ZIPs by restaurant type
    Resto.i <- list() # Chain
    Resto.c <- list() # Independent

    Fees <- list() # Prices charged to consumers by platforms
    WTs  <- list() # Waiting times

    colnames(fees) <- sub('\\.price$', '', colnames(fees))
    p.vars <- c('dd', 'uber', 'gh', 'pm')
    wt.vars <- sprintf('WT_%s_lasso', p.vars)
    for (m in 1:M){
        market <- CBSAs[m]

        # Consumer/restaurant choice data
        Resto[[market]]   <- resto[which(resto$CBSA_name == market), ]
        Resto.1[[market]] <- resto.1[which(resto.1$CBSA_name == market), ]
        Resto.c[[market]] <- resto.c[which(resto.c$CBSA_name == market), ]
        Resto.i[[market]] <- resto.i[which(resto.i$CBSA_name == market), ]

        # Prices
        fees.m <- fees[which(fees$CBSA_name == market), ]
        N.m <- nrow(fees.m)
        zips.m <- fees.m$zip
        F.m <- lapply(1:N.m, function(k) as.numeric.keep.names(fees.m[k, p.vars]))
        names(F.m) <- zips.m
        Fees[[market]] <- F.m

        # Waiting times
        WTs.m <- lapply(1:N.m, function(k) as.numeric.keep.names(fees.m[k, wt.vars]))
        for (k in 1:length(WTs.m)){
            names(WTs.m[[k]]) <- gsub('(WT_|_lasso)', '', names(WTs.m[[k]]))
        }
        names(WTs.m) <- zips.m
        WTs[[market]] <- WTs.m
    }

    portfolios <- grep('^G', colnames(resto), value = TRUE)
    G.mat <- generate.membership.mat(portfolios)

    # Some indices
    G.indices <- list()  # Indices of platforms belonging to each portfolio
    fg.indices <- list() # Index of each platform within portfolio G
    nportfolios <- nrow(G.mat)
    for (g in 1:nportfolios){
        # Determine platforms belonging to the portfolio
        G.indices[[g]] <- which(G.mat[g, ] == 1)

        fg.indices[[g]] <- list()
        for (f in G.indices[[g]]){
            fg.indices[[g]][[f]] <- which(G.indices[[g]] == f)
        }
    }

    # Merge waiting times into buy
    WT.dfs <- list()
    for (market in CBSAs){
        WT.df <- Reduce(rbind, WTs[[market]])
        WT.df <- as.data.frame(WT.df)
        colnames(WT.df) <- sprintf('WT_%d', 1:ncol(WT.df))
        WT.df$zip <- names(WTs[[market]])
        WT.dfs[[market]] <- WT.df
    }
    WT.df <- Reduce(rbind, WT.dfs)
    buy <- dplyr::left_join(buy, WT.df, by = 'zip')

    Buy <- list()
    for (m in 1:M){
        market <- CBSAs[m]
        Buy[[market]]   <- buy[which(buy$m == m), ]
    }

    # Random coefficient fixed cost shocks
    set.seed(1)
    Mu.f <- construct.FC.mu(G.mat, n.mu = 100)

    # Compile outputs
    dat <- list(buy            = buy,
                cbsa.id        = cbsa.id,
                demand.param   = demand.param,
                caps.df        = caps.df,
                excl.chain     = excl.chain,
                resto          = resto,
                resto.1        = resto.1, # version with chain/indep as different ZIPs
                resto.c        = resto.c,
                resto.i        = resto.i,
                range.counts   = range.counts,
                range.counts.1 = range.counts.1,
                range.counts.c = range.counts.c,
                range.counts.i = range.counts.i,
                prices         = fees,
                geo            = geo,
                zip.mats       = zip.mats,
                zip.mats.1     = zip.mats.1,
                zip.map        = zip.map,
                zip.map.1      = zip.map.1,
                Buy            = Buy,
                Resto          = Resto,
                Resto.1        = Resto.1,
                Resto.c        = Resto.c,
                Resto.i        = Resto.i,
                Fees           = Fees,
                WTs            = WTs,
                G.mat          = G.mat,
                G.indices      = G.indices,
                fg.indices     = fg.indices,
                T.i            = T.i,
                Mu.f           = Mu.f)

    # Relayer the output data
    {
        mkt.lvl <- list()
        nplatforms  <- ncol(G.mat)
        for (m in 1:M){
            # Initialize the list of market-specific data
            market <- CBSAs[m]
            mkt.lvl[[market]] <- list()

            # Construct the J.G and Jp.G matrices
            resto.m <- dat$Resto[[market]]
            J.G.m   <- generate.J.G(resto.m)
            Jp.G.m  <- map.J.to.Jp(J.G.m, zip.mat.m = dat$zip.mats[[market]])

            resto.1.m <- dat$Resto.1[[market]]
            J.G.1.m   <- generate.J.G(resto.1.m)
            Jp.G.1.m  <- map.J.to.Jp(J.G.1.m, zip.mat.m = dat$zip.mats.1[[market]])

            ## Chains
            resto.c.m <- dat$Resto.c[[market]]
            J.G.c.m   <- generate.J.G(resto.c.m)
            Jp.G.c.m  <- map.J.to.Jp(J.G.c.m, zip.mat.m = dat$zip.mats[[market]])
            ## Independents
            resto.i.m <- dat$Resto.i[[market]]
            J.G.i.m   <- generate.J.G(resto.i.m)
            Jp.G.i.m  <- map.J.to.Jp(J.G.i.m, zip.mat.m = dat$zip.mats[[market]])

            # Consider chains and independents as separate ZIPs
            names(J.G.c.m)  <- paste0(names(J.G.c.m),  'c')
            names(J.G.i.m)  <- paste0(names(J.G.i.m),  'i')
            names(Jp.G.c.m) <- paste0(names(Jp.G.c.m), 'c')
            names(Jp.G.i.m) <- paste0(names(Jp.G.i.m), 'i')

            ## Market-specific fee caps
            ### Extract ZIP mappings
            zip.mat.m   <- dat$zip.mats[[market]]
            zip.mat.1.m <- dat$zip.mats.1[[market]]
            ### Produce a list of ZIPs
            zips.m   <- rownames(zip.mat.m)
            zips.1.m <- rownames(zip.mat.1.m)

            zips.m.df   <- data.frame(zip = zips.m)
            zips.1.m.df <- data.frame(zip = zips.1.m)

            caps.df.m <- dplyr::inner_join(zips.m.df,   caps.df, by = 'zip')

            caps.df.1 <- caps.df
            caps.df.1 <- EconTools::rename.var(caps.df.1, 'zip', 'zip0')
            zips.1.m.df$zip0 <- substr(zips.1.m.df$zip, 1, 5)
            caps.df.1.m <- dplyr::inner_join(zips.1.m.df, caps.df.1, by = 'zip0')

            caps.m   <- list()
            caps.1.m <- list()

            excl.chain.m <- c()

            for (zip in caps.df.m$zip){
                cap.mz <- caps.df.m$caps[which(caps.df.m$zip == zip)]
                caps.m[[zip]] <- rep(cap.mz, times = nplatforms - 1)
                names(caps.m[[zip]]) <- c('dd', 'uber', 'gh', 'pm')

                excl.chain.m[zip] <- excl.chain[zip]
            }
            for (zip1 in caps.df.1.m$zip){
                cap.mz <- caps.df.1.m$caps[which(caps.df.1.m$zip == zip1)]
                caps.1.m[[zip1]] <- rep(cap.mz, times = nplatforms - 1)
                names(caps.1.m[[zip1]]) <- c('dd', 'uber', 'gh', 'pm')
            }

            ## Market-specific zip code mapping
            zip.map.m <- lapply(zips.m, function(z) zip.map[[z]])
            names(zip.map.m) <- zips.m

            zip.map.1.m <- lapply(zips.1.m, function(z) zip.map.1[[z]])
            names(zip.map.1.m) <- zips.1.m

            # Fill in the list of market-specific data
            buy.m <-  dat$Buy[[market]]
            mkt.lvl[[market]]$buy.m       <- buy.m
            mkt.lvl[[market]]$zip.mat.m   <- zip.mat.m
            mkt.lvl[[market]]$zip.mat.1.m <- zip.mat.1.m
            mkt.lvl[[market]]$fees        <- dat$Fees[[market]]
            mkt.lvl[[market]]$WTs.m       <- dat$WTs[[market]]

            # Restaurants
            ## All
            mkt.lvl[[market]]$resto.m     <- resto.m
            mkt.lvl[[market]]$J.G.m       <- J.G.m
            mkt.lvl[[market]]$Jp.G.m      <- Jp.G.m
            mkt.lvl[[market]]$J.G.1.m     <- J.G.1.m
            mkt.lvl[[market]]$Jp.G.1.m    <- Jp.G.1.m
            ## Chain
            mkt.lvl[[market]]$resto.c.m   <- resto.c.m
            mkt.lvl[[market]]$J.G.c.m     <- J.G.c.m
            mkt.lvl[[market]]$Jp.G.c.m    <- Jp.G.c.m
            ## Independent
            mkt.lvl[[market]]$resto.i.m   <- resto.i.m
            mkt.lvl[[market]]$J.G.i.m     <- J.G.i.m
            mkt.lvl[[market]]$Jp.G.i.m    <- Jp.G.i.m

            mkt.lvl[[market]]$zip.map     <- zip.map.m
            mkt.lvl[[market]]$zip.map.1   <- zip.map.1.m
            mkt.lvl[[market]]$caps.df     <- caps.df.m
            mkt.lvl[[market]]$caps.df.1   <- caps.df.1.m
            mkt.lvl[[market]]$excl.chain  <- excl.chain.m
            mkt.lvl[[market]]$G.mat       <- G.mat
            mkt.lvl[[market]]$nplatforms  <- nplatforms
            mkt.lvl[[market]]$nportfolios <- nportfolios
            mkt.lvl[[market]]$portfolios  <- portfolios
            mkt.lvl[[market]]$nbuy        <- compute.nbuy(dat$Buy[[market]])
            mkt.lvl[[market]]$G.indices   <- G.indices
            mkt.lvl[[market]]$fg.indices  <- fg.indices
            mkt.lvl[[market]]$T.i         <- T.i
            mkt.lvl[[market]]$Mu.f        <- Mu.f

            ## Commissions
            comm.zips <- names(J.G.1.m)
            comm <- list()
            for (cz in comm.zips){
                idx <- which(caps.df.1.m$zip == cz)
                comm.z <- caps.df.1.m$caps[idx]
                comm[[cz]] <- rep(comm.z, times = nplatforms - 1)
            }
            mkt.lvl[[market]]$comm <- comm

            # Region-specific buy
            buy.zip <- list()
            for (z in zips.m){
                buy.zip[[z]] <- buy.m[which(buy.m$zip == z), ]
            }
            mkt.lvl[[market]]$buy.zip <- buy.zip
            # Demand parameters
            demand.param.m     <- demand.param
            demand.param.m$psi <- demand.param.m$Psi[[market]]
            if (('MuEta' %in% names(demand.param.m)) & (length(demand.param.m[['MuEta']]) > 1)){
                demand.param.m$mu.eta <- demand.param.m$MuEta[market]
            }
            mkt.lvl[[market]]$demand.param <- demand.param.m
        }

        # Override dat
        dat <- mkt.lvl
    }

    # Rescale purchases to match the YipitData dataset
    rhos <- EconTools::read.dat(inpath.price)
    opts <- load.opts()
    ## Estimate restaurant marginal costs and the phi parameter
    outputs <- phi.bisection(dat, rhos, opts)


    for (m in 1:M){
        market <- names(dat)[m]
        dat.m <- dat[[market]]
        cap.df <- dat.m$caps.df.1

        # Extract prices
        Rhos <- list() ## initialize list of prices
        zips.m <- names(dat.m$Jp.G.1.m)

        pl.names <- c('direct', 'dd', 'uber', 'gh', 'pm')
        dat.m$phi <- outputs$phi

        for (z in zips.m){
            Rhos[[z]] <- list()

            # Determine which commissions apply
            comm.z <- cap.df$caps[which(cap.df$zip == z)]

            # Specify prices accordingly
            idx.rhos <- which(rhos$r == comm.z)
            rhos.z <- as.numeric(rhos[idx.rhos, pl.names])
            names(rhos.z) <- pl.names

            for (g in 1:length(dat.m$G.indices)){
                g.idx <- dat.m$G.indices[[g]]
                g.names <- pl.names[g.idx]
                NG <- length(g.idx)
                if (g == 1){
                    Rhos[[z]][[g]] <- rhos.z[1]
                } else {
                    Rhos[[z]][[g]] <- c(rhos.z[1],
                                        rep(rhos.z[2], times = NG - 1))
                }
                names(Rhos[[z]][[g]]) <- g.names
            }
        }

        # Prepare the MC.m list
        costs.m <- outputs$costs[[market]]
        mc.on  <- costs.m$costs.on
        mc.off <- costs.m$costs.off
        MC.m <- list(on = mc.on, off = mc.off)

        pracma::fprintf("%s mean costs = %f (on), %f (off)\n",
                        market, mean(mc.on), mean(mc.off))


        num.param <- load.num.param()
        opts <- load.opts(verbose = TRUE)
        opts$resto.param$mc <- MC.m

        eqm.m <- compute.market.sales(dat.m, num.param, opts,
                                      J.G.m = dat.m$J.G.1.m)

        Rhos <- eqm.m$Rhos
        online.sales.m <- sum(eqm.m$Sales.platform[2:5])
        yipit.sales.m <- sum(orders.yd$orders_YD[which(orders.yd$m == m)])
        multiplier <- yipit.sales.m/online.sales.m
        dat.m$buy.m$count <- dat.m$buy.m$count*(multiplier/pop.unit)
        for (z in names(dat.m$buy.zip)){
            dat.m$buy.zip[[z]]$count <- dat.m$buy.zip[[z]]$count*multiplier/pop.unit
        }
        dat.m$eqm.weighting <- eqm.m
        dat.m$markups <- costs.m$markups
        dat.m$eqm.inputs <- list(num.param = num.param,
                                 opts      = opts,
                                 J.G.m     = dat.m$J.G.1.m,
                                 Jp.G.m    = dat.m$Jp.G.1.m)
        dat.m$CCP.dat <- NULL
        dat[[market]] <- dat.m
    }

    return(dat)
}

process.zip.range <- function(cbsa.id, geo, zip.map, resto, range.counts){
    # Process certain objects characterizing the zip codes nearby each zip code
    #
    # Inputs
    #   cbsa.id: Table giving CBSA names and ID codes
    #   geo: data.frame with geographical data at the zipcode level
    #   zip.map: List giving the zipcodes that are in range of each zipcode
    #   resto: data.frame of zipcode/portfolio-specific restaurant counts
    #   range.counts: data.frame with counts of restaurants on certain portfolios
    #       within range of each zipcode
    #
    # Outputs
    #   range.counts1: list of market-specific subsets of range.counts
    #   range.counts2: like range.counts1, but computed using pairwise distances between
    #       zip codes rather than distances between zip codes and restaurants
    #   zip.mats: market-specific matrices encoding the information in zip.map

    # Initialize outputs
    range.counts1 <- list()
    range.counts2 <- list()
    zip.mats      <- list()

    # Loop over markets
    M <- nrow(cbsa.id)
    for (m in 1:M){
        market <- cbsa.id$CBSA_name[m]

        cbsa.zips <- geo$zip[which(geo$CBSA_name == market)]

        if (grepl('(c|i)$', names(zip.map)[1])){
            # Restaurant heterogeneity
            cbsa.zips <- c(paste0(cbsa.zips, 'i'),
                           paste0(cbsa.zips, 'c'))

        }
        cbsa.zips <- intersect(cbsa.zips, names(zip.map))
        cbsa.zips <- intersect(cbsa.zips, range.counts$zip)
        cbsa.zips <- intersect(cbsa.zips, resto$zip)

        cbsa.zips <- sort(cbsa.zips)
        nzips <- length(cbsa.zips)
        zip.mat <- pracma::zeros(nzips, nzips)
        for (j in 1:nzips){
            zip.j <- cbsa.zips[j]
            nearby.zips <- zip.map[[zip.j]]
            nearby.idx <- which(cbsa.zips %in% nearby.zips)
            zip.mat[nearby.idx, j] <- 1
        }
        rownames(zip.mat) <- colnames(zip.mat) <- cbsa.zips

        range.m <- range.counts[which(range.counts$zip %in% cbsa.zips), ]
        loc.m   <- resto[which(resto$zip %in% cbsa.zips), ]

        rownames(range.m) <- range.m$zip
        rownames(loc.m)   <- loc.m$zip
        idx <- setdiff(colnames(loc.m), 'zip')
        range.m <- range.m[order(range.m$zip), idx]
        loc.m   <- loc.m[order(loc.m$zip), idx]

        range.counts1[[market]] <- range.m
        range.counts2[[market]] <- as.data.frame(zip.mat%*%as.matrix(loc.m))
        zip.mats[[market]]      <- Matrix::Matrix(zip.mat, sparse = TRUE)
    }
    out <- list(range.counts1 = range.counts1,
                range.counts2 = range.counts2,
                zip.mats      = zip.mats)
    return(out)
}


add.sim.dat <- function(buy, nsim, demand.param, nplatforms = 5, importance.sampling = FALSE){
    # Add zeta simulates to the "buy" dataset of consumer transactions

    demo.vars <- c('young', 'married', 'high_income')
    demo.vars <- intersect(demo.vars, colnames(buy))
    if ('id' %in% colnames(buy)){
        # Old way (before buy was "collapsed" into zip x demo bins)

        buy <- buy[, c('zip', 'id', demo.vars, 'm', 'f', 't')]
    } else {
        # Simulation method when buy is collapsed into zip x demo bins:
        # Generate fictitious consumers who represent may consumers in practice
        buy <- buy[, c('zip', demo.vars, 'm', 'count')]
        buy$id <- 1:nrow(buy)
    }
    consumer <- unique(buy$id)
    nconsumer <- length(consumer)

    # Draw the simulates
    consumer.sim <- kronecker(consumer, pracma::ones(nsim, 1))
    consumer.sim <- data.frame(id = consumer.sim, sim = rep(1:nsim, times = nconsumer))
    ndraws <- nrow(consumer.sim)

    if (importance.sampling){
        ## Determine overall acceptance rate
        NS <- 1e5
        ## Dimensions of heterogeneity: platform-generic, platform-specific
        ## (dimensionality equal to number of platforms), restaurant dining,
        ## and relative taste for chain restaurants
        Xi <- lapply(1:NS, function(x) rnorm(7))
        phi.Xi <- sapply(Xi, function(xi) phi(xi, demand.param))
        S <- mean(phi.Xi)

        ## Draw from the importance sampling distribution
        Xi <- sapply(1:ndraws, function(k) draw.IS(demand.param))
        Xi <- t(Xi)
        ## Compute adjustment weight for each draw
        scores <- apply(Xi, 1, function(xi) phi(xi, demand.param))
        weights <- S/scores

        consumer.sim$zeta.1 <- Xi[, 1]
        for (f in 1:(nplatforms - 1)){
            vname <- paste0('zeta.2-', as.character(f))
            consumer.sim[, vname] <- Xi[, f + 1]
        }
        consumer.sim$eta   <- Xi[, 6]
        consumer.sim$phi.i <- Xi[, 7]

        consumer.sim$phi.weight <- weights
    } else {
        consumer.sim$zeta.1 <- rnorm(ndraws)
        for (f in 1:(nplatforms - 1)){
            vname <- paste0('zeta.2-', as.character(f))
            consumer.sim[, vname] <- rnorm(ndraws)
        }
        consumer.sim$eta   <- rnorm(ndraws)
        consumer.sim$phi.i <- rnorm(ndraws)
    }
    consumer.sim$eta.idio <- rnorm(ndraws)

    # Merge into the transaction level dataset
    buy <- dplyr::left_join(consumer.sim, buy, by = 'id')

    # Adjust weights
    if ('count' %in% colnames(buy)){
        buy$count <- buy$count/nsim
    }
    if (importance.sampling){
        buy$count <- buy$count*buy$phi.weight
    }

    return(buy)
}
