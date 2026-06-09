# Produce data at the level of a month/ZIP for use in demand estimation
library(data.table)
library(dplyr)
library(EconTools)

main <- function(){
    partner.only <- TRUE
    fee.version <- '_v3'
    menu.price.type <- 'did'
    rho.by.G <- FALSE

    produce.month.zip.level('april', fee.version = fee.version,
                            partner.only = partner.only,
                            menu.price.type = menu.price.type,
                            rho.by.G = rho.by.G)
    produce.month.zip.level('may',   fee.version = fee.version,
                            partner.only = partner.only,
                            menu.price.type = menu.price.type,
                            rho.by.G = rho.by.G)
    produce.month.zip.level('june',  fee.version = fee.version,
                            partner.only = partner.only,
                            menu.price.type = menu.price.type,
                            rho.by.G = rho.by.G)

}

produce.month.zip.level <- function(month, fee.version = '_v4',
                                    partner.only = TRUE, rho.by.G = TRUE,
                                    menu.price.type = 'hedonic'){
    # Combine the consumer panel data with the restaurant location data.
    # The active replication path uses menu.price.type = 'did' (set in main()).
    # The 'hedonic', 'psample', 'rho.by.G', and unnamed fallback branches below
    # are alternative menu-price constructions retained for the robustness
    # specifications supported by the MATLAB demand code
    # (see auxiliary_functions/DetermineDataPath.m, mpt = 1..4).

    psuffix <- ifelse(partner.only, '_partnered', '')
    rho.suffix <- ifelse(rho.by.G, '_rhoByG', '')
    dat.dir <- 'data/est_dat'
    base.path <- 'zcta_code_portfoliosByCap-yipitdata_w_infogroup2021'
    if (month == 'april'){
        inpath.zip   <- sprintf('%s/%s%s_apr2021.csv',       dat.dir, base.path, psuffix)
        inpath.chain <- sprintf('%s/%s%s_apr2021_chain.csv', dat.dir, base.path, psuffix)
        inpath.indep <- sprintf('%s/%s%s_apr2021_indep.csv', dat.dir, base.path, psuffix)
        month.num <- '2021-04-01'
    } else if (month == 'may'){
        inpath.zip   <- sprintf('%s/%s%s_may2021.csv',       dat.dir, base.path, psuffix)
        inpath.chain <- sprintf('%s/%s%s_may2021_chain.csv', dat.dir, base.path, psuffix)
        inpath.indep <- sprintf('%s/%s%s_may2021_indep.csv', dat.dir, base.path, psuffix)
        month.num <- '2021-05-01'
    } else if (month == 'june'){
        # No June 2021 restaurant snapshot exists (zcta_resto_data.R only
        # generates data through May 2021 per platform.adoption.times()).
        # Use the May 2021 snapshot as a proxy — restaurant locations and
        # platform portfolios change slowly month-to-month.
        inpath.zip   <- sprintf('%s/%s%s_may2021.csv',       dat.dir, base.path, psuffix)
        inpath.chain <- sprintf('%s/%s%s_may2021_chain.csv', dat.dir, base.path, psuffix)
        inpath.indep <- sprintf('%s/%s%s_may2021_indep.csv', dat.dir, base.path, psuffix)
        month.num <- '2021-06-01'
    }

    inpath.fee <- sprintf('data/prices/price_indices%s.csv', fee.version)

    hedonic <- (menu.price.type == 'hedonic')
    psample <- (menu.price.type == 'psample')
    did     <- (menu.price.type == 'did')

    if (hedonic){
        inpath.menu <- 'output/numerator_menu_pricing/estimated_rhos.rds'
    } else if (psample){
        inpath.menu <- 'output/restaurant_price_sample/price_indices.rds'
    } else if (did){
        inpath.menu <- 'output/numerator_menu_pricing/disagg_results/price_indices.csv'
    } else {
        if (rho.by.G){
            inpath.menu <- 'output/numerator_menu_pricing/rhos_adjusted.csv'
        } else {
            inpath.menu <- 'output/numerator_menu_pricing/menu_prices_fe_cap_effect.csv'
        }
    }

    inpath.cbsa.codes <- 'data/small_data/cbsa_codenames.csv'
    inpath.geo <- 'data/geo/geo_with_zctas.csv'
    inpath.caps <- 'data/fee_caps/monthly_fee_caps.csv'
    outdir <- 'data/est_dat'
    create.dir(outdir)

    if (hedonic | psample | did){
        rho.suffix <- paste0('_', menu.price.type)
    }

    outpath <- sprintf('%s/month_zip_data%s%s%s_%s.csv',
                       outdir, fee.version, psuffix, rho.suffix, month)

    ## Load ZIP code data on portfolios
    zip <- fread(inpath.zip)
    zip$zip <- fix.zip.codes(zip$zip)
    ### Chain restaurants
    zip.chain <- fread(inpath.chain)
    zip.chain$zip <- fix.zip.codes(zip.chain$zip)
    ### Independent restaurants
    zip.indep <- fread(inpath.indep)
    zip.indep$zip <- fix.zip.codes(zip.indep$zip)

    # Merge datasets
    idx <- colnames(zip.chain) != 'zip'
    colnames(zip.chain)[idx] <- paste0(colnames(zip.chain)[idx], '_chain')
    idx <- colnames(zip.indep) != 'zip'
    colnames(zip.indep)[idx] <- paste0(colnames(zip.indep)[idx], '_indep')
    zip <- dplyr::left_join(zip, zip.chain, by = 'zip')
    zip <- dplyr::left_join(zip, zip.indep, by = 'zip')
    zip <- as.data.frame(zip)
    for (k in grep('(_chain|_indep)', colnames(zip), value = TRUE)){
        idx <- is.na(zip[, k])
        zip[idx, k] <- 0
    }

    ## Load commission caps
    cap.df <- read.dat(inpath.caps, colClasses = c('zip' = 'character'))
    cap.df <- cap.df[which(cap.df$month == month.num), ]
    cap.df$month <- NULL

    ## Load consumer fees
    CC <- c('zip'  = 'character', 'zcta' = 'character')
    fees <- read.dat(inpath.fee, colClasses = CC)
    fee.vars <- grep('\\.price$', colnames(fees), value = TRUE)
    WT.vars    <- grep('WT_[a-z]+_lasso', colnames(fees), value = TRUE)
    fees <- fees[, c('zcta', fee.vars, WT.vars)]
    fee.vars <- sub('\\.', '_', fee.vars)
    WT.vars <- gsub('(WT_|_lasso)', '', WT.vars)
    WT.vars <- paste0(WT.vars, '_WT')
    colnames(fees) <- c('zcta', fee.vars, WT.vars)

    # Load geographical data
    geo <- read.dat(inpath.geo, colClasses = CC)
    geo <- geo[, c('zip', 'zcta', 'is.zcta', 'CBSA_name')]
    geo <- geo[which(geo$is.zcta), ]
    geo$is.zcta <- NULL

    # Load menu prices
    if (hedonic){
        mdat <- readRDS(inpath.menu)
        # Convert to wide
        md.cap.c   <- mdat$rho.noG.cap.c
        md.cap.i   <- mdat$rho.noG.cap.i
        md.nocap.c <- mdat$rho.noG.c
        md.nocap.i <- mdat$rho.noG.i

        ## There isn't a difference by portfolio, so collapse to platform/market level
        fn <- function(md, rho.name){
            md$G <- NULL
            md <- md[!duplicated(md), ]
            # Subset columns as well
            md <- md[, c('market', 'platform', 'price_index')]
            colnames(md) <- c('CBSA_name', 'platform', rho.name)
            return(md)
        }
        md.cap.c   <- fn(md.cap.c, 'rho_chain_cap')
        md.cap.i   <- fn(md.cap.i, 'rho_indep_cap')
        md.nocap.c <- fn(md.nocap.c, 'rho_chain')
        md.nocap.i <- fn(md.nocap.i, 'rho_indep')

        # Merge
        byvar <- c('CBSA_name', 'platform')
        menu.dat <- dplyr::left_join(md.cap.c, md.cap.i, by = byvar)
        menu.dat <- dplyr::left_join(menu.dat, md.nocap.c, by = byvar)
        menu.dat <- dplyr::left_join(menu.dat, md.nocap.i, by = byvar)

        # Convert to wide
        menu.dat <- tidyr::pivot_wider(menu.dat, names_from = 'platform',
                                        values_from = c('rho_chain', 'rho_chain_cap',
                                                        'rho_indep', 'rho_indep_cap'))
    } else if (psample){
        rhos <- readRDS(inpath.menu)
        markets <- sort(read.dat('data/small_data/cbsa_codenames.csv')$CBSA_name)

        menu.dat <- data.frame(CBSA_name = markets)
        platforms <- c('direct', 'dd', 'uber', 'gh')
        for (f in platforms){
            if (f == 'direct'){
                p.no.cap <- rhos['off']
                p.cap    <- rhos['off']
            } else {
                p.no.cap <- rhos['on']
                p.cap    <- rhos['on_cap']
            }

            # Specify variable names
            indep.no.cap <- sprintf('rho_indep_%s', f)
            indep.cap    <- sprintf('rho_indep_cap_%s', f)
            chain.no.cap <- sprintf('rho_chain_%s', f)
            chain.cap    <- sprintf('rho_chain_cap_%s', f)
            # Define variables
            menu.dat[, indep.no.cap] <- p.no.cap
            menu.dat[, indep.cap]    <- p.cap
            menu.dat[, chain.no.cap] <- p.no.cap
            menu.dat[, chain.cap]    <- p.cap
        }
    } else if (did){
        rhos <- read.dat(inpath.menu)
        markets <- sort(read.dat('data/small_data/cbsa_codenames.csv')$CBSA_name)

        ## Unique commission levels
        K <- nrow(rhos)
        keep.cols <- setdiff(colnames(rhos), 'r')
        Rows <- list()
        for (k in 1:K){
            rhos.k <- rhos[k, keep.cols]
            r.k <- round(rhos$r[k]*100)
            names(rhos.k) <- sprintf('rho_%s_%d', names(rhos.k), r.k)
            Rows[[k]] <- rhos.k
        }
        menu.dat <- Reduce(cbind, Rows)
        menu.dat <- Reduce(dplyr::bind_rows, lapply(1:length(markets), function(x) menu.dat))
        menu.dat$CBSA_name <- markets

    } else if (rho.by.G){
        menu.dat <- read.dat(inpath.menu)

        # Convert to wide
        menu.dat <- menu.dat[, c('CBSA_name', 'm', 'platform', 'portfolio', 'rho', 'rho_cap')]
        menu.dat <- tidyr::pivot_wider(menu.dat, names_from = c(platform, portfolio),
                                       values_from = c(rho, rho_cap))
        menu.dat <- as.data.frame(menu.dat)
    } else {
        menu.dat <- read.dat(inpath.menu)
        ## Approach 1: adjust the offline price to shrink the markup
        menu.dat$rho_offline_cap <- menu.dat$rho_online/menu.dat$markup_cap
        ## Approach 2: adjust the online price to shrink the markup
        ## don't combine the approaches!
        menu.dat$rho_online_cap <- menu.dat$rho_offline*menu.dat$markup_cap
        cbsa.codes <- read.dat(inpath.cbsa.codes)
        menu.dat <- left_join(menu.dat, cbsa.codes, by = 'cbsa')
        menu.dat <- menu.dat[, c('rho_online', 'rho_offline', 'rho_offline_cap', 'rho_online_cap', 'CBSA_name')]
    }

    # Combine the datasets
    ## Merge in the number of nearby restaurants
    geo <- inner_join(geo, zip, by = 'zip')
    geo <- inner_join(geo, fees, by = 'zcta')
    geo <- inner_join(geo, menu.dat, by = 'CBSA_name')
    geo$month <- month

    # Select rho based on cap
    geo <- left_join(geo, cap.df, by = 'zip')
    idx.no.cap <- which(geo$cap >= 0.30)

    if (hedonic | psample){
        cap.vars <- grep('^rho_.*_cap', colnames(geo), value = TRUE)
        for (cap.var in cap.vars){
            noncap.var <- sub('_cap', '', cap.var)
            geo[idx.no.cap, cap.var] <- geo[idx.no.cap, noncap.var]
        }
    } else if (did){
        cap.levels <- sort(unique(geo$cap))
        channels <- c('direct', 'dd', 'uber', 'gh', 'pm')
        for (f in 1:length(channels)){
            cf <- channels[f]
            new.var <- sprintf('rho_%s', cf)
            geo[, new.var] <- 0
            for (k in 1:length(cap.levels)){
                r.k   <- round(100*cap.levels[k])
                idx.k <- which(geo$cap == cap.levels[k])

                existing.var <- sprintf('rho_%s_%d', cf, r.k)
                geo[idx.k, new.var] <- geo[idx.k, existing.var]
            }
        }

    } else if (rho.by.G){
        cap.vars <- grep('^rho_cap_', colnames(geo), value = TRUE)
        for (cap.var in cap.vars){
            noncap.var <- sub('_cap', '', cap.var)
            geo[idx.no.cap, cap.var] <- geo[idx.no.cap, noncap.var]
        }
    } else {
        geo$rho_online_cap[idx.no.cap]  <- geo$rho_online[idx.no.cap]
        geo$rho_offline_cap[idx.no.cap] <- geo$rho_offline[idx.no.cap]
    }

    write.csv(geo, file = outpath, row.names = FALSE, quote = TRUE)
}

main()
