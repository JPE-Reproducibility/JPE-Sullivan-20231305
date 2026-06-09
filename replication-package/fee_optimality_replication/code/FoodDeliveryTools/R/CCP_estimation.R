# Estimate CCPs for use in the estimation of restaurants' costs
CCP.estimation <- function(dat, outpath, minimal = FALSE,
                           outdir = NULL){
    # Estimate CCPs for use in the estimation of restaurant fixed cost parameters
    #
    # Inputs
    #   minimal: TRUE if plot-making is to be skipped
    #   outdir: directory in which to save plots when minimal is FALSE

    # Specify paths
    inpath.geo <- 'data/geo/geo_with_zctas.csv'
    inpath.demo.range <- 'data/eqm_data/nearby_demo_data.rds'
    inpath.price <- 'data/prices/price_indices_v3.csv'

    if (!minimal){
        EconTools::create.dir(outdir)
    }
    outpath.c <- sub('CCP_outputs', 'CCP_outputs_chain', outpath)
    outpath.i <- sub('CCP_outputs', 'CCP_outputs_indep', outpath)

    # Load the data
    geo <- EconTools::read.dat(inpath.geo, colClasses = c('zip' = 'character', 'zcta' = 'character'))
    demo.range <- readRDS(inpath.demo.range)
    prices <- EconTools::read.dat(inpath.price, colClasses = c('zip' = 'character'))
    prices <- prices[, c('zip', 'uber.price', 'dd.price', 'gh.price', 'pm.price')]
    markets <- names(dat)

    CCP.data <- list()
    for (market in markets){
        CCP.data[[market]] <-
            estimate.market.CCPs(market, dat, geo, demo.range,
                                 prices, outdir, minimal)
    }
    # Save the results. The _chain and _indep files are copies of the pooled
    # CCPs: the downstream expected-profit code reads type-specific files
    # (keying ZCTAs by 'c'/'i'), but adoption CCPs are pooled across
    # restaurant types, matching the single multinomial-logit specification
    # described in the paper; type-specificity enters downstream through the
    # second-stage moments and cost parameters.
    saveRDS(CCP.data, file = outpath)
    saveRDS(CCP.data, file = outpath.c)
    saveRDS(CCP.data, file = outpath.i)
 }


estimate.market.CCPs <- function(market, dat, geo, demo.range, prices, outdir,
                                 minimal, muni.min = 200){
    # Estimate CCPs for a particular market
    #
    # Inputs
    #   muni.min: The minimum number of restaurants in a municipality for it to be
    #       given a fixed effect

    cbsa.codes  <- EconTools::read.dat('data/small_data/cbsa_codenames.csv')
    market.code <- cbsa.codes$cbsa[which(cbsa.codes$CBSA_name == market)]

    dat.m <- dat[[market]]
    resto.m <- dat.m$resto.m
    J.min <- 200

    geo.m        <- geo[which(geo$CBSA_name == market), ]
    demo.range.m <- demo.range[[market]]

    # Prepare the data
    G.vars <- grep('^G[0-1]', colnames(resto.m), value = TRUE)
    cols <- c('zip', G.vars)
    resto.sub <- resto.m[, cols]
    resto.long <- tidyr::pivot_longer(resto.sub, -zip, names_to = 'portfolio', values_to = 'count')

    # Add in regressors
    ## Add in consumer fees
    resto.long <- dplyr::left_join(resto.long, prices, by = 'zip')
    ## Add in commissions
    resto.long <- dplyr::left_join(resto.long, dat.m$caps.df, by = 'zip')
    ## Municipality fixed effects
    resto.long <- dplyr::left_join(resto.long, resto.m[, c('zip', 'municipality', 'county')], by = 'zip')

    ## If there are at least `muni.min` restaurants in the municipality, use
    ## municipality fixed effects
    muni.counts <- doBy::summaryBy(count ~ municipality, data = as.data.frame(resto.long),
                                   FUN = sum, keep.names = TRUE)
    keep.munis <- muni.counts$municipality[which(muni.counts$count >= muni.min)]
    resto.long$geo <- resto.long$municipality
    idx <- which(!(resto.long$municipality %in% keep.munis))
    resto.long$geo[idx] <- resto.long$county[idx]

    # Now add demographics
    resto.long <- dplyr::left_join(resto.long, demo.range.m, by = 'zip')
    idx <- which(is.na(resto.long$ntrans))
    resto.long$ntrans[idx] <- 0
    ## Mean imputation for ZCTAs with no transactions
    for (svar in grep('^share_', colnames(resto.long))){
        idx <- which(is.na(resto.long[, svar]))
        resto.long[idx, svar] <- sum(resto.long[, svar]*resto.long$ntrans, na.rm = TRUE)/sum(resto.long$ntrans)
    }
    resto.long <- dplyr::left_join(resto.long, resto.m[, c('zip', 'J_total')], 'zip')

    # Run regressions and generate predictions
    if (!minimal){
        ## Regression 0: mean freque ncies in entire market
        est0 <- nnet::multinom(portfolio ~ 1, resto.long, weights = resto.long$count,
                               maxit = 5000, MaxNWts = 5000)
        fitted.probs0 <- produce.fitted.probs(est0, resto.long)
        err0 <- compute.classification.error(resto.m, fitted.probs = fitted.probs0, G.vars = G.vars)
        ## Regression 1: municipality fixed effects
        est1 <- nnet::multinom(portfolio ~ geo - 1, resto.long, weights = resto.long$count,
                               maxit = 5000, MaxNWts = 5000)
        fitted.probs1 <- produce.fitted.probs(est1, resto.long)
        err1 <- compute.classification.error(resto.m, fitted.probs = fitted.probs1, G.vars = G.vars)
        ## Regression 2: demographic variables and other state variables
        est2 <- nnet::multinom(portfolio ~ share_young_range*ntrans +
                                   share_married_range*ntrans +
                                   share_ym_range*ntrans + J_total*ntrans + geo - 1,
                               resto.long, weights = resto.long$count,
                               maxit = 5000, MaxNWts = 5000)
        fitted.probs2 <- produce.fitted.probs(est2, resto.long)
        err2 <- compute.classification.error(resto.m, fitted.probs2, G.vars)
    }

    ## Regression 3:
    ##  - Geography
    ##  - Number of restaurants
    ##  - Demographics: married, young, low-income
    ##  - Overall population
    ## Note: ntrans is the population of surrounding area *in transactions*,
    ## i.e., maximum number of potential restaurant orders
    est3 <- nnet::multinom(portfolio ~ share_young_range*ntrans +
                           share_married_range*ntrans +
                           share_ym_range*ntrans + J_total*ntrans + share_low*ntrans +
                           geo + caps - 1,
                           resto.long, weights = resto.long$count,
                           maxit = 5000, MaxNWts = 5000)
    fitted.probs3 <- produce.fitted.probs(est3, resto.long)
    err3 <- compute.classification.error(resto.m, fitted.probs3, G.vars)


    if (!minimal){
        # Plot of fit
        ## Compute empirical frequencies
        empirical.probs <- compute.empirical.probs(resto.m, G.vars)
        # Specify paths
        outdir.plots <- sprintf('%s/fit_plots', outdir)
        EconTools::create.dir(outdir.plots)
        outpath.plot1 <- sprintf('%s/%s-1.pdf', outdir.plots, market.code)
        outpath.plot2 <- sprintf('%s/%s-2.pdf', outdir.plots, market.code)
        outpath.plot3 <- sprintf('%s/%s-3.pdf', outdir.plots, market.code)

        plot.ccp.fit(empirical.probs, fitted.probs1, resto.m, G.vars,
                     outpath.plot1, J.min = J.min)
        plot.ccp.fit(empirical.probs, fitted.probs2, resto.m, G.vars,
                     outpath.plot2, J.min = J.min)
        plot.ccp.fit(empirical.probs, fitted.probs3, resto.m, G.vars,
                     outpath.plot3, J.min = J.min)
    }

    if (minimal){
        errs <- c(err3 = err3)
        outputs <- list(errs = errs,
                        fitted.probs3 = fitted.probs3,
                        coef3 = coef(est3))
    } else {
        errs <- c(err0 = err0, err1 = err1, err2 = err2, err3 = err3)
        outputs <- list(errs = errs,
                        fitted.probs0 = fitted.probs0,
                        fitted.probs1 = fitted.probs1,
                        fitted.probs2 = fitted.probs2,
                        fitted.probs3 = fitted.probs3,
                        coef1 = coef(est1), coef2 = coef(est2), coef3 = coef(est3))
    }

    return(outputs)
}


produce.fitted.probs <- function(est, est.df){
    # Produce fitted probabilities from a regression `est` on
    # a dataset `est.df`
    fitted.probs <- predict(est, type = 'prob')
    fitted.probs <- as.data.frame(fitted.probs)
    fitted.probs$zip <- est.df$zip
    fitted.probs <- fitted.probs[!duplicated(fitted.probs$zip), ]
    return(fitted.probs)
}


compute.empirical.probs <- function(resto.m, G.vars){
    # Compute empirical probabilities
    resto.m.sub <- resto.m[which(resto.m$J_total > 0), ]
    empirical.probs <- resto.m.sub[, c('zip', G.vars)]
    for (g in G.vars){
        empirical.probs[, g] <- empirical.probs[, g]/resto.m.sub$J_total
    }
    return(empirical.probs)
}

compute.classification.error <- function(resto.m, fitted.probs, G.vars){
    # Compute distance between ZIP-specific empirical frequencies and
    # fitted probabilities
    # Weight by the number of restaurants belonging to a ZIP/portfolio pair

    # Compute empirical probabilities
    resto.m.sub <- resto.m[which(resto.m$J_total > 0), ]
    empirical.probs <- resto.m.sub[, c('zip', G.vars)]
    for (g in G.vars){
        empirical.probs[, g] <- empirical.probs[, g]/resto.m.sub$J_total
    }
    # Subset ZCTAs
    zips.incl <- intersect(fitted.probs$zip, empirical.probs$zip)
    resto.m.sub     <- resto.m.sub[which(resto.m.sub$zip %in% zips.incl), ]
    fitted.probs    <- fitted.probs[which(fitted.probs$zip %in% zips.incl), ]
    empirical.probs <- empirical.probs[which(empirical.probs$zip %in% zips.incl), ]
    # Sorting
    resto.m.sub     <- resto.m.sub[order(resto.m.sub$zip), ]
    fitted.probs    <- fitted.probs[order(fitted.probs$zip), ]
    empirical.probs <- empirical.probs[order(empirical.probs$zip), ]
    # Distance
    probs.dist <- abs(fitted.probs[, G.vars] - empirical.probs[, G.vars])
    err.rate   <- sum(probs.dist*resto.m.sub[, G.vars])/sum(resto.m$J_total)

    return(err.rate)
}

plot.ccp.fit <- function(empirical.probs, fitted.probs, resto.m, G.vars, outpath.plot, J.min = 200){
    # Plot the fit of the CCPs to the empirical probabilities among
    # ZCTAs with at least `J.min` restaurants

    # Subset ZCTAs
    zips.incl <- intersect(fitted.probs$zip, empirical.probs$zip)
    zips.incl <- intersect(zips.incl, resto.m$zip[which(resto.m$J_total >= J.min)])
    fitted.probs    <- fitted.probs[which(fitted.probs$zip %in% zips.incl), ]
    empirical.probs <- empirical.probs[which(empirical.probs$zip %in% zips.incl), ]
    # Sorting
    fitted.probs    <- fitted.probs[order(fitted.probs$zip), ]
    empirical.probs <- empirical.probs[order(empirical.probs$zip), ]

    fitted.long    <- tidyr::pivot_longer(fitted.probs,    all_of(G.vars), names_to = 'portfolio', values_to = 'prob_fitted')
    empirical.long <- tidyr::pivot_longer(empirical.probs, all_of(G.vars), names_to = 'portfolio', values_to = 'prob_empirical')
    probs.long <- dplyr::left_join(fitted.long, empirical.long, by = c('zip', 'portfolio'))

    pdf(outpath.plot)
    plot(x = probs.long$prob_empirical, y = probs.long$prob_fitted, pch = 4,
         ylab = 'Fitted probabilities', xlab = 'Empirical frequencies')
    grid()
    abline(a = 0, b = 1, lty = 2)
    reg <- lm(prob_fitted ~ prob_empirical, data = probs.long)
    abline(reg)
    legend(x = 'bottomright', legend = c('45 degree', 'Regression'), lty = c(2, 1))
    dev.off()
}

