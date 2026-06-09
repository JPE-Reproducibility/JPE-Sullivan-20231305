compute.market.sales <- function(dat.m, num.param, opts, selected.zip = NULL,
                                    J.G.m = NULL, menu.subset = NULL){
    # Compute the sales of restaurants in each zipcode in a market under each possible choice
    # of platform subset
    #
    # Inputs
    #   dat.m$J.G.m: list of restaurants' portfolio choices (specific to market m)
    #   dat.m$buy.m: transactions dataset (specific to market m)
    #   dat.m$fees: prices charged to consumers by platforms in market m
    #   dat.m$caps.df: caps on commissions charged to restaurants by platforms
    #   dat.m$demand.param: parameters of the demand model
    #   opts$resto.param: parameters of the restaurant model
    #   dat.m$zip.mat.m: mapping between zip codes and sets of zip codes in range of these zip codes
    #   opts$p.CF: counterfactual prices charged to restaurants
    #   selected.zip: Use a single zip code's data?
    #   menu.subset: Subset of zip code for which to find menu pricing equilibrium

    # Extract options
    resto.param     <- opts$resto.param
    p.CF            <- opts$p.CF

    # Determine restaurants in range of the zip code
    if (is.null(J.G.m)){
        J.G.m <- dat.m$J.G.1.m
    }
    Jp.G.m <- map.J.to.Jp(J.G.m, zip.mat.m = dat.m$zip.mat.1.m)

    zip.map <- dat.m$zip.map.1
    # Extract portfolio membership matrix
    G.mat <- dat.m$G.mat
    # Update restaurant information
    dat.m$J.G.1.m  <- J.G.m
    dat.m$Jp.G.1.m <- Jp.G.m

    # Determine menu prices: compute equilibrium in menu prices
    mc <- resto.param$mc
    out <- menu.price.eqm(dat.m, mc, num.param = num.param,
                          verbose = opts$verbose, menu.subset = menu.subset,
                          opts = opts, return.markup = TRUE)
    Rhos    <- out$Rhos
    markups <- out$markups

    # Impose counterfactual prices
    if (!is.null(p.CF)){
        for (zip in names(dat.m$fees)){
            dat.m$fees[[zip]] <- p.CF$p.c
        }
    }

    Rhos <- set.exclusive.Rhos(Rhos, opts, G.mat)

    # Compute sales in each zipcode
    Sales.mats <- compute.zip.sales(dat.m, Rhos = Rhos, selected.zip = selected.zip,
                                    opts = opts,
                                    more.outputs = TRUE)
    Sales.mats$Rhos <- Rhos
    Sales.mats$markup <- markups

    return(Sales.mats)
}
