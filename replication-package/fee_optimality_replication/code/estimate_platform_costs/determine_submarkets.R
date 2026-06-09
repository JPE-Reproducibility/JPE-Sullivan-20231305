# Define submarkets in which to compute pricing equilibria

library(Matrix)
library(FoodDeliveryTools)
library(EconTools)

main <- function(){

    # Set path to output
    outpath <- 'output/estimate_platform_costs/pricing_zones.rds'

    nsim.suffix <- '_nsim50'
    inpaths <- collect.inpaths(nsim.suffix)


    geo <- load.geo(inpaths$inpath.geo)

    eqm.objs <- load.all.eqm.objs(nsim.suffix)

    markets <- names(eqm.objs)

    outputs <- list()
    for (market in markets){
        dat.m <- eqm.objs[[market]]$dat.m
        outputs[[market]] <- determine.pricing.zones(dat.m, geo)
    }
    saveRDS(outputs, outpath)
}

determine.pricing.zones <- function(dat.m, geo){
    # Choose zones in which to compute pricing equilibria

    # Set parameters
    min.pop    <- 100e3
    min.nresto <- 300

    # Initialize outputs (counties only; the municipality branch is unused
    # downstream — load.all.eqm.objs.feefirst reads Zones.c)
    Zones.c <- list()

    # Extract data
    rdat <- dat.m$resto.m
    zips <- names(dat.m$J.G.m)

    # Subset the ZIP-level geographical dataset
    geo.sub <- geo[which(geo$zcta %in% zips), ]
    geo.sub <- geo.sub[geo.sub$is.zcta, ]

    # Obtain a listing of counties
    counties <- unique(geo.sub$county)

    # Compute the number of restaurants in each county
    nzip.c   <- c()
    nresto.c <- c()
    pop.c    <- c()
    for (k in 1:length(counties)){
        county.k <- counties[k]
        idx.k    <- which(geo.sub$county == county.k)
        zips.k <- geo.sub$zcta[idx.k]
        nzip.c[k] <- length(zips.k)
        nresto.c[k] <- sum(rdat$J_total[which(rdat$zip %in% zips.k)])
        pop.c[k] <- sum(geo.sub$pop[idx.k])
    }
    names(nzip.c) <- names(nresto.c) <- names(pop.c) <- counties

    # Determine the counties that meet the inclusion criteria
    idx.incl <- which(nresto.c >= min.nresto & pop.c >= min.pop)
    counties.incl <- names(nresto.c)[idx.incl]

    for (ci in counties.incl){
        idx.ci <- which(geo.sub$county == ci)
        Zones.c[[ci]] <- intersect(zips, geo.sub$zcta[idx.ci])
    }

    output <- list()
    output$Zones.c <- Zones.c
    return(output)
}

main()






