limit.to.pricing.zone <- function(dat.m, keep.zips){
    # Limit the ZCTAs considered in the market to those specified in keep.zips

    # Drop all ZCTAs outside the main pricing zone
    dat.zone <- take.data.subset.v2(dat.m, keep.zips)
    keep.zips1 <- names(dat.zone$buy.zip)

    ## Drop any ZCTAs that do not have any nearby buyers
    nearby.buyers <- sapply(keep.zips1, function(z) check.zip.for.nearby.buyers(z, dat.zone))
    if (!all(nearby.buyers)){
        keep.zips2 <- names(nearby.buyers)[nearby.buyers]
        dat.zone <- take.data.subset.v2(dat.zone, keep.zips2)
    }

    return(dat.zone)
}
