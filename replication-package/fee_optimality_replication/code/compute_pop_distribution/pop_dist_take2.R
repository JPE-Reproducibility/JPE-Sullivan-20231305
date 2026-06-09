# Generate a distribution of consumers across ZCTAs and demographic types

library(EconTools)
library(doBy)
library(dplyr)


main <- function(){
    # Settings
    ## Minimum number of transactions for a ZCTA's distribution to be used
    min.nbuy <- 10

    # Paths to inputs
    inpath.acs       <- 'data/ACS/processed/ACS_data.csv'
    inpath.geo       <- 'data/geo/geo_with_zctas.csv'
    inpath.cbsa      <- 'data/est_dat/cbsa_ids-yipitdata_april.csv'
    inpath.buy       <- 'data/est_dat/connect_sample_v3_hedonic_sset_0s.csv'
    inpath.zipmap    <- 'data/geo/zcta_map.rds'

    # Paths to outputs
    outpath.dist <- 'data/eqm_data/pop_dist.csv'

    # Read data
    acs     <- process.acs.data(inpath.acs)
    geo     <- read.dat(inpath.geo, colClasses = c('zcta' = 'character', 'zip' = 'character'))
    cbsa    <- read.dat(inpath.cbsa, sep = '|')
    buy     <- read.dat(inpath.buy)
    zip.map <- readRDS(inpath.zipmap)

    buy$zip <- fix.zip.codes(buy$zip)

    geo <- geo[which(geo$is.zcta), ]
    acs <- left_join(acs, geo[, c('zcta', 'CBSA_name')], by = 'zcta')
    acs <- inner_join(acs, cbsa, by = 'CBSA_name')
    buy <- buy[!duplicated(buy$USER_ID), ]

    # Compare number of ZIPs with positive sales in ACS versus buy
    buy.zip.lvl <- summaryBy(USER_ID ~ zip, data = buy, FUN = length)
    colnames(buy.zip.lvl) <- c('zip', 'nbuy')
    buy.zip.lvl <- full_join(buy.zip.lvl, acs, by = 'zip')
    buy.zip.lvl$nbuy[which(is.na(buy.zip.lvl$nbuy))] <- 0

    # Distribution of demographic types in each ZCTA
    Types <- expand.grid(0:1, 0:1, 0:1)
    chars <- c('young', 'married', 'high_income')
    colnames(Types) <- chars
    Types$type <- 1:nrow(Types)

    buy <- left_join(buy, Types, by = chars)

    # Compute the type distribution for each ZCTA
    TypeDists <- generate.type.dists(buy, buy.zip.lvl, zip.map, Types, min.nbuy)

    # Processing
    DFs <- list()
    for (zip in names(TypeDists)){
        df.z <- Types
        df.z$zip <- zip
        df.z$count <- TypeDists[[zip]]
        DFs[[zip]] <- df.z
    }
    DF <- as.data.frame(data.table::rbindlist(DFs))

    # Merge in the populations
    DF <- left_join(DF, acs[, c('zip', 'pop_over_15')], by = 'zip')
    DF$count <- DF$count*DF$pop_over_15

    zip.to.m <- acs[, c('zip', 'm')]
    DF <- left_join(DF, zip.to.m, by = 'zip')

    # Save the data
    write.dat(DF, file = outpath.dist)
}

process.acs.data <- function(inpath.acs){
    # Process the ACS data
    acs     <- read.dat(inpath.acs, colClasses = c('zcta' = 'character'))
    ## Keep only ZCTAs with positive populations
    acs <- acs[which(acs$pop_over_15 > 0), ]
    acs$zip <- acs$zcta
    return(acs)
}

generate.type.dists <- function(buy, buy.zip.lvl, zip.map, Types, min.nbuy){
    # Compute the type distribution for each ZCTA
    TypeDists <- list()
    zips <- buy.zip.lvl$zip

    for (zip in zips){
        idx <- which(buy.zip.lvl$zip == zip)
        nbuy.z <- buy.zip.lvl$nbuy[idx]
        if (nbuy.z < min.nbuy){
            # Use nearby distribution of characteristics
            nearby.z <- zip.map[[zip]]
            buy.z <- buy[which(buy$zip %in% nearby.z), ]
        } else {
            buy.z <- buy[which(buy$zip == zip), ]
        }
        if (nrow(buy.z) == 0){
            next
        }
        TypeDists[[zip]] <- sapply(1:nrow(Types), function(k) mean(buy.z$type == k))
    }
    return(TypeDists)
}

main()
