# Produce separate location and platform choice datasets
# This version uses Infogroup data on restaurants that haven't joined any
# platform

library(doBy)
library(dplyr)

main <- function(){
    r.times <- FoodDeliveryTools::platform.adoption.times()
    years   <- r.times$years
    months  <- r.times$months

    for (year in years){
        for (month in months[[year]]){
            separate.data(month, year)
        }
    }
}

separate.data <- function(month, year){
    # Separate the data into platform-level and platform/location-level datasets
    suffix <- sprintf('_w_infogroup%s_%s%s', year, month, year)

    data.dir <- 'data/yipitdata/'
    inpath      <- sprintf('%s/locations%s.rds',      data.dir, suffix)
    outpath.loc <- sprintf('%s/location_level%s.rds', data.dir, suffix)
    outpath.pl  <- sprintf('%s/plat_loc_level%s.rds', data.dir, suffix)

    df <- readRDS(inpath)

    # Sort by platform, putting offline first, to keep the name from Infogroup when it is available
    df$is.online <- 1*(df$platform != 'offline')
    df <- df[order(df$loc.id, df$is.online, df$platform), ]

    df$restaurant_id <- NULL
    df$first.letters <- NULL
    df$geo <- NULL
    df$CBSA_name <- NULL

    loc.vars <- c('loc.id', 'restaurant', 'zip', 'county', 'county_name',
                  'cbsa', 'city', 'lat', 'lon', 'brand')
    locations <- df[, loc.vars]
    locations <- locations[order(locations$loc.id), ]
    idx <- which(!is.na(locations$brand))
    locations$restaurant[idx] <- locations$brand[idx]

    # First-pass: don't try to aggregate within-location distances, etc
    # Just collapse the data and keep the "first observation"
    locations <- locations[!duplicated(locations[, c('loc.id')]), ]

    pl.vars <- c('platform', 'loc.id', 'brand', 'is_partnered_merchant')
    pl.loc <- df[, pl.vars]

    # Enforce that all restaurants belong to the offline platform
    online.loc  <- unique(pl.loc$loc.id[which(pl.loc$platform != 'offline')])
    offline.loc <- unique(pl.loc$loc.id[which(pl.loc$platform == 'offline')])
    online.only <- setdiff(online.loc, offline.loc)
    online.only.brands <- data.frame(loc.id = online.only)
    online.only.brands <- left_join(online.only.brands, locations[, c('loc.id', 'brand')], by = 'loc.id')

    addendum <- data.frame(platform = 'offline', loc.id = online.only.brands$loc.id,
                           brand = online.only.brands$brand)
    pl.loc <- bind_rows(pl.loc, addendum)

    # Save the data
    saveRDS(locations, file = outpath.loc)
    saveRDS(pl.loc,    file = outpath.pl)
}

main()
