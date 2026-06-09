# Merge restaurant locations across channels
# (i.e., platforms and the offline channel)

library(geosphere)
library(stringdist)

library(EconTools)

main <- function(){
    r.times <- FoodDeliveryTools::platform.adoption.times()
    years   <- r.times$years
    months  <- r.times$months

    ## Test mode
    test <- FALSE
    if (test){
        years <- years[1]
        months[[years[1]]] <- months[[years[1]]][1]
    }

    for (year in years){
        for (month in months[[year]]){
            add.locations.w.infogroup(month, year, test)
        }
    }
}

add.locations.w.infogroup <- function(month, year, test){
    # Settings
    ## Max distance (m) for two restaurants to be considered the same
    max.dist.chain <- 200
    max.dist.indep <- 100
    ## Max string distance of names for two locations to be considered the same
    max.sdist <- 5
    ## Min first-letters count to separate by geo
    K <- 1000

    inpath         <- sprintf('data/yipitdata/brands_w_infogroup%s_%s%s.rds',
                              year, month, year)
    outpath        <- sprintf('data/yipitdata/locations_w_infogroup%s_%s%s.rds',
                              year, month, year)
    outpath.w.dupl <- sprintf('data/yipitdata/locations_w_dupl_and_infogroup%s_%s%s.rds',
                              year, month, year)

    df <- readRDS(inpath)
    df$county <- paste(df$county_name, df$state, sep = ' ')
    df <- reformat.restaurant.names(df)

    if (test){
        counties.tab <- sort(table(df$county), decreasing = TRUE)
        keep.counties <- names(counties.tab)[1:10]
        df <- df[which(df$county %in% keep.counties), ]
        outpath        <- sub('\\.rds', '_test.rds', outpath)
        outpath.w.dupl <- sub('\\.rds', '_test.rds', outpath.w.dupl)
    }

    # Fill in missing CBSA
    df$cbsa <- ifelse(is.na(df$CBSA_name), paste0('Other', df$state), df$CBSA_name)

    # First letters of the restaurant name (with spaces removed)
    df$first.letters <- substr(gsub(' ', '', df$restaurant), 1, 2)

    # Add platform-restaurant ID
    df$pr.id <- 1:nrow(df)

    # Initialize the location ID variable
    df$loc.id <- NA

    # Set chain restaurants' location IDs
    ## Use county as the geographical level
    df$county <- sub('[ ]*$', '', df$county)
    df$geo <- df$county
    geos <- unique(df$geo)
    brands <- unique(df$brand[!is.na(df$brand)])
    for (brand in brands){
        print(brand)
        df <- add.brand.locations(df, brand, geos, max.dist.chain)
    }

    # Set non-chains' location IDs
    ## Separate first letter combinations into a
    ## popular group and a non-popular group
    tab.fl <- table(df$first.letters)
    pop.FLs <- names(tab.fl)[tab.fl >= K]
    nonpop.FLs <- setdiff(names(tab.fl), pop.FLs)

    ## Add location IDs for the popular locations
    df$geo <- df$cbsa
    geos <- unique(df$cbsa)
    for (FL in pop.FLs){
        print(FL)
        df <- add.nonchain.locations(df, FL, geos, max.dist.indep, max.sdist)
    }
    ## Add location IDs for the nonpopular locations
    df$geo <- 1
    geos <- unique(df$geo)
    for (FL in nonpop.FLs){
        print(FL)
        df <- add.nonchain.locations(df, FL, geos, max.dist.indep, max.sdist)
    }

    saveRDS(df, file = outpath.w.dupl)

    # Delete duplicates on platform/location
    df <- df[!duplicated(df[, c('platform', 'loc.id')]), ]

    saveRDS(df, file = outpath)
}

add.brand.locations <- function(df, brand, geos, max.dist.chain){
    # Add location IDs for restaurants belonging to major brands (ie chains)
    #
    # Inputs
    #   df: Restaurant-platform level dataset
    #   brand: A chain of restaurants
    #   geos: List of geographical regions for which to add locations
    #   max.dist.chain: The maximum distance between two observations for them
    #       to be considered the same location

    ngeos <- length(geos)
    keep.cols <- c('pr.id', 'platform', 'restaurant', 'lat', 'lon', 'geo')

    for (k in 1:ngeos){
        # Select a geographical region
        geo <- geos[k]

        # Find all observations for the brand and geographical region
        idx.geo <- which(df$geo == geo & df$brand == brand)
        ngeo <- length(idx.geo)
        if (ngeo == 0){
            next
        } else if (ngeo == 1){
            # One observation - give the single location an ID code
            loc.id <- sprintf('%s-%s-1', brand, geo)
            df[idx.geo, 'loc.id'] <- loc.id
            next
        }
        df.geo <- df[idx.geo, keep.cols]

        # Compute the distance matrix (in metres)
        df.geo <- df.geo[order(df.geo$lat, df.geo$lon), ]
        coors <- cbind(df.geo$lon, df.geo$lat)
        dist.mat <- distm(coors)
        close.dist <- 1*(dist.mat < max.dist.chain)

        # Generate restaurant ID codes and assign them to the restaurants
        nres <- nrow(df.geo)
        loc.id <- rep(NA, times = nres)
        id.no <- 1
        for (r in 1:nres){
            # Determine if the restaurant is in an existing group
            if (is.na(loc.id[r])){
                # If not, create a new group
                idx <- which(close.dist[, r] == 1)
                loc.id[idx] <- id.no
                id.no <- id.no + 1
            }
        }
        loc.id <- sprintf('%s-%s-%d', brand, geo, loc.id)

        df.geo$loc.id <- loc.id
        df.geo <- df.geo[order(df.geo$pr.id), ]
        if (min(df.geo$pr.id == idx.geo) < 1){
            stop('Error in merging county/chain data into main dataset')
        }
        # Add the restaurant ID variable
        df[idx.geo, 'loc.id'] <- df.geo$loc.id
    }
    return(df)
}

add.nonchain.locations <- function(df, FL, geos, max.dist.indep, max.sdist){
    # Add location IDs for restaurants that don't belong to major chains
    #
    # Inputs
    #   df: Restaurant-platform level dataset
    #   FL: First letters of the restaurant's name
    #   geos: List of geographical regions for which to add locations
    #   max.dist.indep:  The maximum distance between two observations for them
    #       to be considered the same location
    #   max.sdist: The maximum string distance between two restaurants' names
    #       for them to be considered the same location

    ngeos <- length(geos)
    keep.cols <- c('pr.id', 'platform', 'restaurant', 'lat', 'lon', 'geo')

    # Weights for string distances
    W <- c(1e-5, 1, 1, 1)

    for (k in 1:ngeos){
        geo <- geos[k]

        # Find all observations for the brand and geographical region
        idx.geo <- which(df$geo == geo & is.na(df$brand) & df$first.letters == FL)
        ngeo <- length(idx.geo)

        cat(sprintf('Geo: %s | n = %d\n', as.character(geo), ngeo))
        if (ngeo == 0){
            # No observations - skip to the next region
            next
        } else if (ngeo == 1){
            # One observation - give the single location an ID code
            loc.id <- sprintf('%s-%s-1', FL, as.character(geo))
            df[idx.geo, 'loc.id'] <- loc.id
            next
        }
        df.geo <- df[idx.geo, keep.cols]

        df.geo <- df.geo[order(df.geo$lat, df.geo$lon), ]

        # Compute the distance matrix (in metres)
        coors <- cbind(df.geo$lon, df.geo$lat)
        dist.mat <- distm(coors)
        close.dist <- 1*(dist.mat < max.dist.indep)

        # Compute distances of strings
        rnames <- tolower(df.geo$restaurant)
        sdist.mat <- stringdistmatrix(rnames, weight = W, useNames = TRUE)
        sdist.mat <- as.matrix(sdist.mat)
        close.sdist <- 1*(sdist.mat < max.sdist)
        # Reversal to ensure symmetry
        rev.names <- rev(rnames)
        sdist.rev <- stringdistmatrix(rev.names, weight = W, useNames = TRUE)
        sdist.rev <- as.matrix(sdist.rev)
        sdist.rev <- sdist.rev[rev(1:nrow(sdist.rev)), rev(1:ncol(sdist.rev))]
        close.rev <- 1*(sdist.rev < max.sdist)
        close.sdist <- pmax(close.sdist, close.rev)

        # Determine which restaurants are the same
        nearby.mat <- close.dist*close.sdist

        # Generate restaurant ID codes and assign them to the restaurants
        nres <- nrow(df.geo)
        loc.id <- rep(NA, times = nres)
        id.no <- 1
        for (r in 1:nres){
            # Determine if the restaurant is in an existing group
            if (is.na(loc.id[r])){
                # If not, create a new group
                idx <- which(nearby.mat[, r] == 1)
                loc.id[idx] <- id.no
                id.no <- id.no + 1
            }
        }
        loc.id <- sprintf('%s-%s-%d', FL, as.character(geo), loc.id)

        df.geo$loc.id <- loc.id
        df.geo <- df.geo[order(df.geo$pr.id), ]
        if (min(df.geo$pr.id == idx.geo) < 1){
            stop('Error in merging county/chain data into main dataset')
        }
        # Add the restaurant ID variable
        df[idx.geo, 'loc.id'] <- df.geo$loc.id
    }
    return(df)
}


main()
