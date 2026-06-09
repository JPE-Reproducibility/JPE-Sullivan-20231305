# Compute the number of restaurants within K kilometres of
# each ZCTA that have each possible platform portfolio.
# Also compute 
# - the number of restaurants within K km of each ZCTA
#   on each possible portfolio that belong to areas with/without caps
# - the number of restaurants in each ZCTA

library(tidyr)
library(dplyr)
library(geosphere)
library(doBy)
library(EconTools)
library(FoodDeliveryTools)

main <- function(){

    adoption.times <- FoodDeliveryTools::platform.adoption.times()
    years  <- adoption.times$years
    months <- adoption.times$months
    resto.types <- c('all', 'chain', 'indep')

    for (year in years){
        for (month in months[[year]]){
            for (resto.type in resto.types){
                generate.ZCTA.resto.data(month = month, year = year,
                                         resto.type = resto.type, partner.only = TRUE)
            }
        }
    }
}

generate.ZCTA.resto.data <- function(month, year, resto.type, partner.only){
    # Generate data reporting the number of restaurants with each
    # portfolio choice in each zip code. Also generate the number
    # of restaurants within range of each zip code that have made
    # each portfolio choice.

    suffix     <- sprintf('_w_infogroup%s', year)
    psuffix    <- ifelse(partner.only, '_partnered',   '')
    datasource <- 'yipitdata'

    # Number of restaurants within K kilometres for each zip code
    kilos.per.mile <- 1.60934
    max.miles <- 5
    max.kilos <- max.miles*kilos.per.mile

    # Specify paths
    inpath.loc <- sprintf('data/%s/location_level%s_%s%s.rds', datasource, suffix, month, year)
    inpath.pl <- sprintf('data/%s/plat_loc_level%s_%s%s.rds',  datasource, suffix, month, year)
    inpath.geo   <- 'data/geo/geo_with_zctas.csv'
    inpath.cap <- 'data/fee_caps/monthly_fee_caps.csv'

    outdir <- 'data/est_dat'
    create.dir(outdir)

    rtype.suffix <- ifelse(resto.type == 'all', '', paste0('_', resto.type))

    # Number of restaurants on each portfolio nearby each zip code
    outpath.portfolio <- sprintf('%s/zcta_code_portfolios-%s%s%s_%s%s%s.csv',
                                 outdir, datasource, suffix, psuffix, month, year, rtype.suffix)
    ## Decomposed by cap status
    outpath.G.by.cap <- sprintf('%s/zcta_code_portfoliosByCap-%s%s%s_%s%s%s.csv',
                                 outdir, datasource, suffix, psuffix, month, year, rtype.suffix)
    # Number of restaurants on each platform nearby each zip code
    outpath.platforms <- sprintf('%s/zcta_code_platforms-%s%s%s_%s%s%s.csv',
                                 outdir, datasource, suffix, psuffix, month, year, rtype.suffix)
    # Number of restaurants on each portfolio within each zip code
    outpath.locs      <- sprintf('%s/zcta_code_counts-%s%s%s_%s%s%s.csv',
                                 outdir, datasource, suffix, psuffix, month, year, rtype.suffix)

    # Load the location and location/platform datasets
    loc <- readRDS(inpath.loc)
    pl  <- readRDS(inpath.pl)
    # Drop non-partnered
    if (partner.only){
        idx <- c(which(pl$is_partnered_merchant), which(is.na(pl$is_partnered_merchant)))
        pl <- pl[idx, ]
    }
    # Merge the loc and loc/pl datasets together
    pl <- pl[, c('platform', 'loc.id')]
    df <- left_join(pl, loc, by = 'loc.id')

    if (resto.type == 'chain'){
        df <- df[which(!is.na(df$brand)), ]
    } else if (resto.type == 'indep'){
        df <- df[which(is.na(df$brand)), ]
    }

    # Load the geographical dataset
    geo <- read.dat(inpath.geo, colClasses = c('zip' = 'character', 'zcta' = 'character'))
    # Load the cap dataset
    cap.df <- read.dat(inpath.cap, colClasses = c('zip' = 'character'))
    month.label <- generate.month.label(month, year, suffix = TRUE)
    cap.df <- cap.df[which(cap.df$month == month.label), ]
    cap.df$month <- NULL
    cap.df$has.cap <- 1*(cap.df$cap < 0.3)

    df <- rename.var(df, 'lat', 'restaurant_lat')
    df <- rename.var(df, 'lon', 'restaurant_lon')

    # Ensure that every restaurant has a ZCTA
    df <- inner_join(df, geo[, c('zip', 'zcta')], by = 'zip')

    # Merge in cap status
    df <- left_join(df, cap.df, by = 'zip')

    keep.cols <- c('platform', 'restaurant', 'zcta', 'restaurant_lat',
                   'restaurant_lon', 'loc.id', 'has.cap')
    df <- df[, keep.cols]

    # Drop non-ZCTA ZIPs
    geo <- geo[which(geo$is.zcta), ]
    geo$latitude  <- geo$lat_zcta
    geo$longitude <- geo$lon_zcta

    # For YipitData, relabel the platforms
    df$platform <- tolower(df$platform)

    # Want: for each zip code, the number of restaurants with each platform portfolio
    # within max.kilos of the zip code
    # First: reduce to a restaurant-level dataset with platform portfolios
    platforms <- c('offline', 'doordash', 'ubereats', 'grubhub', 'postmates')

    loc.ids <- unique(df$loc.id)
    df.locs <- data.frame(loc.id = loc.ids)
    for (pl in platforms){
        print(pl)
        # Subset to restaurants on the platform pl
        df.pl <- df[which(df$platform == pl), c('loc.id', 'platform')]
        colnames(df.pl) <- c('loc.id', pl)
        if (nrow(df.pl) == 0){
            # No restaurants on this platform in the current slice; mark all
            # locs as zero and continue.
            df.locs[[pl]] <- 0
            next
        }
        # Add an indicator for platform membership
        df.pl[[pl]] <- 1
        # Record platform membership in the master "df.locs" data.frame
        df.locs <- full_join(df.locs, df.pl, by = 'loc.id')
        df.locs[is.na(df.locs[[pl]]), pl] <- 0
    }

    # Merge info into df.locs
    restos <- df[, c('loc.id','restaurant', 'zcta', 'restaurant_lat', 'restaurant_lon', 'has.cap')]
    restos <- restos[!duplicated(restos$loc.id), ]
    df.locs <- inner_join(df.locs, restos, by = 'loc.id')

    # Drop restaurants with missing coordinates
    keep.idx <- which(!is.na(df.locs$restaurant_lat) & !is.na(df.locs$restaurant_lon))
    df.locs <- df.locs[keep.idx, ]

    # Compute number of restaurants on each combination
    # At this point, I need a conclusive listing of delivery platforms
    # 0. Offline
    # 1. Doordash
    # 2. Uber Eats
    # 3. Grubhub
    # 4. Postmates
    combos <- expand.grid(0:1, 0:1, 0:1, 0:1, 0:1)

    colnames(combos) <- platforms
    df.locs$G <- NA
    g.vals <- c()
    ## Impose membership on offline platform
    combos <- combos[combos[, 1] == 1, ]
    rownames(combos) <- 1:nrow(combos)

    for (k in 1:nrow(combos)){
        row <- as.numeric(combos[k, ])
        idx <- rep(TRUE, times = nrow(df.locs))
        for (j in 1:length(platforms)){
            pl <- platforms[j]
            idx <- idx & (df.locs[[pl]] == row[j])
        }

        # Cut out the offline platform (which is always included)
        row <- row[2:length(row)]

        portfolio.name <- paste0('G', Reduce(paste0, row))
        df.locs$G[idx] <- portfolio.name
        g.vals <- c(g.vals, portfolio.name)
    }

    counties <- unique(geo$county)
    zcta.restos  <- list()
    zcta.has.cap <- list()
    zcta.no.cap  <- list()
    for (county in counties){
        print(county)
        county.zctas <- geo$zcta[geo$county == county]
        df.sub <- df.locs[which(df.locs$zcta %in% county.zctas), ]
        if (nrow(df.sub) == 0){
            next
        }
        # Data on ZIPs in the county
        geo.sub <- geo[geo$county == county, ]
        # Restaurant coordinates and distances from ZIP centroids
        resto.coords <- cbind(df.sub$restaurant_lon, df.sub$restaurant_lat)
        zcta.coords   <- cbind(geo.sub$longitude, geo.sub$latitude)
        dist.mat <- distm(zcta.coords, resto.coords)/1000
        ## Set the distance of a restaurant to its own zip code to zero
        rownames(dist.mat) <- county.zctas
        for (z in county.zctas){
            idx <- which(df.sub$zcta == z)
            dist.mat[z, idx] <- 0.0
        }
        close.mat <- 1*(dist.mat <= max.kilos)

        # Record how many restaurants are nearby z
        for (zcta in county.zctas){
            resto.idx <- as.logical(close.mat[zcta, ])
            zcta.restos[[zcta]]  <- sapply(g.vals, function(g) sum(df.sub$G[resto.idx] == g))
            zcta.has.cap[[zcta]] <- sapply(g.vals, function(g) sum((df.sub$G[resto.idx] == g) &
                                                                   (df.sub$has.cap[resto.idx] == 1)))
            zcta.no.cap[[zcta]]  <- sapply(g.vals, function(g) sum((df.sub$G[resto.idx] == g) &
                                                                   (df.sub$has.cap[resto.idx] == 0)))
        }
    }
    zcta.G.df <- Reduce(rbind, zcta.restos)
    zcta.G.df <- as.data.frame(zcta.G.df)

    zcta.G.cap.df <- Reduce(rbind, zcta.has.cap)
    zcta.G.cap.df <- as.data.frame(zcta.G.cap.df)

    zcta.G.no.cap.df <- Reduce(rbind, zcta.no.cap)
    zcta.G.no.cap.df <- as.data.frame(zcta.G.no.cap.df)

    # Adjust names
    colnames(zcta.G.cap.df)    <- paste0('cap_',   colnames(zcta.G.cap.df))
    colnames(zcta.G.no.cap.df) <- paste0('nocap_', colnames(zcta.G.no.cap.df))

    # Add the total number of restaurants
    zcta.G.df$J_total            <- rowSums(zcta.G.df[, grep('^G', colnames(zcta.G.df))])
    zcta.G.cap.df$cap_total_cap  <- rowSums(zcta.G.cap.df[, grep('^cap_G', colnames(zcta.G.cap.df))])
    zcta.G.no.cap.df$nocap_total <- rowSums(zcta.G.no.cap.df[, grep('^nocap_G', colnames(zcta.G.no.cap.df))])

    zcta.G.df$zip <- names(zcta.restos)
    zcta.G.cap.df$zip <- names(zcta.restos)
    zcta.G.no.cap.df$zip <- names(zcta.restos)

    # Save data
    write.dat(zcta.G.df, file = outpath.portfolio)
    ### Version with decomposition by cap status
    decomp.df <- left_join(zcta.G.df, zcta.G.cap.df,    by = 'zip')
    decomp.df <- left_join(decomp.df, zcta.G.no.cap.df, by = 'zip')
    write.dat(decomp.df, file = outpath.G.by.cap)

    # Create a version with the number of restaurants per platform within range
    # of each zip code
    zcta.platforms <- zcta.G.df
    for (pl in platforms){
        idx <- combos[, pl] == 1
        g.pl <- g.vals[idx]
        zcta.platforms[[pl]] <- rowSums(zcta.platforms[, g.pl])
    }
    for (g.val in g.vals){
        zcta.platforms[[g.val]] <- NULL
    }
    # Save data
    write.dat(zcta.platforms, file = outpath.platforms)

    # Create a version with the number of restaurants within each zip code
    zctas <- zcta.G.df$zip
    in.zcta <- data.frame(zip = zctas)
    portfolios <- grep('^G', colnames(zcta.G.df), value = TRUE)
    in.zcta.G <- list()
    for (portfolio in portfolios){
        print(portfolio)
        # Take subset of restaurant locations on the current portfolio
        df.sub <- df.locs[which(df.locs$G == portfolio), ]
        if (nrow(df.sub) == 0){
            in.zcta.G[[portfolio]] <- data.frame(zip = character(0),
                                                 setNames(list(integer(0)), portfolio),
                                                 stringsAsFactors = FALSE,
                                                 check.names = FALSE)
            next
        }
        # Collapse by zip code
        in.zcta.G[[portfolio]] <- summaryBy(G ~ zcta, data = df.sub, FUN = length)
        colnames(in.zcta.G[[portfolio]]) <- c('zip', portfolio)
    }
    # Merge
    for (portfolio in portfolios){
        in.zcta <- left_join(in.zcta, in.zcta.G[[portfolio]], by = 'zip')
        idx.na <- which(is.na(in.zcta[, portfolio]))
        in.zcta[idx.na, portfolio] <- 0
    }
    in.zcta$J_total <- rowSums(in.zcta[, portfolios])
    write.dat(in.zcta, file = outpath.locs)
}

main()

