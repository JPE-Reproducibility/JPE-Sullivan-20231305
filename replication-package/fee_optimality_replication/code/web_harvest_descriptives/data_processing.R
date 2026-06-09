library(stringr)
library(stringi)
library(geosphere)
library(data.table)
library(dplyr)

library(FoodDeliveryTools)
library(EconTools)

main <- function(){
    # Specify input paths
    ## To details
    inpath.uber.details <- 'data/web_harvesting/analysis_data/uber_with_resto_info.csv'
    inpath.dd.details   <- 'data/web_harvesting/analysis_data/doordash_with_resto_info.csv'
    inpath.gh.details   <- 'data/web_harvesting/analysis_data/grubhub_with_resto_info.csv'
    inpath.pm.details   <- 'data/web_harvesting/analysis_data/postmates_with_resto_info.csv'
    ## To nondelivery
    inpath.uber.nd <- 'data/web_harvesting/ubereats_nondelivery/geocoded_orders.csv'
    inpath.dd.nd   <- 'data/web_harvesting/doordash_nondelivery/geocoded_orders.csv'
    inpath.pm.nd   <- 'data/web_harvesting/analysis_data/postmates_with_resto_info.csv'
    ## To geographic/timezone data
    inpath.geo    <- 'data/geo/geo_with_zctas.csv'
    inpath.tz     <- 'data/small_data/state_timezones.csv'
    inpath.offset <- 'data/small_data/timezone_offsets.csv'
    inpath.cbsas  <- 'data/small_data/cbsa_codenames.csv'
    ## To chains and cuisines
    inpath.chains.cuisines <- 'output/process_yipit/top_chains_and_cuisines.rds'
    ## To fee cap data
    inpath.caps <- 'data/fee_caps/monthly_fee_caps.csv'
    ## To nearby denmographics
    inpath.demos <- 'data/ACS/processed/ACS_nearby.csv'

    # Specify output paths
    outdir <- 'data/web_harvesting/analysis_data'
    create.dir(outdir)
    outpath.dat  <- sprintf('%s/analysis_data.rds', outdir)

    # Settings
    max.dist <- 100
    nchains <- 25
    ncuisines <- 50
    min.obs <- 100 # for including county FEs

    # Read the data
    ## Details
    df.uber <- read.dat(inpath.uber.details)
    df.dd   <- read.dat(inpath.dd.details)
    df.gh   <- read.dat(inpath.gh.details)
    df.pm   <- read.dat(inpath.pm.details)

    ## Nondelivery
    uber.nd <- read.dat(inpath.uber.nd)
    dd.nd   <- read.dat(inpath.dd.nd)
    pm.nd   <- read.dat(inpath.pm.nd)

    ## Geography
    geo <- read.dat(inpath.geo, colClasses = c('zip' = 'character', 'zcta' = 'character'))
    cbsa.dat <- read.dat(inpath.cbsas)

    ## Time
    state.tz <- read.dat(inpath.tz)
    tz.offset <- read.dat(inpath.offset)
    state.tz <- left_join(x = state.tz, y = tz.offset, by = 'timezone')

    ## Chains and cuisines
    chains.cuisines <- readRDS(inpath.chains.cuisines)
    top.chains   <- names(chains.cuisines$top.chains)
    top.chains   <- top.chains[top.chains != '']
    top.chains   <- top.chains[1:nchains]
    top.cuisines <- names(chains.cuisines$top.cuisines)
    top.cuisines <- top.cuisines[1:ncuisines]

    ## Fee caps
    caps.df <- read.dat(inpath.caps, colClasses = c('zip' = 'character'))
    caps.df <- rename.var(caps.df, 'zip', 'zcta')

    ## Demographics
    demos <- read.dat(inpath.demos, colClasses = c('zcta' = 'character'))

    # Add distances
    df.dd <- df.dd[which(df.dd$latitude != 'missing' & df.dd$longitude != 'missing'), ]
    df.dd$longitude <- as.numeric(df.dd$longitude)
    df.dd$latitude  <- as.numeric(df.dd$latitude)
    df.uber <- add.dists(df.uber)
    df.dd   <- add.dists(df.dd)
    df.pm   <- add.dists(df.pm)
    pm.nd   <- add.dists(pm.nd)

    ## Limit distances
    df.uber$distance[which(df.uber$distance >= max.dist)] <- NA
    df.dd$distance[which(df.dd$distance >= max.dist)]     <- NA
    df.gh$distance[which(df.gh$distance >= max.dist)]     <- NA
    df.pm$distance[which(df.pm$distance >= max.dist)]     <- NA
    pm.nd$distance[which(pm.nd$distance >= max.dist)]     <- NA

    # Process the Grubhub delivery fee variable
    ## Delete missing delivery fees
    df.gh <- df.gh[which(df.gh$delivery_fee != 'missing'), ]
    ## Delete proportional delivery fees
    df.gh <- df.gh[which(!grepl('[0-9]%', df.gh$delivery_fee)), ]
    df.gh$delivery_fee <- sub('Free', '$0.00', df.gh$delivery_fee)
    ## Remove "+"
    df.gh$delivery_fee <- sub('\\+', '', df.gh$delivery_fee)
    ## Parsing
    df.gh$delivery_fee <- sub(' delivery', '', df.gh$delivery_fee)
    df.gh$delivery_fee <- sub('^\\$', '', df.gh$delivery_fee)
    df.gh$delivery_fee <- as.numeric(df.gh$delivery_fee)

    # Add some corrections to Postmates nondelivery variables
    idx <- which(pm.nd$spec_fee_name == 'CA Driver Benefits')
    pm.nd$spec_fee_name[idx] <- 'none'
    pm.nd$spec_fee_amt[idx]  <- 0.00
    idx <- which(pm.nd$spec_fee_name %in% c('Merchant Fee', 'Bag Fee'))
    pm.nd$spec_fee_name[idx] <- 'none'
    pm.nd$spec_fee_amt[idx]  <- 0.00

    # Keep only restaurants with valid delivery fees
    df.uber <- subset.df(df.uber)
    df.dd   <- subset.df(df.dd)
    df.pm   <- subset.df(df.pm)

    # Construct day of week and time of day variables
    df.uber <- add.time.vars(df.uber, state.tz)
    df.dd   <- add.time.vars(df.dd, state.tz)
    df.gh   <- add.time.vars(df.gh, state.tz)

    df.pm$collection_time <- df.pm$time
    df.pm$state <- df.pm$address_region
    df.pm   <- add.time.vars(df.pm, state.tz)

    # Add an Uber Eats market variable
    df.uber$mkt <- sapply(df.uber$link, extract.mkt)
    df.uber$mkt <- as.factor(df.uber$mkt)

    # Construct a price category variable. Each branch guards against the
    # source column being absent (synthetic-data path), in which case
    # price_category is left as NA.
    df.uber$price_category <- if ('priceRange' %in% colnames(df.uber)) as.factor(df.uber$priceRange) else NA
    df.dd$price_category   <- if ('priceRange' %in% colnames(df.dd))   as.factor(df.dd$priceRange)   else NA
    ## Grubhub's needs to be recorded
    if ('price_range' %in% colnames(df.gh)) {
        df.gh$price_category <- c('$', '$$', '$$$', '$$$$')[df.gh$price_range]
        df.gh$price_category <- as.factor(df.gh$price_category)
    } else {
        df.gh$price_category <- factor(NA, levels = c('$', '$$', '$$$', '$$$$'))
    }
    ## Same with Postmates
    if ('price_range' %in% colnames(pm.nd)) {
        pm.nd$price_category <- c('$', '$$', '$$$', '$$$$')[pm.nd$price_range + 1]
        pm.nd$price_category <- as.factor(pm.nd$price_category)
    } else {
        pm.nd$price_category <- factor(NA, levels = c('$', '$$', '$$$', '$$$$'))
    }

    # Construct a "busy" indicator
    df.uber$busy <- 1*(df.uber$busy_flag == 'True')

    # Add continuous waiting time variables
    df.uber$wait_time <- sapply(df.uber$delivery_time, function(x) time.fn(x, 'uber'))
    df.dd$wait_time   <- sapply(df.dd$delivery_time,   function(x) time.fn(x, 'doordash'))
    df.gh$wait_time   <- sapply(df.gh$delivery_time,   function(x) time.fn(x, 'grubhub'))
    df.pm$wait_time   <- sapply(df.pm$wait_time,       function(x) time.fn(x, 'postmates'))
    pm.nd$wait_time   <- sapply(pm.nd$wait_time,       function(x) time.fn(x, 'postmates'))

    # Merge in geographical variables
    keep.vars <- c('zip', 'zcta', 'city', 'state', 'CBSA_name', 'municipality', 'county_name', 'county')
    geo.sub <- geo[, keep.vars]
    ## Fix zip code variables
    df.uber$zip <- fix.zip.codes(df.uber$zip)
    df.dd$zip   <- fix.zip.codes(df.dd$zip)
    df.gh$zip   <- fix.zip.codes(df.gh$zip)
    df.pm$zip   <- fix.zip.codes(df.pm$zip)
    uber.nd$zip <- fix.zip.codes(uber.nd$zip)
    dd.nd$zip   <- fix.zip.codes(dd.nd$zip)
    pm.nd$zip   <- fix.zip.codes(pm.nd$zip)

    idx <- which(is.na(df.gh$zip))
    df.gh$zip[idx] <- fix.zip.codes(df.gh$postal_code[idx])

    ## Conduct merger
    df.uber$state <- NULL
    df.uber <- left_join(x = df.uber, y = geo.sub, by = 'zip')
    df.dd$state <- NULL
    df.dd <- left_join(x = df.dd, y = geo.sub, by = 'zip')
    df.gh$state <- NULL
    df.gh <- left_join(x = df.gh, y = geo.sub, by = 'zip')
    df.pm$state <- NULL
    df.pm <- left_join(x = df.pm, y = geo.sub, by = 'zip')

    ## Merge in demographic variables for nondelivery data
    uber.nd <- left_join(x = uber.nd, y = geo.sub, by = 'zip')
    dd.nd   <- left_join(x = dd.nd,   y = geo.sub, by = 'zip')
    pm.nd   <- left_join(x = pm.nd,   y = geo.sub, by = 'zip')

    # Add time variables to nondelivery datasets
    uber.nd <- rename.var(uber.nd, 'time', 'collection_time')
    dd.nd   <- rename.var(dd.nd,   'time', 'collection_time')
    pm.nd   <- rename.var(pm.nd,   'time', 'collection_time')
    uber.nd <- add.time.vars(uber.nd, state.tz)
    dd.nd   <- add.time.vars(dd.nd,   state.tz)
    pm.nd   <- add.time.vars(pm.nd,   state.tz)

    # Coerce chain/cuisine to character so add.restaurant.characteristics's
    # strsplit() does not fail when the join with listings_v2 returned no
    # matches (read.csv then loads the columns as logical NA).
    for (df.nm in c('df.uber', 'df.dd', 'df.gh', 'df.pm', 'pm.nd')) {
        df.x <- get(df.nm)
        if ('chain'   %in% colnames(df.x)) df.x$chain   <- as.character(df.x$chain)
        if ('cuisine' %in% colnames(df.x)) df.x$cuisine <- as.character(df.x$cuisine)
        assign(df.nm, df.x)
    }
    df.uber <- add.restaurant.characteristics(df.uber, top.chains, top.cuisines)
    df.dd   <- add.restaurant.characteristics(df.dd, top.chains, top.cuisines)
    df.gh   <- add.restaurant.characteristics(df.gh, top.chains, top.cuisines)
    df.pm   <- add.restaurant.characteristics(df.pm, top.chains, top.cuisines)
    pm.nd   <- add.restaurant.characteristics(pm.nd, top.chains, top.cuisines)

    # Add fee caps
    df.uber <- add.fee.caps(df.uber, caps.df)
    df.dd   <- add.fee.caps(df.dd,   caps.df)
    df.gh   <- add.fee.caps(df.gh,   caps.df)
    df.pm   <- add.fee.caps(df.pm,   caps.df)
    pm.nd   <- add.fee.caps(pm.nd,   caps.df)

    # Add demographics
    ## Take a subset of the demographic dataset
    demos$pop_over_15 <- NULL
    demos$pop_over_18 <- NULL
    for (k in grep('^X', colnames(demos), value = TRUE)){
        demos[[k]] <- NULL
    }
    demos$share_uni <- demos$share_college + demos$share_advanced
    demos$share_college <- NULL
    demos$share_advanced <- NULL
    demos$share_20s <- demos$share_20_to_24 + demos$share_25_to_29
    demos$share_30s <- demos$share_30_to_34 + demos$share_35_to_39
    demos$share_40s <- demos$share_40_to_44 + demos$share_45_to_49
    demos$share_50s <- demos$share_50_to_54 + demos$share_55_to_59
    demos$share_60plus <- demos$share_60_to_64 + demos$shr_65p
    for (k in grep('share_[0-9]{2}_to', colnames(demos), value = TRUE)){
        demos[[k]] <- NULL
    }
    # Put population in millions
    demos$population <- demos$population/1e6

    df.uber <- left_join(df.uber, demos, by = 'zcta')
    df.dd   <- left_join(df.dd,   demos, by = 'zcta')
    df.gh   <- left_join(df.gh,   demos, by = 'zcta')
    df.pm   <- left_join(df.pm,   demos, by = 'zcta')
    pm.nd   <- left_join(pm.nd,   demos, by = 'zcta')

    # Special geographies
    geo.sub <- geo[which(geo$CBSA_name %in% cbsa.dat$CBSA_name), ]
    markets <- unique(cbsa.dat$CBSA_name)

    geo.sub$geo <- geo.sub$CBSA_name
    for (market in markets){
        all.counties.dd   <- s.table(df.dd$county[which(df.dd$CBSA_name == market)])
        all.counties.uber <- s.table(df.uber$county[which(df.uber$CBSA_name == market)])
        all.counties.gh   <- s.table(df.gh$county[which(df.gh$CBSA_name == market)])

        top.counties.dd   <- names(all.counties.dd)[all.counties.dd >= min.obs]
        top.counties.uber <- names(all.counties.uber)[all.counties.uber >= min.obs]
        top.counties.gh   <- names(all.counties.gh)[all.counties.gh >= min.obs]

        top.counties <- Reduce(intersect, list(top.counties.dd, top.counties.gh,
                                               top.counties.uber))

        # For county fixed effects to be identified the within-platform reference
        # category must be non-empty: if every observed county for a platform
        # ends up in `top.counties`, drop that platform's smallest county so the
        # CBSA-level residual category retains at least one record.
        if (all(names(all.counties.uber) %in% top.counties)){
            top.counties <- setdiff(top.counties, names(all.counties.uber)[length(all.counties.uber)])
        }
        if (all(names(all.counties.dd) %in% top.counties)){
            top.counties <- setdiff(top.counties, names(all.counties.dd)[length(all.counties.dd)])
        }
        if (all(names(all.counties.gh) %in% top.counties)){
            top.counties <- setdiff(top.counties, names(all.counties.gh)[length(all.counties.gh)])
        }

        idx <- which(geo.sub$county %in% top.counties)
        geo.sub$geo[idx] <- geo.sub$county[idx]
    }
    geo.sub <- geo.sub[which(geo.sub$is.zcta), ]
    df.dd   <- left_join(df.dd,   geo.sub[, c('zcta', 'geo')], by = 'zcta')
    df.uber <- left_join(df.uber, geo.sub[, c('zcta', 'geo')], by = 'zcta')
    df.gh   <- left_join(df.gh,   geo.sub[, c('zcta', 'geo')], by = 'zcta')
    df.pm   <- left_join(df.pm,   geo.sub[, c('zcta', 'geo')], by = 'zcta')
    pm.nd   <- left_join(pm.nd,   geo.sub[, c('zcta', 'geo')], by = 'zcta')

    output.data <- list(uber.details = df.uber,
                        dd.details   = df.dd,
                        gh.details   = df.gh,
                        uber.nd      = uber.nd,
                        dd.nd        = dd.nd,
                        pm.nd        = pm.nd)
    saveRDS(file = outpath.dat, object = output.data)
}

add.fee.caps <- function(df, caps.df){
    # Add fee caps. Web-harvested observations outside the caps panel's date range
    # inherit the latest available cap snapshot (May 2021).
    caps.df$month2 <- sub('-01$', '', caps.df$month)
    df$month2 <- df$month
    df$month2[which(!(df$month %in% unique(caps.df$month2)))] <- '2021-05'
    df <- left_join(df, caps.df[, c('zcta', 'month2', 'cap')], by = c('zcta', 'month2'))
    df$month2 <- NULL
    return(df)
}

add.dists <- function(df){
    # Add distances to df
    dists <- c()
    for (k in 1:nrow(df)){
        collect.loc <- cbind(df$collection_lon[k], df$collection_lat[k])
        resto.loc   <- cbind(df$longitude[k],      df$latitude[k])
        dist.k   <- distGeo(p1 = collect.loc, p2 = resto.loc)
        dists[k] <- dist.k
    }
    df$distance <- dists/1000 # distance in kilometres
    return(df)
}

subset.df <- function(df){
    # Subset a food delivery details data.frame
    df <- df[which(df$delivery_fee != -1), ]
    if ('distance' %in% colnames(df)){
        df <- df[which(df$distance <= 10), ]
    }

    if ('collection_loc' %in% colnames(df)){
        df <- df[which(!grepl('Windsor, Ontario', df$collection_loc)), ]
    }
    return(df)
}

add.time.vars <- function(df, state.tz){
    # Early-exit guard for synthetic / empty inputs: downstream column
    # assignment via df$x <- '' fails on zero-row data frames.
    if (nrow(df) == 0) return(df)
    # Construct day of week and time of day variables
    df$day     <- sub(' .*', '', df$collection_time)
    df$day     <- as.Date(df$day)
    df$weekday <- weekdays(df$day)
    df$weekday <- as.factor(df$weekday)
    ## Time of day in hours (round down)
    df$time.of.day <- sub('^[0-9/]+ ', '', df$collection_time)
    df$time.of.day <- sub(':.*', '', df$time.of.day)
    df$time.of.day <- as.numeric(df$time.of.day)

    # Add state abbreviations
    if ('collection_state' %in% colnames(df)){
        df$state.abbrev <- df$resto_state
    } else if ('state' %in% colnames(df)){
        df$state.abbrev <- df$state
        df$state <- NULL
    } else if ('collection_loc' %in% colnames(df)){
        df$state.abbrev <- sapply(df$collection_loc, determine.state)
    } else if ('addressRegion' %in% colnames(df)){
        df$state.abbrev <- df$addressRegion
    } else {
        df$state.abbrev <- NA_character_
    }
    df  <- df[which(!is.na(df$state.abbrev)), ]
    if (nrow(df) == 0) return(df)

    state.abb <- c(state.abb, 'DC')
    state.name <- c(state.name, 'District of Columbia')
    state.mapping <- data.frame(state.abbrev = state.abb, state = state.name, stringsAsFactors = FALSE)
    df$state.abbrev <- as.character(df$state.abbrev)

    df <- merge(x = df, y = state.mapping, by = 'state.abbrev',
                all.x = TRUE, all.y = FALSE)
    df <- merge(x = df, y = state.tz, by = 'state',
               all.x = TRUE, all.y = FALSE)
    df$time.of.day <- (df$time.of.day + df$offset) %% 24

    ## Time of day in periods
    df$morning   <- 1*(df$time.of.day >= 5  & df$time.of.day < 11)
    df$midday    <- 1*(df$time.of.day >= 11 & df$time.of.day < 14)
    df$afternoon <- 1*(df$time.of.day >= 14 & df$time.of.day < 17)
    df$evening   <- 1*(df$time.of.day >= 17 & df$time.of.day < 21)
    df$night     <- 1*(df$time.of.day >= 21)
    df$late      <- 1*(df$time.of.day < 5)

    periods <- c('morning', 'midday', 'afternoon', 'evening', 'night', 'late')
    df$period <- ''
    for (p in periods){
        df$period[df[, p] == 1] <- p
    }
    df$period <- as.factor(df$period)

    # Month
    df$month <- sub('/[0-9]+ [0-9]+:[0-9]+:[0-9]+$', '', df$collection_time)
    df$month <- sub('/', '-', df$month)

    # Get rid of state abbreviation, which will not be used from this point on
    df$state.abbrev <- NULL
    return(df)
}

determine.state <- function(x){
    if (grepl('[A-Z]{2}$', x) & !grepl('USA?$', x)){
        y <- str_extract(x, '[A-Z]{2}$')
    } else {
        y <- str_extract_all(x, ' [A-Z]{2},')
        y <- y[[1]]
        y <- y[length(y)]
        y <- substr(y, 2, 3)
    }
    return(y)
}

# Market indicators
extract.mkt <- function(x){
    y <- str_extract(x, 'www.ubereats.com/[a-z_-]+/food-delivery')
    y <- str_extract(y, '/[a-z_-]+/')
    y <- gsub('/', '', y)
    return(y)
}

time.fn <- function(x, platform){
    # Create continuous delivery time measure
    if (platform == 'grubhub'){
        if (grepl('mins$', x)){
            x <- sub(' mins', '', x)
            x.0 <- sub('-[0-9]*', '', x)
            x.1 <- sub('^[0-9]+-', '', x)
            x.0 <- as.numeric(x.0)
            x.1 <- as.numeric(x.1)
            x.avg <- 0.5*(x.0 + x.1)
        } else {
            x.avg <- NA
        }
        return(x.avg)
    }

    if (platform == 'doordash'){
        x <- sub(' - ', ' to ', x)
        x <- sub('minutes', 'min', x)
    }

    if (platform == 'postmates'){
        x = sub('[-—]', ' to ', x)
        if (grepl('^[0-9]+ min$', x)){
            x <- sub(' min', '', x)
            x <- sprintf('%s to %s', x, x)
        }
    }

    if (x == 'missing'){
        y <- NA
    } else {
        x1 <- as.numeric(sub(' to.*', '', x))
        x2 <- sub('^[0-9]+ to ', '', x)
        x2 <- sub(' [mM]in$',    '', x2)
        x2 <- as.numeric(x2)
        y <- 0.5*(x1 + x2)
    }
    return(y)
}


main()
