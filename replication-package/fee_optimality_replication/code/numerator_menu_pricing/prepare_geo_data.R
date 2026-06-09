# Prepare geographical data for the analysis of Numerator menu item prices
# Two outputs
# - ZIP/month-level
# - ZIP3/platform/month-level

library(FoodDeliveryTools)
library(EconTools)


main <- function(){
    
    # Specify input paths
    ## To adoption data
    inpath.yipit <- 'data/yipitdata/listings_v2.csv'
    ## To fee caps
    inpath.cap <- 'data/fee_caps/zip_fee_caps.rds'
    
    # Specify output paths
    ## To listing counts data
    outpath.comp <- 'data/numerator/nlistings_for_price_analysis.rds'
    ## To commission cap data
    outpath.cap <- 'data/numerator/commission_cap_for_price_analysis.rds'
    
    # Load data
    yipit <- data.table::fread(inpath.yipit)
    yipit <- as.data.frame(yipit)
    cap <- readRDS(inpath.cap)
    cap <- cap$zip.df
    
    # Processing
    process.cap(cap, outpath.cap)
    process.competition(yipit, outpath.comp)
}

process.cap <- function(cap, outpath.cap){
    # Process commission cap data for analysis of menu prices
    cap$cap[is.infinite(cap$cap)] <- 0.30
    cap$month <- sub('-[0-9]{2}$', '', cap$date)
    excl <- aggregate(excl_chains ~ zip + month, FUN = mean, data = cap)
    cap <- aggregate(cap ~ zip + month, FUN = max, data = cap)
    cap <- dplyr::left_join(cap, excl, by = c('zip', 'month'))
    
    ## Extend cap data to the end of 2021
    zips <- unique(cap$zip)
    Caps <- list()
    add.months <- c('2021-07', '2021-08', '2021-09', '2021-10', '2021-11', '2021-12')
    for (z in zips){
        cap.z <- cap[which(cap$zip == z), ]
        cap.z <- cap.z[order(cap.z$month), ]
        cap.lvl <- as.numeric(tail(cap.z, 1)['cap'])
        excl.z  <- as.numeric(tail(cap.z, 1)['excl_chains'])
        new.df <- data.frame(zip = z, month = add.months, cap = cap.lvl, excl_chains = excl.z)
        cap.z <- dplyr::bind_rows(cap.z, new.df)
        Caps[[z]] <- cap.z
    }
    cap <- do.call(dplyr::bind_rows, Caps)
    
    # ZIPs with caps excluding chains
    excl.zips <- unique(cap$zip[which(cap$excl_chains > 0 & cap$cap < 0.30)])
    cap$excl_zip <- 1*(cap$zip %in% excl.zips)
    cap$has_cap <- 1*(cap$cap < 0.30)

    saveRDS(cap, outpath.cap)
}

process.competition <- function(yipit, outpath.comp){
    # Merge in platform adoption by ZIP3 
    yipit$zip <- fix.zip.codes(yipit$postal_code)
    yipit$zip3 <- substr(yipit$zip, 1, 3)
    
    # Collapse yipit to platform x month x zip3
    yipit.agg <- doBy::summaryBy(restaurant_id ~ platform + observation_month + zip3,
                                 data = yipit, FUN = length, keep.names = TRUE)
    yipit.agg <- yipit.agg[which(yipit.agg$platform %in% c('DoorDash', 'Grubhub', 'UberEats')), ]
    yipit.agg$platform[which(yipit.agg$platform == 'DoorDash')] <- 'dd'
    yipit.agg$platform[which(yipit.agg$platform == 'Grubhub')]  <- 'gh'
    yipit.agg$platform[which(yipit.agg$platform == 'UberEats')] <- 'uber'
    yipit.agg <- rename.var(yipit.agg, 'restaurant_id', 'n_listings')
    yipit.agg <- rename.var(yipit.agg, 'observation_month', 'month')
    yipit.agg$month <- sub('-[0-9]{2}$', '', yipit.agg$month)
    yipit.agg$log_listings <- log(yipit.agg$n_listings)
    
    saveRDS(yipit.agg, outpath.comp)
}

main()


