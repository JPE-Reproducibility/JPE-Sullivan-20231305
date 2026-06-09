library(data.table)
library(tidyr)
library(dplyr)
library(EconTools)

library(FoodDeliveryTools)

main <- function(){
    for (time.period in c('april', 'may', 'june')){
        prepare.data(time.period)
    }
}

prepare.data <- function(time.period){
    # Prepare data for use in regressions and demand estimation

    inpath.price <- 'data/prices/price_indices_v3.csv'
    # Purchase data
    inpath.static <- 'data/numerator/all_baskets_static-combined.rds'
    inpath.non    <- 'data/numerator/all_baskets_nonstatic-combined.rds'
    # Panelist data
    inpath.tab.static <- 'data/numerator/static_users-combined.rds'
    inpath.tab.non    <- 'data/numerator/nonstatic_users-combined.rds'
    inpath.ppl        <- 'data/numerator/standard_nmr_feed_people_table.csv'

    # Path to output
    outpath.dat <- sprintf('data/numerator/regression_data-%s_v3.rds', time.period)

    # Size of an order
    order.p <- 30

    # Load data
    geo <- fread(inpath.price)
    buy.static <- readRDS(inpath.static)
    buy.non    <- readRDS(inpath.non)
    buy.static <- data.table(buy.static)
    buy.non    <- data.table(buy.non)
    tab.static <- readRDS(inpath.tab.static)
    tab.non    <- readRDS(inpath.tab.non)
    ppl        <- fread(inpath.ppl)

    # Drop some variables
    drop.var <- c('START_DATE', 'END_DATE', 'PAYMENT_METHOD',
                  'STORE_NUMBER', 'ORDER_METHOD_TYPE', 'DELIVERY_METHOD_TYPE',
                  'DELIVERY_PROVIDER', 'POSTAL_CODE', 'METRO_AREA')
    for (dv in drop.var){
        buy.static[, (dv) := NULL]
        buy.non[, (dv) := NULL]
    }
    buy <- rbind(buy.static, buy.non)
    remove(list = c('buy.static', 'buy.non'))

    buy$TRANSACTION_DATE <- as.Date(buy$TRANSACTION_DATE)

    # Classify purchases as from a chain or an independent restaurant
    buy <- classify.chain(buy)

    # Keep only the selected month of data
    month.lookup <- c(april = '2021-04', may = '2021-05', june = '2021-06')
    buy <- subset(buy, month == month.lookup[time.period])
    # Merge in geo data
    ppl.vars <- c('USER_ID', 'POSTAL_CODE')
    buy <- left_join(buy, ppl[, ..ppl.vars], by = 'USER_ID')

    static.panelists <- tab.static$USER_ID[tab.static$END_DATE >= as.Date('2021-04-01')]
    non.panelists    <- tab.non$USER_ID[tab.non$END_DATE >= as.Date('2021-04-01')]
    panelists <- c(static.panelists, non.panelists)
    buy <- subset(buy, USER_ID %in% panelists)

    # Compute fees in addition to delivery and service fees
    platforms <- c('uber', 'dd', 'gh', 'pm')

    geo$dd.delivery   <- geo$dfee_dd_lasso
    geo$uber.delivery <- geo$dfee_uber_lasso
    geo$gh.delivery   <- geo$dfee_gh_lasso
    geo$pm.delivery   <- geo$dfee_pm_lasso

    geo$dd.WT   <- geo$WT_dd_lasso
    geo$uber.WT <- geo$WT_uber_lasso
    geo$gh.WT   <- geo$WT_gh_lasso
    geo$pm.WT   <- geo$WT_pm_lasso

    for (platform in platforms){
        new.var   <- paste0(platform, '.add')
        dfee.var  <- paste0(platform, '.delivery')
        sfee.var  <- paste0(platform, '.service.fee')
        total.var <- paste0(platform, '.price')

        geo[, (new.var) := .SD[[total.var]] - .SD[[dfee.var]] - .SD[[sfee.var]]*order.p]
    }

    # v3 price-index file is a single-month snapshot; month-subsetting is a no-op
    if ('month' %in% colnames(geo)){
        geo <- geo[which(geo$month == month.lookup[time.period]), ]
    }

    # Merge in prices
    keep.vars <- c('zip', 'CBSA_name',
                   'uber.add', 'dd.add', 'gh.add', 'pm.add',
                   'uber.price', 'dd.price', 'gh.price', 'pm.price',
                   'dd.WT', 'uber.WT', 'gh.WT', 'pm.WT')
    geo$zip <- fix.zip.codes(geo$zip)
    geo.sub <- geo[, ..keep.vars]

    # Subset to markets of interest
    buy <- buy[(buy$POSTAL_CODE %in% geo.sub$zip), ]

    buy <- rename.var(buy, 'POSTAL_CODE', 'zip')
    buy <- merge(buy, geo.sub, by = 'zip')

    # Save the data
    output.data <- list(buy = buy, geo = geo, panelists = panelists,
                        ppl = ppl)
    saveRDS(output.data, outpath.dat)
}


classify.chain <- function(buy){
    # Classify purchases as having taken place on a chain or not

    # Construct a restaurant name variable
    buy$restaurant <- buy$BANNER_ID
    idx.online <- which(buy$platform != 'outside')
    buy$restaurant[idx.online] <- buy$ORDER_PROVIDER[idx.online]
    buy.brand <- buy[, c('restaurant', 'platform')]
    buy.brand <- buy.brand[!duplicated(buy.brand), ]
    buy.brand <- as.data.frame(buy.brand)

    buy.brand$brand <- NA
    buy.brand$restaurant0 <- gsub('_', ' ', buy.brand$restaurant)
    for (k in 1:length(brand.map)){
        brand.pattern  <- brand.map[k]
        brand.name <- names(brand.map)[k]
        buy.brand <- assign.brand(buy.brand, brand.name, brand.pattern,
                                  restaurant.var = 'restaurant0')
    }

    # Merge the brand names into df.tot
    buy <- dplyr::left_join(buy, buy.brand[, c('restaurant', 'platform', 'brand')],
                            by = c('restaurant', 'platform'))
    buy$is_chain <- 1*!is.na(buy$brand)
    return(buy)
}


main()

