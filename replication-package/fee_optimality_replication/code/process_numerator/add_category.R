# To be run after identify_static_trans.py
library(data.table)

main <- function(){
    # All baskets
    use.combo <- TRUE
    combo.suffix <- ifelse(use.combo, '-combined', '')
    inpath.static <- sprintf('data/numerator/all_baskets_static%s.csv', combo.suffix)
    inpath.non    <- sprintf('data/numerator/all_baskets_nonstatic%s.csv', combo.suffix)
    outpath.static <- sub('csv$', 'rds', inpath.static)
    outpath.non    <- sub('csv$', 'rds', inpath.non)
    add.category(inpath.static, outpath.static)
    add.category(inpath.non, outpath.non)
}

add.category <- function(inpath, outpath){
    buy <- fread(inpath)
    # Add platform
    buy$platform <- 'outside'
    uber.names <- c('uberEats', 'Uber Eats', 'ubereats')
    gh.names   <- c('GrubHub', 'grubhubcom', 'Grubhub')
    idx <- (buy$BANNER_ID == 'ubereats') | (buy$DELIVERY_PROVIDER %in% uber.names)
    buy$platform[idx] <- 'uber'
    idx <- (buy$BANNER_ID == 'doordash') | (buy$DELIVERY_PROVIDER %in% c('DoorDash', 'doordash'))
    buy$platform[idx] <- 'dd'
    idx <- (buy$BANNER_ID == 'grubhubcom') | (buy$DELIVERY_PROVIDER %in% gh.names)
    buy$platform[idx] <- 'gh'
    idx <- (buy$BANNER_ID == 'postmates') | (buy$DELIVERY_PROVIDER == 'Postmates')
    buy$platform[idx] <- 'pm'

    # Classify transactions
    buy$category <- NA
    for (pl in c('uber', 'dd', 'gh', 'pm')){
        buy$category[buy$platform == pl] <- pl
    }
    delivery.types <- c('DELIVERY_SERVICE', 'SHIPPING', 'MULTIPLE')
    idx <- is.na(buy$category) & (buy$DELIVERY_METHOD_TYPE %in% delivery.types)
    buy$category[idx] <- 'other_delivery'
    buy$category[is.na(buy$category)] <- 'other'

    buy$date <- as.Date(buy$date)

    saveRDS(buy, file = outpath)
}

main()
