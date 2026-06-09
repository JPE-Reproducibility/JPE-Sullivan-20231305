# Prepare data for the analysis of Numerator menu item prices

library(EconTools)


main <- function(){
    # Specify paths
    inpath <- 'data/numerator/qsr_transactions_master.csv'
    inpath.s <- 'data/numerator/qsr_baskets_static-combined.csv'
    outpath <- 'data/numerator/item_level_price_panel.rds'
    
    
    dat <- data.table::fread(inpath)
    dat <- as.data.frame(dat)
    
    # Determine static status
    dat.s <- data.table::fread(inpath.s)
    dat.s <- as.data.frame(dat.s)
    dat.s <- dat.s[, c('BASKET_ID', 'static_trans')]
    dat.s <- dat.s[!duplicated(dat.s$BASKET_ID), ]
    dat <- dplyr::left_join(dat, dat.s, by = 'BASKET_ID')
    dat$static_trans[is.na(dat$static_trans)] <- FALSE
    
    # Delete some variables to preserve memory
    drop.vars <- c('PAYMENT_METHOD', 'METRO_AREA')
    for (dv in drop.vars){
        dat[, dv] <- NULL
    }
    
    dat <- add.time.variables(dat)
    dat <- generate.platform.variable(dat)
    
    # Add ZIP
    dat$zip <- suppressWarnings(fix.zip.codes(dat$POSTAL_CODE))
    dat$zip3 <- substr(dat$zip, 1, 3)
    
    # Subset the data
    dat <- subsetting(dat)
    
    saveRDS(dat, outpath)
}

add.time.variables <- function(dat){
    dat$month <- sub('-[0-9]{2}$', '', dat$TRANSACTION_DATE)
    dat$year  <- sub('-[0-9]{2}$', '', dat$month)
    return(dat)
}

generate.platform.variable <- function(dat){
    # Assign platforms
    ## DoorDash
    dat$platform <- dat$DELIVERY_PROVIDER
    idx.dd <- which(dat$BANNER_ID == 'doordash')
    dat$platform[idx.dd] <- 'dd'
    idx.dd <- which(dat$DELIVERY_PROVIDER == 'doordash')
    dat$platform[idx.dd] <- 'dd'
    idx.dd <- which(dat$DELIVERY_PROVIDER == 'DoorDash')
    dat$platform[idx.dd] <- 'dd'
    ## Uber Eats
    idx.uber <- which(dat$BANNER_ID == 'ubereats')
    dat$platform[idx.uber] <- 'uber'
    idx.uber <- which(dat$DELIVERY_PROVIDER == 'ubereats')
    dat$platform[idx.uber] <- 'uber'
    idx.uber <- which(dat$DELIVERY_PROVIDER == 'uberEats')
    dat$platform[idx.uber] <- 'uber'
    ## Grubhub
    idx.gh <- which(dat$BANNER_ID == 'grubhubcom')
    dat$platform[idx.gh] <- 'gh'
    idx.gh <- which(dat$DELIVERY_PROVIDER == 'grubhubcom')
    dat$platform[idx.gh] <- 'gh'
    idx.gh <- which(dat$DELIVERY_PROVIDER == 'GrubHub')
    dat$platform[idx.gh] <- 'gh'
    
    return(dat)
}

subsetting <- function(dat){
    # Apply various subsetting rules
    
    ## Valid ZIP
    dat <- dat[!is.na(dat$zip), ]
    dat <- dat[which(dat$POSTAL_CODE != 'na'), ]
    ## Valid price
    dat <- dat[which(dat$ITEM_UNIT_PRICE > 0), ]
    ## Time period
    dat <- dat[which(dat$year != '2019'), ]
    ## Platforms
    keep.platforms <- c('na', 'dd', 'uber', 'gh')
    dat <- dat[which(dat$platform %in% keep.platforms), ]
    
    # Drop variable
    dat$POSTAL_CODE <- NULL    
    
    return(dat)
}

main()
