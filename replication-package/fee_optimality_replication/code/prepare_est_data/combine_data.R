# Prepare data for demand estimation in MATLAB
# by combining the following datasets:
# - Consumer choice
# - Consumer demographics
# - Restaurant platform adoption

library(data.table)
library(dplyr)
library(EconTools)

main <- function(){
    partner.only    <- TRUE
    fee.version     <- '_v3'
    menu.price.type <- 'did'
    subsetting      <- TRUE

    combine.data('april', fee.version = fee.version,
                 partner.only = partner.only, menu.price.type = menu.price.type,
                 subsetting = subsetting)
    combine.data('may',   fee.version = fee.version,
                 partner.only = partner.only, menu.price.type = menu.price.type,
                 subsetting = subsetting)
    combine.data('june',  fee.version = fee.version,
                 partner.only = partner.only, menu.price.type = menu.price.type,
                 subsetting = subsetting)
}

combine.data <- function(month, fee.version = '_v4', partner.only = TRUE,
                         menu.price.type = 'hedonic', subsetting = FALSE, ig.year = '2021'){
    # Combine the consumer panel data with the restaurant location data

    # Maximum number of purchases
    max.nbuy <- 10
    
    hedonic <- (menu.price.type == 'hedonic')
    psample <- (menu.price.type == 'psample')
    did     <- (menu.price.type == 'did')

    # Specify paths
    psuffix <- ifelse(partner.only, '_partnered', '')
    ssuffix <- ifelse(subsetting, '_sset', '')

    if (hedonic | psample | did){
        rho.suffix <- paste0('_', menu.price.type)
    } else {
        rho.suffix <- ''
    }

    dat.dir <- 'data/est_dat'
    inpath.mz <- sprintf('%s/month_zip_data%s%s%s_%s.csv', dat.dir,
                         fee.version, psuffix, rho.suffix, month)
    inpath.buy <- sprintf('data/numerator/regression_data-%s%s.rds', month, fee.version)
    inpath.ppl <- 'data/numerator/people_table_combined.csv'
    inpath.cbsa.codes <- 'data/small_data/cbsa_codenames.csv'
    inpath.geo <- 'data/geo/geo_with_zctas.csv'

    outdir <- 'data/est_dat'
    create.dir(outdir)

    if (ig.year != ''){
        outpath <- sprintf('%s/estimation_sample-yipitdata_ig%s%s%s%s%s_%s.csv',
                           outdir, ig.year, fee.version, psuffix,
                           rho.suffix, ssuffix, month)
    } else {
        outpath <- sprintf('%s/estimation_sample-yipitdata%s%s%s%s_%s.csv',
                           outdir, fee.version, psuffix, rho.suffix,
                           ssuffix, month)
    }
    outpath.cbsa <- sprintf('%s/cbsa_ids-yipitdata%s%s%s_%s.csv',
                            outdir, psuffix, rho.suffix, ssuffix, month)

    ## Load ZIP code data on portfolios
    CC <- c('zip' = 'character', 'zcta' = 'character')
    mz <- read.dat(inpath.mz, colClasses = CC)
    ## Load demographics
    ppl <- fread(inpath.ppl)
    ppl <- subset(ppl, POSTAL_CODE != 'na')

    ## Load transactions
    buy <- readRDS(inpath.buy)
    buy <- buy$buy

    # Subsetting
    if (subsetting){
        lower.lim <- 5.90 # 1st percentile in april
        low <- rep(0, times = nrow(buy))
        low[which(buy$BASKET_SUB_TOTAL <= lower.lim)] <- 1
        buy <- buy[which(low == 0), ]
    }

    # Ensure that each consumer has a ZCTA
    geo <- read.dat(inpath.geo, colClasses = CC)
    geo <- geo[, c('zip', 'zcta', 'is.zcta')]
    buy <- left_join(buy, geo, by = 'zip')
    buy$zip[which(!buy$is.zcta)] <- buy$zcta[which(!buy$is.zcta)]
    buy$zcta <- NULL
    buy$is.zcta <- NULL
    buy <- buy[, c('USER_ID', 'month', 'zip', 'platform', 'is_chain',
                   'in_static', 'static_trans')]

    # Add platform code
    platforms <- c('outside', 'dd', 'uber', 'gh', 'pm')
    platform.df <- data.frame(f = 0:4, platform = platforms)
    buy <- inner_join(buy, platform.df, by = 'platform')
    buy[, platform := NULL]

    # New USER ID: 1 to n.user
    buy <- buy[order(buy$USER_ID), ]
    buy$id <- as.numeric(as.factor(buy$USER_ID))
    # Add time variable for panel reasons
    users <- unique(buy$id)
    buy$t <- 1
    for (user in users){
        idx <- which(buy$id == user)
        buy[idx, t := 1:length(idx)]
    }
    # Month codes
    buy$month <- as.numeric(sub('2021-0', '', buy$month))
    
    # Drop orders after `max.nbuy`
    buy <- buy[which(buy$t <= max.nbuy), ]

    # Add in purchase counts
    id.lvl <- compute.purchase.counts(buy, which.restos = 'all')
    id.c   <- compute.purchase.counts(buy, which.restos = 'chain')
    id.i   <- compute.purchase.counts(buy, which.restos = 'indep')

    ## Merge in chains
    f.c.vars <- grep('^f[0-9]', colnames(id.c), value = TRUE)
    keep.vars <- c('id', f.c.vars)
    id.lvl <- dplyr::left_join(id.lvl, id.c[, keep.vars], by = 'id')
    ### Fill in missing values
    for (v in f.c.vars){
        idx <- which(is.na(id.lvl[, v]))
        id.lvl[idx, v] <- 0
    }
    ## Merge in independents
    f.i.vars <- grep('^f[0-9]', colnames(id.i), value = TRUE)
    keep.vars <- c('id', f.i.vars)
    id.lvl <- dplyr::left_join(id.lvl, id.i[, keep.vars], by = 'id')
    ### Fill in missing values
    for (v in f.i.vars){
        idx <- which(is.na(id.lvl[, v]))
        id.lvl[idx, v] <- 0
    }

    # Merge in ZIP-level data
    mz$month <- NULL
    id.lvl <- dplyr::left_join(id.lvl, mz, by = 'zip')

    ## Keep selected demographics
    ppl$young <- 1*(ppl$AGE_BUCKET %in% c('18-20', '21-24', '25-34'))
    ppl$married <- 1*(ppl$MARITAL_STATUS == 'Married')
    ppl$high_income <- 1*(ppl$INCOME_BUCKET != 'low') # Low = <$40k
    keep.vars <- c('USER_ID', 'young', 'married', 'high_income')
    ppl <- ppl[, ..keep.vars]
    ## Merge in demographics
    id.lvl <- inner_join(id.lvl, ppl, by = 'USER_ID')

    # Add CBSA code (numeric)
    cbsas <- sort(unique(id.lvl$CBSA_name))
    cbsas <- data.frame(CBSA_name = cbsas, m = 1:length(cbsas))
    write.table(cbsas, file = outpath.cbsa, row.names = FALSE,
                quote = TRUE, sep = '|')
    id.lvl <- inner_join(id.lvl, cbsas, by = 'CBSA_name')
    id.lvl$CBSA_name <- NULL
    id.lvl$T_i <- rowSums(id.lvl[, grep('^f[0-9]+$', colnames(id.lvl))])

    # Save the data to be used by MATLAB estimation script
    write.csv(id.lvl, file = outpath, row.names = FALSE, quote = FALSE)
}

compute.purchase.counts <- function(buy, which.restos = 'all'){
    # Compute panelist-level purchase counts
    buy <- as.data.frame(buy)

    if (which.restos == 'all'){
        buy.sub <- buy
        vname <- ''
    } else if (which.restos == 'chain'){
        buy.sub <- buy[which(buy$is_chain == 1), ]
        vname <- 'c'
    } else if (which.restos == 'indep'){
        buy.sub <- buy[which(buy$is_chain == 0), ]
        vname <- 'i'
    }
    if (nrow(buy.sub) == 0) {
        # Empty slice: emit a zero-row frame that still carries the f0..f4
        # columns the downstream join expects (with the `vname` suffix), so
        # the left_join in main() is a no-op for this restaurant type.
        f.cols <- sprintf('f%d%s', 0:4, vname)
        out <- data.frame(id = integer(0))
        for (fc in f.cols) out[[fc]] <- integer(0)
        return(out)
    }
    choice.counts <- buy.sub[, c('id', 'f')]
    # Always emit f0..f4 so downstream check_contradiction.R does not fail
    # when a synthetic month lacks purchases on some platform.
    f.vals <- 0:4
    for (k in f.vals){
        varname <- sprintf('f%d%s', k, vname)
        choice.counts[, varname] <- 1*(choice.counts$f == k)
    }
    choice.counts$f <- NULL
    choice.counts <- doBy::summaryBy(. ~ id, data = choice.counts,
                                     FUN = sum, keep.names = TRUE)
    keep.vars <- c('id', 'USER_ID', 'month', 'zip', 'in_static', 'static_trans')
    id.lvl <- buy.sub[, keep.vars]
    id.lvl <- id.lvl[!duplicated(id.lvl), ]
    id.lvl <- dplyr::left_join(id.lvl, choice.counts, by = 'id')

    return(id.lvl)
}

main()
