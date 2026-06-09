# Combine monthly estimation samples into a single panel; build
# zero-filled, static-only, and email-connected variants.
library(EconTools)

main <- function(){
    months <- c('april', 'may', 'june')
    fee.version <- '_v3'
    partner.only <- TRUE
    subsetting <- TRUE

    menu.price.type <- 'did'

    data.dir <- 'data/est_dat'
    combine.months(data.dir, months, fee.version, partner.only,
                   menu.price.type, subsetting)
}


combine.months <- function(data.dir, months, fee.version, partner.only,
                           menu.price.type, subsetting){
    # Combine datasets from several different months

    rho.suffix <- paste0('_', menu.price.type)

    ssuffix <- ifelse(subsetting, '_sset', '')

    inpath.connect <- 'data/numerator/static_connect_table.csv'

    if (partner.only){
        path.root <- sprintf('%s/estimation_sample-yipitdata_ig2021%s_possible%s%s',
                             data.dir, fee.version, rho.suffix, ssuffix)
        mz.root <- sprintf('%s/month_zip_data%s_partnered%s',
                           data.dir, fee.version, rho.suffix)

        outpath          <- sprintf('%s/combined_sample%s%s%s.csv', data.dir, fee.version, rho.suffix, ssuffix)
        outpath.zeros    <- sprintf('%s/combined_sample%s%s%s_0s.csv', data.dir, fee.version, rho.suffix, ssuffix)
        # Static
        outpath.s        <- sprintf('%s/static_sample%s%s%s.csv', data.dir, fee.version, rho.suffix, ssuffix)
        outpath.zeros.s  <- sprintf('%s/static_sample%s%s%s_0s.csv', data.dir, fee.version, rho.suffix, ssuffix)
        # Online connections only
        outpath.on       <- sprintf('%s/connect_sample%s%s%s.csv', data.dir, fee.version, rho.suffix, ssuffix)
        outpath.zeros.on <- sprintf('%s/connect_sample%s%s%s_0s.csv', data.dir, fee.version, rho.suffix, ssuffix)
    } else {
        path.root       <- sprintf('%s/estimation_sample-yipitdata_ig2021%s%s_possible_nonp',
                                   data.dir, fee.version, ssuffix)
        mz.root         <- sprintf('%s/month_zip_data%s%s', data.dir, fee.version, rho.suffix)

        outpath          <- sprintf('%s/combined_sample_nonp%s%s%s.csv', data.dir, fee.version, rho.suffix, ssuffix)
        outpath.zeros    <- sprintf('%s/combined_sample_nonp%s%s%s_0s.csv', data.dir, fee.version, rho.suffix, ssuffix)
        outpath.s        <- sprintf('%s/static_sample_nonp%s%s%s.csv', data.dir, fee.version, rho.suffix, ssuffix)
        outpath.zeros.s  <- sprintf('%s/static_sample_nonp%s%s%s_0s.csv', data.dir, fee.version, rho.suffix, ssuffix)
        outpath.on       <- sprintf('%s/connect_sample_nonp%s%s%s.csv', data.dir, fee.version, rho.suffix, ssuffix)
        outpath.zeros.on <- sprintf('%s/connect_sample_nonp%s%s%s_0s.csv', data.dir, fee.version, rho.suffix, ssuffix)
    }

    # Load month/zip level data
    mz <- list()
    for (month in months){
        inpath <- sprintf('%s_%s.csv',  mz.root, month)
        mz[[month]] <- read.dat(inpath, colClasses = c('zip' = 'character'))
    }
    mz <- Reduce(dplyr::bind_rows, mz)
    month.map <- c('april' = 4, 'may' = 5, 'june' = 6)
    mz$month <- month.map[mz$month]

    connect <- read.dat(inpath.connect, sep = '|')
    connect <- connect[order(connect$USER_ID, connect$END_DATE, decreasing = TRUE), ]
    connect <- connect[!duplicated(connect$USER_ID), ]
    # EMAIL_CONNECT flags panelists with email-receipt collection; absent in
    # synthetic data, fall back to keeping every connected USER_ID.
    if ('EMAIL_CONNECT' %in% colnames(connect)) {
        keep.IDs <- connect$USER_ID[which(connect$EMAIL_CONNECT == 1)]
    } else {
        keep.IDs <- connect$USER_ID
    }

    # Load choice data
    dat <- list()
    for (month in months){
        inpath <- sprintf('%s_%s.csv',  path.root, month)
        dat[[month]] <- read.dat(inpath, colClasses = c('zip' = 'character'))
    }
    dat <- Reduce(dplyr::bind_rows, dat)
    panel.id <- sprintf('%s-%s', as.character(dat$USER_ID), as.character(dat$month))
    panel.id <- as.numeric(as.factor(panel.id))
    dat$id <- panel.id

    # Version with zeros (i.e., fill in the missing USER/month pairs)
    ## Delete columns with month/zip level data
    incl.vars <- c('zip', 'month', 'm')
    dat0 <- dat[, c(incl.vars, setdiff(colnames(dat), c(colnames(mz), incl.vars)))]
    ## Add missing ZIP/month pairs
    dat0 <- tidyr::complete(dat0, USER_ID, month)
    ## Fill in missing values
    idx <- which(is.na(dat0$zip))
    dat.sub <- dat0[idx, ]
    ### Add in various consumer characteristics
    replace.vars <- c('zip', 'in_static', 'static_trans', 'young', 'married', 'high_income', 'm')
    user.lvl <- dat[, c('USER_ID', replace.vars)]
    user.lvl <- user.lvl[!duplicated(user.lvl$USER_ID), ]
    for (rv in replace.vars){
        dat.sub[[rv]] <- NULL
    }
    dat.sub <- dplyr::left_join(dat.sub, user.lvl, by = 'USER_ID')

    ### Next, add in various purchasing variables
    for (fvar in grep('^f[0-9][ci]?$', colnames(dat.sub), value = TRUE)){
        dat.sub[[fvar]] <- 0
    }
    dat.sub$T_i <- 0

    ### Next add in some remaining price variables
    if ('rho_online' %in% colnames(dat0)){
        rho.vars <- c('rho_online', 'rho_offline',
                      'rho_offline_cap', 'rho_online_cap')
        mz.rhos <- dat[, c('zip', rho.vars)]
        mz.rhos <- mz.rhos[!duplicated(mz.rhos$zip), ]
        for (x in rho.vars){
            dat.sub[, x] <- NULL
        }
        dat.sub <- dplyr::left_join(dat.sub, mz.rhos, by = 'zip')
    }

    # Add in the missing rows (dat0) into dat.sub
    dat0 <- dat0[which(!is.na(dat0$zip)), ]
    dat0 <- dplyr::bind_rows(dat0, dat.sub)
    if ('m' %in% colnames(mz)){
        dat0$m <- NULL
    }
    dat0 <- dplyr::left_join(dat0, mz, by = c('zip', 'month'))
    dat0 <- dat0[order(dat0$USER_ID, dat0$month), ]
    panel.id <- sprintf('%s-%s', as.character(dat0$USER_ID), as.character(dat0$month))
    panel.id <- as.numeric(as.factor(panel.id))
    dat0$id <- panel.id

    # Static-only versions
    dat.s  <- dat[which(dat$in_static == 1), ]
    dat0.s <- dat0[which(dat0$in_static == 1), ]

    # Delete some variables
    delete.vars <- c('in_static', 'static_trans', 'CBSA_name')
    for (dv in delete.vars){
        dat[, dv]    <- NULL
        dat0[, dv]   <- NULL
        dat.s[, dv]  <- NULL
        dat0.s[, dv] <- NULL
    }

    write.dat(x = dat, file = outpath)
    write.dat(x = dat0, file = outpath.zeros)

    write.dat(x = dat.s,  file = outpath.s)
    write.dat(x = dat0.s, file = outpath.zeros.s)

    # Only connected consumers (email receipts)
    dat.on  <- dat[which(dat$USER_ID %in% keep.IDs), ]
    dat0.on <- dat0[which(dat0$USER_ID %in% keep.IDs), ]
    write.dat(x = dat.on,  file = outpath.on)
    write.dat(x = dat0.on, file = outpath.zeros.on)
}


main()
