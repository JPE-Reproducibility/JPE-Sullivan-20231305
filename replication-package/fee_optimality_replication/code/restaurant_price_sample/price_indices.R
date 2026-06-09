# Restaurant pricing regression on the hand-collected price sample (OA Table G1,
# output/restaurant_price_sample/reg_results.csv). The same script also writes
# price_indices.rds (offline + capped/uncapped online basket-subtotal indices)
# consumed by prepare_est_data/produce_month_zip_level.R in the 'psample'
# robustness branch.
library(EconTools)
library(fixest)

main <- function(){
    # Hand-collected prices (offline + DoorDash + UberEats + Grubhub) for
    # restaurants sampled across NYC, NJ (Hoboken), and CT (control).
    # The `wave` column identifies the collection draw
    # (seed_1868_list_1, seed_1867_list_4, seed_1_extra).
    inpath.sample <- 'data/small_data/restaurant_price_sample/restaurant_price_sample.csv'

    # Numerator basket data for online price level
    inpath.nmr1 <- 'data/numerator/all_baskets_nonstatic-combined.rds'
    inpath.nmr2 <- 'data/numerator/all_baskets_static-combined.rds'
    inpath.user <- 'data/numerator/people_table_combined.csv'
    inpath.caps <- 'data/fee_caps/monthly_fee_caps.csv'

    outpath     <- 'output/restaurant_price_sample/price_indices.rds'
    outpath.reg <- 'output/restaurant_price_sample/reg_results.csv'

    # Sample of online/offline price ratios, with commission-rate (`comm`)
    # assigned per arm (CT has no cap, so its commission is the 30% base rate).
    dat.longer <- process.data(inpath.sample)

    r.base <- 0.30
    r.cap  <- 0.15

    dat.longer$comm <- r.base
    dat.longer$comm[which(dat.longer$arm == 'NJ')]  <- 0.15
    dat.longer$comm[which(dat.longer$arm == 'NYC')] <- 0.20

    reg.cluster <- feols(ratio ~ comm, dat.longer, cluster = ~menu_item)

    alpha <- reg.cluster$coefficients[1]
    beta  <- reg.cluster$coefficients[2]
    ratio.no.cap <- alpha + r.base*beta
    ratio.cap    <- alpha + r.cap*beta

    # Mean online basket subtotal in places without a cap, used to scale the
    # ratio results into dollar-denominated offline/online price indices.
    nmr <- process.numerator(inpath.nmr1, inpath.nmr2, inpath.user, inpath.caps)
    idx <- which(nmr$platform != 'outside' & nmr$has_cap == 0)
    p.bar <- mean(nmr$BASKET_SUB_TOTAL[idx], na.rm = TRUE)

    p.off    <- p.bar/ratio.no.cap
    p.on     <- p.bar
    p.on.cap <- ratio.cap*p.off

    # OA Table G1
    reg.coef <- summary(reg.cluster)$coeftable
    col.1 <- c('$\\alpha$', '', '$\\beta$', '',
               '$N$',
               '$\\hat y_{jf}(r_j = 0.30)$',
               '$\\hat y_{jf}(r_j = 0.15)$')
    col.2 <- c(sprintf('%0.2f', reg.coef[1, 1]),
               sprintf('\\footnotesize (%0.2f)', reg.coef[1, 2]),
               sprintf('%0.2f', reg.coef[2, 1]),
               sprintf('\\footnotesize (%0.2f)', reg.coef[2, 2]),
               sprintf('%d', length(reg.cluster$residuals)),
               sprintf('%0.2f', c(ratio.no.cap, ratio.cap)))
    tab <- data.frame(var = col.1, val = col.2)
    write.dat(tab, outpath.reg)

    # Price indices consumed by the psample robustness branch
    p.indices <- c(off = p.off, on = p.on, on_cap = p.on.cap)
    saveRDS(p.indices, outpath)
}


process.data <- function(inpath.sample){
    dat <- read.csv(inpath.sample, stringsAsFactors = FALSE)
    dat <- dat[which(dat$restaurant_name != ''), ]

    # Drop restaurants flagged by the data collectors as unusable
    dat <- dat[which(!(dat$closed           %in% c('x', 'X')) &
                     !(dat$no_website       %in% c('x', 'X')) &
                     !(dat$no_menu          %in% c('x', 'X')) &
                     !(dat$not_a_restaurant %in% c('x', 'X'))), ]

    # Each row has two menu-item observations; reshape to one row per item
    dat.1 <- dat[, !grepl('_2$', colnames(dat))]
    dat.2 <- dat[, !grepl('_1$', colnames(dat))]
    colnames(dat.1) <- sub('_1', '', colnames(dat.1))
    colnames(dat.2) <- sub('_2', '', colnames(dat.2))
    dat.long <- dplyr::bind_rows(dat.1, dat.2)

    for (x in grep('_price', colnames(dat.long))){
        idx <- which(dat.long[, x] %in% c('x', 'X'))
        dat.long[idx, x] <- NA
        dat.long[, x] <- as.numeric(sub('$', '', as.character(dat.long[, x]), fixed = TRUE))
    }
    dat.long$dd_ratio   <- dat.long$doordash_price/dat.long$offline_price
    dat.long$uber_ratio <- dat.long$uber_price/dat.long$offline_price
    dat.long$gh_ratio   <- dat.long$grubhub_price/dat.long$offline_price

    dat.long$arm <- 'NYC'
    dat.long$arm[which(dat.long$state == 'CT')] <- 'CT'
    dat.long$arm[which(dat.long$state == 'NJ')] <- 'NJ'

    # Reshape further: one row per (menu item, platform)
    dat.long$id <- 1:nrow(dat.long)
    keep.all  <- c('id', 'restaurant_name', 'menu_item', 'street_address',
                   'city', 'state', 'muni', 'search_query', 'offline_price', 'arm')
    dat.dd    <- dat.long[, c(keep.all, 'doordash_price', 'dd_ratio')]
    dat.uber  <- dat.long[, c(keep.all, 'uber_price',     'uber_ratio')]
    dat.gh    <- dat.long[, c(keep.all, 'grubhub_price',  'gh_ratio')]
    new.names <- c(keep.all, 'online_price', 'ratio')
    colnames(dat.dd)   <- new.names
    colnames(dat.uber) <- new.names
    colnames(dat.gh)   <- new.names
    dat.dd$platform   <- 'dd'
    dat.uber$platform <- 'uber'
    dat.gh$platform   <- 'gh'

    dat.longer <- do.call(dplyr::bind_rows, list(dat.dd, dat.uber, dat.gh))
    dat.longer <- dat.longer[!is.na(dat.longer$ratio), ]
    return(dat.longer)
}


process.numerator <- function(inpath.nmr1, inpath.nmr2, inpath.user, inpath.caps){
    nmr1 <- as.data.frame(readRDS(inpath.nmr1))
    nmr2 <- as.data.frame(readRDS(inpath.nmr2))

    user <- read.dat(inpath.user)
    user <- user[, c('USER_ID', 'POSTAL_CODE')]
    user <- user[which(!is.na(user$POSTAL_CODE) & user$POSTAL_CODE != 'na'), ]
    user$zip <- EconTools::fix.zip.codes(user$POSTAL_CODE)

    keep.vars <- c('platform', 'BASKET_SUB_TOTAL', 'BASKET_TOTAL', 'USER_ID', 'month')
    nmr <- dplyr::bind_rows(nmr1[, keep.vars], nmr2[, keep.vars])
    nmr <- dplyr::inner_join(nmr, user, by = 'USER_ID')
    nmr$month <- sprintf('%s-01', nmr$month)

    cap.df <- read.dat(inpath.caps)
    cap.df$zip <- fix.zip.codes(cap.df$zip)
    nmr <- dplyr::inner_join(nmr, cap.df, by = c('zip', 'month'))
    nmr$has_cap <- 1*(nmr$cap < 0.3)
    return(nmr)
}


main()
