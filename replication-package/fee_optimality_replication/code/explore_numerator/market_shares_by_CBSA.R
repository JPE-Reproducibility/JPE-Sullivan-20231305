# Compute Numerator market shares by CBSA for Q1-2021 and Q2-2021
# (inputs to plot_market_shares.R and evaluate_numerator.R).
library(data.table)
library(dplyr)
library(doBy)
library(EconTools)

main <- function(){
    inpath.geo       <- 'data/geo/geo.csv'
    inpath.panel     <- 'data/numerator/static_panel-combined.rds'
    inpath.static    <- 'data/numerator/static_users-combined.rds'
    inpath.nonstatic <- 'data/numerator/nonstatic_users-combined.rds'
    inpath.weeks     <- 'data/numerator/weeks_df-combined.rds'
    inpath.ppl       <- 'data/numerator/people_table_combined.csv'

    outdir <- 'output/explore_numerator'

    # Load data
    geo <- read.dat(inpath.geo, colClasses = c('zip' = 'character'))
    dat <- data.table(readRDS(inpath.panel))
    dat$n_baskets_other <- NULL
    dat$n_baskets_other_delivery <- NULL
    dat$basket_subtotal_other <- NULL
    dat$basket_subtotal_other_delivery <- NULL
    dat$basket_total_other <- NULL
    dat$basket_total_other_delivery <- NULL

    static    <- readRDS(inpath.static)
    nonstatic <- readRDS(inpath.nonstatic)
    weeks.df  <- readRDS(inpath.weeks)

    ppl <- fread(inpath.ppl, stringsAsFactors = FALSE)
    users <- unique(c(static$USER_ID, nonstatic$USER_ID))
    ppl <- ppl[ppl$USER_ID %in% users, ]
    ppl$POSTAL_CODE <- fix.zip.codes(ppl$POSTAL_CODE)
    colnames(ppl)[colnames(ppl) == 'POSTAL_CODE'] <- 'zip'

    geo <- geo[, c('county', 'city', 'zip', 'CBSA_name', 'state')]
    ppl <- left_join(ppl, geo, by = 'zip')
    ppl.vars <- c('USER_ID', 'zip', 'city', 'county', 'CBSA_name', 'state')
    dat <- inner_join(dat, ppl[, ..ppl.vars], by = 'USER_ID')

    # Time manipulations
    weeks.df <- rename.var(weeks.df, 'period.id', 'week.id')
    weeks.df <- rename.var(weeks.df, 'period',    'week')
    week.level <- weeks.df[, c('week.id', 'week')]
    week.level <- week.level[!duplicated(week.level), ]
    setnames(dat, 'period.id', 'week.id')
    dat <- left_join(x = dat, y = week.level, by = 'week.id')

    dat <- dat[dat$week >= as.Date('2020-06-01'), ]
    dat$month <- sub('-[0-9]+$', '', as.character(dat$week))

    quarters <- list('Q1-2021' = c('2021-01', '2021-02', '2021-03'),
                     'Q2-2021' = c('2021-04', '2021-05', '2021-06'))
    for (qr in names(quarters)){
        outpath <- sprintf('%s/shrs_%s.csv', outdir, qr)
        produce.market.share.table(dat, quarters[[qr]], outpath)
    }
}

produce.market.share.table <- function(dat, months, outpath){
    idx <- dat$month %in% months
    dat.sub <- dat[idx, ]

    keep.cols <- c('USER_ID', 'week.id', 'basket_subtotal_uber',
                   'basket_subtotal_dd', 'basket_subtotal_gh',
                   'basket_subtotal_pm', 'CBSA_name')
    keep.cbsa <- c("New York-Newark-Jersey City, NY-NJ-PA",
                   "Los Angeles-Long Beach-Anaheim, CA",
                   "Chicago-Naperville-Elgin, IL-IN-WI",
                   "Dallas-Fort Worth-Arlington, TX",
                   "Atlanta-Sandy Springs-Roswell, GA",
                   "Philadelphia-Camden-Wilmington, PA-NJ-DE-MD",
                   "Miami-Fort Lauderdale-West Palm Beach, FL",
                   "Washington-Arlington-Alexandria, DC-VA-MD-WV",
                   "Riverside-San Bernardino-Ontario, CA",
                   "Phoenix-Mesa-Scottsdale, AZ",
                   "Detroit-Warren-Dearborn, MI",
                   "Boston-Cambridge-Newton, MA-NH",
                   "San Francisco-Oakland-Hayward, CA",
                   "Seattle-Tacoma-Bellevue, WA")
    dat.sub <- dat.sub[, ..keep.cols]
    dat.sub <- dat.sub[which(dat.sub$CBSA_name %in% keep.cbsa), ]
    shrs <- summaryBy(basket_subtotal_uber + basket_subtotal_dd + basket_subtotal_gh + basket_subtotal_pm ~ CBSA_name,
                      data = dat.sub, FUN = mean, keep.names = TRUE)
    shrs <- as.data.frame(shrs)
    basket.vars <- grep('basket', colnames(shrs), value = TRUE)
    shrs[, basket.vars] <- shrs[, basket.vars]/rowSums(shrs[, basket.vars])

    # Round for printed table
    shrs.rounded <- shrs
    for (bv in basket.vars){
        shrs.rounded[, bv] <- sprintf('%0.2f', shrs.rounded[, bv])
    }

    write.table(x = shrs.rounded, file = outpath, quote = FALSE,
                sep = ';', row.names = FALSE)
}

main()

