# Compute the demographics of regions nearby each ZCTA.
#
# This script computes, for each market and each ZCTA z in that market,
# transaction-weighted shares of (young, married, low_income) panelists
# whose transactions occur in ZCTAs within 5 miles of z (the "nearby"
# range). It outputs data/eqm_data/nearby_demo_data.rds, consumed by
# FoodDeliveryTools::CCP_estimation and estimate_restaurant_FCs (Table 4
# in the paper, plus all CF exhibits via the FC parameters and CCPs).
#
# Inputs (all primary; no dependence on eqm_data):
#   data/est_dat/combined_sample_ig2021_v3.csv  -- consumer-panel
#       transactions with demographic columns. We use this file rather
#       than the current pipeline's combined_sample_v3_did_sset.csv
#       because the published nearby_demo_data.rds was built from a
#       sample of the same structure as combined_sample_ig2021_v3.csv
#       (an older, non-subsetted naming convention). Switching to the
#       current did_sset variant would shift the panel composition
#       materially (subsetting filter, DiD pricing pipeline) and change
#       downstream CF / FC results. ig2021_v3 reproduces the per-market
#       zip set and the share variables to within rounding; ntrans is
#       slightly higher because the Numerator panel was refreshed since
#       the original build, but norders/npanelists match exactly for the
#       majority of zips.
#   data/geo/zips_to_markets.csv  -- zip -> CBSA assignment (already
#       ZCTA-level). Used to define the per-market zip universe;
#       includes cross-state ZCTAs within a multi-state CBSA (NY-NJ-PA,
#       PA-NJ-DE-MD, IL-IN-WI), which a within-state zip map would miss.
#   data/est_dat/cbsa_ids-yipitdata_partnered_did_sset_april.csv  -- the
#       canonical CBSA_name <-> m (numeric market code) mapping for the
#       current pipeline.
#
# Per-market zip.map: built on the fly from zips_to_markets.csv via
# geosphere::distm with a 5-mile radius. This matches the cross-state
# zip.map that eqm_data carried per market for multi-state CBSAs and
# avoids the within-state restriction of data/geo/zcta_map.rds.

library(EconTools)
library(doBy)
library(dplyr)
library(geosphere)

# Inputs
inpath.sample   <- 'data/est_dat/combined_sample_ig2021_v3.csv'
inpath.z2m      <- 'data/geo/zips_to_markets.csv'
inpath.cbsa.ids <- 'data/est_dat/cbsa_ids-yipitdata_partnered_did_sset_april.csv'
outpath         <- 'data/eqm_data/nearby_demo_data.rds'

# Settings
kilos.per.mile <- 1.60934
max.miles      <- 5
max.kilos      <- max.miles * kilos.per.mile

# Load auxiliary data
cbsa.map   <- read.dat(inpath.cbsa.ids, sep = '|')
z2m        <- read.dat(inpath.z2m, colClasses = c('zip' = 'character'))
cbsa.zctas <- split(z2m, z2m$CBSA_name)

build.cbsa.zip.map <- function(cbsa.name){
    g <- cbsa.zctas[[cbsa.name]]
    if (is.null(g) || nrow(g) == 0) return(list())
    coords <- cbind(g$lon, g$lat)
    d      <- distm(coords) / 1000
    close  <- d <= max.kilos
    rownames(close) <- g$zip
    setNames(lapply(seq_len(nrow(close)), function(k) g$zip[which(close[k, ])]),
             g$zip)
}

# Load consumer-panel transactions; pad leading zeros so 4-digit zips
# (Boston 0xxxx, Newark 07xxx, Phila 08xxx) match the ZCTA codes in
# zips_to_markets.csv.
est.sample      <- read.dat(inpath.sample, colClasses = c('zip' = 'character'))
est.sample$zip  <- fix.zip.codes(est.sample$zip)
keep            <- c('USER_ID', 'zip', 'm', 'young', 'married', 'high_income')
est.sample      <- est.sample[, intersect(keep, colnames(est.sample))]

# Per-zip transaction and panelist counts (across all months/markets)
n.orders.tab <- as.data.frame(doBy::summaryBy(USER_ID ~ zip, data = est.sample, FUN = length))
colnames(n.orders.tab) <- c('zip', 'n_orders')
est.panelists <- est.sample[, c('USER_ID', 'zip')]
est.panelists <- est.panelists[!duplicated(est.panelists), ]
n.pan.tab <- as.data.frame(doBy::summaryBy(USER_ID ~ zip, data = est.panelists, FUN = length))
colnames(n.pan.tab) <- c('zip', 'n_panelists')
n.orders.tab <- dplyr::inner_join(n.orders.tab, n.pan.tab, by = 'zip')

# Compute per-market, per-zip demographic aggregates
demo.df <- list()
for (k in seq_len(nrow(cbsa.map))){
    market   <- cbsa.map$CBSA_name[k]
    market.m <- cbsa.map$m[k]
    print(market)

    buy.m <- est.sample[est.sample$m == market.m, , drop = FALSE]
    if (nrow(buy.m) == 0) next
    zip.map <- build.cbsa.zip.map(market)
    zips.m  <- names(zip.map)

    share_y    <- c()
    share_m    <- c()
    share_ym   <- c()
    share_low  <- c()
    ntrans     <- c()
    norders    <- c()
    npanelists <- c()
    for (z in zips.m){
        Z.z   <- zip.map[[z]]
        buy.z <- buy.m[buy.m$zip %in% Z.z, , drop = FALSE]
        if (nrow(buy.z) == 0) next
        n.orders.z <- n.orders.tab[n.orders.tab$zip %in% Z.z, , drop = FALSE]

        ntrans[z]     <- nrow(buy.z)
        norders[z]    <- sum(n.orders.z$n_orders)
        npanelists[z] <- sum(n.orders.z$n_panelists)
        low           <- 1 - buy.z$high_income
        share_y[z]    <- mean(buy.z$young == 1 & buy.z$married == 0)
        share_m[z]    <- mean(buy.z$young == 0 & buy.z$married == 1)
        share_ym[z]   <- mean(buy.z$young == 1 & buy.z$married == 1)
        share_low[z]  <- mean(low)
    }
    demo.df.m <- data.frame(zip                  = names(share_y),
                            share_young_range    = share_y,
                            share_married_range  = share_m,
                            share_ym_range       = share_ym,
                            share_low            = share_low,
                            ntrans               = ntrans,
                            norders              = norders,
                            npanelists           = npanelists)
    demo.df.m$share_all_married_range <- demo.df.m$share_married_range + demo.df.m$share_ym_range
    demo.df.m$share_all_young_range   <- demo.df.m$share_young_range   + demo.df.m$share_ym_range

    demo.df[[market]] <- demo.df.m
}

saveRDS(demo.df, file = outpath)
