# generate_synthetic_data.R
#
# Build synthetic versions of every proprietary raw data file the pipeline
# expects, writing into the `data/` tree alongside the public-access files.
#
# Sources:
#   - SimpleMaps uszips.csv is replaced deterministically with a join of
#     noncensus::zip_codes + ACS S0101 population. No synthesis.
#   - InfoGroup, YipitData listings, Edison consumer_panel, and the seven
#     Numerator tables are synthesised. Continuous variables drawn from
#     LogNormals matching support; counts from Poissons; categoricals
#     uniform over their supports. Random draws independent except where
#     a structural anchor is required (see Anchors section).
#
# Structural anchors:
#   - ZIP universe: every ZIP in noncensus::zip_codes (~43k).
#   - 14-metro ZIP subset: ZIPs whose county is in one of the 14 CBSAs
#     listed in data/small_data/cbsa_codenames.csv. Used to scope the panel,
#     listings, and Numerator tables.
#   - Restaurant counts (InfoGroup): >=10 chain + >=10 indep per ZIP, with
#     n_chain = 10 + Poisson(4) and n_indep = 10 + Poisson(4).
#   - Panelist universe: Stage A emits one panelist per 14-metro ZIP with
#     that ZIP as POSTAL_CODE (guarantees coverage); Stage B adds ~2k more
#     drawn uniformly. Every panelist gets at least one transaction.
#   - Restaurant portfolios (YipitData listings): the listings universe is
#     the InfoGroup-2021 metro universe; every restaurant draws its
#     partnered-platform set uniformly over all 16 subsets of the 4
#     platforms, and its listings reuse its exact InfoGroup name and
#     coordinates so the matcher in
#     process_yipit/add_store_locations_w_infogroup.R reassembles them into
#     one location. All 16 portfolio cells of Figure 2 (incl. 'None') are
#     therefore approximately equal at ~1/16 of restaurants each.
#   - Portfolio anchors (YipitData listings): on top of the uniform draws,
#     one deterministic restaurant per (CBSA x non-empty portfolio)
#     guarantees every CBSA exhibits all 16 platform portfolios, so
#     describe_portfolio_choice.R's 16-cell table is always fully populated.
#
# Run from the repository root:
#   Rscript code/generate_synthetic_data.R

library(noncensus)
library(data.table)

set.seed(1)

# ---------------------------------------------------------------------------
# Output directory tree
# ---------------------------------------------------------------------------
root <- 'data'
dirs <- c(
    'simplemaps_uszips_basicv1.77',
    'infogroup',
    'yipitdata',
    'numerator'
)
for (d in dirs) {
    dir.create(file.path(root, d), recursive = TRUE, showWarnings = FALSE)
}

# ---------------------------------------------------------------------------
# uszips.csv — deterministic from noncensus + ACS S0101
# ---------------------------------------------------------------------------
data(zip_codes)

# ACS S0101: total population by ZCTA.
acs.dir <- 'data/ACS/ACSST5Y2019.S0101_2021-12-27T205424'
acs.file <- list.files(acs.dir, pattern = '_data_', full.names = TRUE)
acs <- fread(acs.file, skip = 1, select = c('Geographic Area Name',
                                            'Estimate!!Total!!Total population'))
setnames(acs, c('NAME', 'pop'))
acs[, zip := sub('^ZCTA5 ', '', NAME)]
acs[, NAME := NULL]
acs[, pop := suppressWarnings(as.numeric(pop))]

# Build the uszips substitute
nz <- as.data.table(zip_codes)
nz[, zip := sprintf('%05s', as.character(zip))]
acs[, zip := sprintf('%05s', zip)]

uszips <- merge(nz, acs, by = 'zip', all.x = TRUE)
state.name.lookup <- data.table(state_id = state.abb, state_name = state.name)
uszips <- merge(uszips, state.name.lookup,
                by.x = 'state', by.y = 'state_id', all.x = TRUE)
setnames(uszips, c('state', 'latitude', 'longitude'),
                 c('state_id', 'lat', 'lng'))
uszips[, `:=`(
    zcta        = !is.na(pop),
    population  = ifelse(is.na(pop), 0, pop),
    density     = 0,
    county_name = '',
    county_fips = 0,
    imprecise   = FALSE,
    military    = FALSE
)]
uszips[, `:=`(pop = NULL, fips = NULL)]
setcolorder(uszips, c('zip', 'lat', 'lng', 'city', 'state_id', 'state_name',
                      'zcta', 'population', 'density', 'county_name',
                      'county_fips', 'imprecise', 'military'))

fwrite(uszips,
       file.path(root, 'simplemaps_uszips_basicv1.77', 'uszips.csv'))

# ---------------------------------------------------------------------------
# Anchor universes
# ---------------------------------------------------------------------------
all.zips <- uszips$zip  # ~43k

# 14-metro ZIP subset via the same noncensus merge generate_geo.R uses
data(corebased_areas)
data(counties)
cbsa.codenames <- fread('data/small_data/cbsa_codenames.csv')
counties <- as.data.table(counties)
counties[, fips := paste0(state_fips, county_fips)]
corebased_areas <- as.data.table(corebased_areas)
setnames(corebased_areas, 'name', 'CBSA_name')
corebased_areas[, CBSA := as.character(CBSA)]
counties[, CBSA := as.character(CBSA)]
counties <- merge(counties, corebased_areas[, .(CBSA, CBSA_name)],
                  by = 'CBSA', all.x = TRUE)
metro.fips <- counties[CBSA_name %in% cbsa.codenames$CBSA_name, fips]

nz[, fips := sprintf('%05s', as.character(fips))]
# Build a (zip, CBSA_name) table, then subsample down to a per-CBSA cap.
# The per-CBSA cap keeps stage 1 tractable (the O(n^2) distance/string
# matrices in process_yipit/add_store_locations_w_infogroup.R are the
# main cost in stage 1, and they run per-CBSA-per-month).
zip.to.cbsa <- merge(nz[, .(zip, fips)],
                     counties[CBSA_name %in% cbsa.codenames$CBSA_name,
                              .(fips, CBSA_name)],
                     by = 'fips')
# Filter to ZCTAs only -- combine_data.R replaces non-ZCTA zips with
# their zcta and drops rows where zcta is NA, so PO-Box-only zips would
# drop every transaction we anchor in them. acs has the ZCTA universe;
# use zip %in% acs$zip[!is.na(acs$pop)] as the is.zcta proxy.
zcta.zips <- acs[!is.na(pop), zip]
zip.to.cbsa <- zip.to.cbsa[zip %in% zcta.zips]
zips.per.cbsa <- 50L
set.seed(101)
metro.zips.dt <- zip.to.cbsa[, .SD[sample(.N, min(.N, zips.per.cbsa))],
                             by = CBSA_name]
metro.zips <- intersect(metro.zips.dt$zip, all.zips)
cat(sprintf('All ZIPs: %d   14-metro ZIPs (sampled, cap %d/CBSA): %d\n',
            length(all.zips), zips.per.cbsa, length(metro.zips)))

# Time grid
time.grid <- seq(as.Date('2019-01-01'), as.Date('2021-12-01'), by = 'month')
time.str  <- format(time.grid, '%Y-%m-01')

# Banner universe
platforms <- c('DoorDash', 'UberEats', 'Grubhub', 'Postmates')
n.banners <- 80
banners <- data.table(
    banner_id      = sprintf('banner_%03d', 1:n.banners),
    banner_name    = paste0('Chain_', sprintf('%03d', 1:n.banners)),
    primary_platform = sample(platforms[1:3], n.banners, replace = TRUE)
)

# Item universe.
# SECTOR_ID: process_numerator/combine_item_table.R drops rows where
# grepl('grocery', SECTOR_ID); use 'restaurant' here so all rows are kept.
# BRAND: prepare_item_data.R filters on BRAND not in c('unknown', 'N/A').
# Use a synthetic brand identifier so the downstream filter keeps most rows.
n.items <- 500
items <- data.table(
    ITEM_ID                = sprintf('item_%06d', 1:n.items),
    LOWEST_CATEGORY_ID     = sample(sprintf('cat_%02d', 1:50), n.items, replace = TRUE),
    SECTOR_ID              = 'restaurant',
    BRAND                  = sample(sprintf('brand_%02d', 1:20),
                                    n.items, replace = TRUE),
    item_description       = paste0('Item_', 1:n.items)
)

# Panelist universe (Stage A + Stage B)
# Stage A: one panelist per 14-metro ZIP, guaranteed coverage
n.stage.A <- length(metro.zips)
panelists.A <- data.table(
    USER_ID     = sprintf('user_A_%05d', seq_len(n.stage.A)),
    POSTAL_CODE = metro.zips
)
# Stage B: 2k more panelists drawn uniformly from the 14-metro ZIPs
n.stage.B <- 2000
panelists.B <- data.table(
    USER_ID     = sprintf('user_B_%05d', seq_len(n.stage.B)),
    POSTAL_CODE = sample(metro.zips, n.stage.B, replace = TRUE)
)
panelists <- rbind(panelists.A, panelists.B)

# ---------------------------------------------------------------------------
# Demand-estimation anchors -- guarantee >=1 transaction per
# (CBSA x online platform) cell so the Berry contraction in
# code/demand_estimation/estimate_consumer_choice.m does not encounter
# log(0) shares. For each of the 14 CBSAs, take 4 Stage A panelists
# (one per online platform) and earmark them: forced into the static panel,
# forced EMAIL_CONNECT=1, and given a deterministic basket with the
# matching DELIVERY_PROVIDER in April-June 2021.
# ---------------------------------------------------------------------------
anchor.platforms <- c('DoorDash', 'uberEats', 'GrubHub', 'Postmates')
anchor.months    <- c('2021-04', '2021-05', '2021-06')
panelists.A.cbsa <- merge(panelists.A,
                          metro.zips.dt[, .(POSTAL_CODE = zip, CBSA_name)],
                          by = 'POSTAL_CODE', all.x = TRUE)
anchor.tbl <- panelists.A.cbsa[, head(.SD, length(anchor.platforms)),
                                by = CBSA_name]
anchor.tbl[, platform.target := rep_len(anchor.platforms, .N), by = CBSA_name]
anchor.tbl[, month.target := rep_len(anchor.months, .N), by = CBSA_name]
anchor.ids <- anchor.tbl$USER_ID

cat(sprintf('Panelists: %d (Stage A: %d, Stage B: %d, anchors: %d)\n',
            nrow(panelists), n.stage.A, n.stage.B, length(anchor.ids)))

# ---------------------------------------------------------------------------
# InfoGroup — chain + indep per ZIP for every US ZIP
# ---------------------------------------------------------------------------
# For each ZIP: n_chain = 10 + Poisson(4), n_indep = 10 + Poisson(4)
generate.infogroup <- function(zips, year, abi.start) {
    n.zip <- length(zips)
    n.chain <- 10L + rpois(n.zip, 4)
    n.indep <- 10L + rpois(n.zip, 4)

    # Chain pool: 80 nationally-recurring parent_numbers. Map each
    # parent_number to a real brand name that brand_map.R's regexes match;
    # process_yipit/add_infogroup.R's brand assignment scans the InfoGroup
    # `company` field, so these names drive whether downstream restaurants
    # get classified as chain or independent.
    chain.pool <- 1:80
    chain.names <- c('MCDONALDS', 'TACO BELL', 'STARBUCKS', 'SUBWAY',
                     'KFC', 'DOMINOS', 'BURGER KING', 'DUNKIN DONUTS',
                     'JACK IN THE BOX', 'PIZZA HUT', 'DENNYS', 'WENDYS',
                     'JIMMY JOHNS', 'POPEYES', 'WHITE CASTLE',
                     'IHOP', 'CHIPOTLE', 'PANERA', 'ARBYS', 'CHICKFILA')

    # Chain rows
    chain.rows <- data.table(
        zipcode       = rep(zips, n.chain),
        is_chain      = TRUE,
        company       = NA_character_,
        parent_number = NA_integer_
    )
    chain.rows[, parent_number := sample(chain.pool, .N, replace = TRUE)]
    # Map parent_number -> brand name (cyclic so all 80 parents get a name)
    chain.rows[, company := chain.names[((parent_number - 1L) %% length(chain.names)) + 1L]]

    # Indep rows: parent_number == abi (singleton)
    indep.rows <- data.table(
        zipcode  = rep(zips, n.indep),
        is_chain = FALSE
    )

    rows <- rbind(chain.rows, indep.rows, fill = TRUE)
    n.rows <- nrow(rows)
    rows[, abi := abi.start + seq_len(n.rows) - 1L]
    rows[is_chain == FALSE, parent_number := abi]
    rows[is_chain == FALSE, company := paste0('Indep_', abi)]

    # Merge in city/state/lat/lng from noncensus
    geo <- nz[, .(zip, city, state, lat = latitude, lng = longitude)]
    rows <- merge(rows, geo, by.x = 'zipcode', by.y = 'zip', all.x = TRUE)

    # Jitter centroid
    rows[, latitude  := as.numeric(lat) + rnorm(.N, 0, 0.005)]
    rows[, longitude := as.numeric(lng) + rnorm(.N, 0, 0.005)]
    rows[, `:=`(lat = NULL, lng = NULL)]

    # Address + SIC + descriptions
    rows[, address_line_1 := sprintf('%d Main St', sample(1:9999, .N, replace = TRUE))]
    rows[, primary_sic_code := sample(c(5812L, 5813L, 5814L), .N, replace = TRUE)]
    sic.lookup <- c(`5812` = 'EATING PLACES',
                    `5813` = 'DRINKING PLACES',
                    `5814` = 'FAST FOOD')
    rows[, sic6_descriptions      := sic.lookup[as.character(primary_sic_code)]]
    rows[, sic6_descriptions_sic1 := sic6_descriptions]
    rows[, sic6_descriptions_sic2 := sic6_descriptions]
    rows[, archive_version_year   := year]

    rows[, is_chain := NULL]
    setcolorder(rows, c('abi', 'parent_number', 'company',
                        'address_line_1', 'city', 'state', 'zipcode',
                        'primary_sic_code', 'sic6_descriptions',
                        'sic6_descriptions_sic1', 'sic6_descriptions_sic2',
                        'latitude', 'longitude', 'archive_version_year'))
    return(rows)
}

# InfoGroup is restricted to the 14-metro ZIPs only — process_yipit/
# add_store_locations_w_infogroup.R builds a per-CBSA distance matrix and
# blows past the 100 GB R vector-memory cap when the "Other" CBSA bucket
# contains the ~140k restaurants in non-metro ZIPs.
ig.zips <- metro.zips

# 2019_2020 file: contains 2019 + 2020 rows (process_infogroup.R splits)
ig.19 <- generate.infogroup(ig.zips, 2019L, abi.start = 1L)
ig.20 <- generate.infogroup(ig.zips, 2020L, abi.start = nrow(ig.19) + 1L)
ig.1920 <- rbind(ig.19, ig.20)
fwrite(ig.1920,
       file.path(root, 'infogroup', 'jjcdvjugq1hsjpmn.csv'))

# 2021 file
ig.21 <- generate.infogroup(ig.zips, 2021L, abi.start = nrow(ig.1920) + 1L)
fwrite(ig.21,
       file.path(root, 'infogroup', 'hiyy2ujx9xeo2wtl.csv'))

cat(sprintf('InfoGroup: 2019-20 file %d rows, 2021 file %d rows\n',
            nrow(ig.1920), nrow(ig.21)))

# ---------------------------------------------------------------------------
# YipitData listings_v2.csv — (restaurant x platform x month) in 14 metros
# ---------------------------------------------------------------------------
# The listings restaurant universe is the InfoGroup-2021 metro universe
# itself: every InfoGroup restaurant draws a platform portfolio uniformly at
# random over all 16 subsets of the four platforms, so the 16 portfolio
# cells in describe_portfolio_choice.R come out approximately equal (~1/16
# of restaurants each, including 'None' -- portfolio 0 yields no partnered
# listings). Listings rows reuse the restaurant's exact InfoGroup name and
# coordinates, so the location matcher in
# process_yipit/add_store_locations_w_infogroup.R reassembles the platform
# rows and the InfoGroup offline row into a single location carrying the
# drawn portfolio. (The 2019/2020 InfoGroup vintages are disjoint abi
# universes, so in 2020-month files the listed locations coexist with that
# year's offline InfoGroup locations instead of merging; Figure 2 uses the
# April-2021 snapshot, where the merge is exact.)
geo.ll <- nz[, .(zip, lat = latitude, lng = longitude)]   # anchors use this below
yipit.platforms <- c('DoorDash', 'UberEats', 'Grubhub', 'Postmates')
restos <- data.table(
    restaurant_id = sprintf('resto_%06d', ig.21$abi),
    merchant_name = ig.21$company,
    postal_code   = ig.21$zipcode,
    latitude      = ig.21$latitude,
    longitude     = ig.21$longitude
)
restos[, portfolio := sample.int(16L, .N, replace = TRUE) - 1L]

# (restaurant x platform) grid: partnered exactly on the drawn portfolio's
# platforms. A 25% random subset of the remaining (restaurant, platform)
# pairs is listed-but-not-partnered so downstream
# is_partnered_merchant == FALSE branches stay exercised; FALSE rows attach
# to the restaurant's location without affecting its portfolio
# (partnered-only subsets drop them).
# process_yipit/separate_data.R writes is_partnered_merchant through to a
# downstream subset (`plat[plat$is_partnered_merchant, ]`), so the column
# must be logical, not integer.
plat.bit <- setNames(bitwShiftL(1L, 0:3), yipit.platforms)
rp <- CJ(restaurant_id = restos$restaurant_id, platform = yipit.platforms)
rp <- merge(rp, restos, by = 'restaurant_id', all.x = TRUE)
rp[, is_partnered_merchant := bitwAnd(portfolio, plat.bit[platform]) > 0L]
rp <- rp[is_partnered_merchant | (runif(.N) < 0.25)]
rp[, portfolio := NULL]

# Cross with months
n.rp <- nrow(rp)
listings <- rp[rep(seq_len(n.rp), each = length(time.str))]
listings[, observation_month := rep(time.str, times = n.rp)]

listings[, name := merchant_name]  # process_yipit/add_brands.R reads `name`
# restaurant_url: parsed by web_harvest_descriptives/add_*_resto_info.R.
# Format mirrors each platform's real URL so determine.code.1 / determine.code.2
# return a non-'missing' code. We intentionally do NOT add price_range,
# delivery_fee, busy_flag, etc., here -- those exist on the per-platform
# web-harvested data (data/web_harvesting/<platform>/...), and adding them to the
# YipitData listings would create _x/_y suffixes after the merge in
# add_<platform>_resto_info.R and break data_processing.R's
# canonical-name references.
listings[, restaurant_url := fcase(
    platform == 'DoorDash',  sprintf('https://www.doordash.com/store/%s',     restaurant_id),
    platform == 'UberEats',  sprintf('https://www.ubereats.com/store/%s/%s',  restaurant_id, restaurant_id),
    platform == 'Grubhub',   sprintf('https://www.grubhub.com/restaurant/%s', restaurant_id),
    platform == 'Postmates', sprintf('https://postmates.com/merchant/%s',     restaurant_id),
    default = sprintf('https://example.com/store/%s', restaurant_id))]
# `chain` and `cuisine` are YipitData fields needed by
# FoodDeliveryTools::add.restaurant.characteristics() (called from
# data_processing.R). chain is the brand/chain identifier (or empty for
# independents); cuisine is a comma-separated list of cuisine tags.
listings[, chain := fifelse(grepl('^chain_', merchant_name, ignore.case = TRUE),
                            tolower(merchant_name), '')]
listings[, cuisine := sample(c('American', 'Asian, Chinese', 'Pizza',
                                'Mexican', 'Burgers', 'Sushi, Asian',
                                'Sandwiches', 'Italian'),
                             .N, replace = TRUE)]
# ---------------------------------------------------------------------------
# Portfolio anchors -- guarantee that every CBSA has at least one restaurant
# on each of the 15 non-empty platform portfolios (subsets of the four
# platforms) in every month. The 16th portfolio (no platforms) needs no
# anchor: ~1/16 of each CBSA's InfoGroup restaurants draw portfolio 0 and
# remain offline-only.
#
# The uniform portfolio draws above make missing cells unlikely but not
# impossible (~1% chance some CBSA misses some portfolio); a missing cell
# leaves an NA in describe_portfolio_choice.R's hard-coded 16-cell table,
# which NAs out every share and produces an empty Figure 2. The anchors
# make coverage deterministic.
#
# Construction:
#   - One anchor per (CBSA x non-empty portfolio), in the CBSA's first
#     sampled ZIP.
#   - All rows of an anchor share one exact (lat, lng) and name, so the
#     matcher's distance (0 m < 100 m) and string-distance (0 < 5) criteria
#     group them into one loc.id.
#   - Distinct anchors (whose names are within string distance 5 of each
#     other) sit on a 4x4 grid with ~0.0035 deg spacing (>= ~260 m apart),
#     so they never satisfy the 100 m criterion against each other. Regular
#     restaurants are never compared with anchors at all: matching pools
#     restaurants by the first two letters of the name, and 'po' contains
#     only the anchors (InfoGroup's POPEYES rows are brand-matched and so
#     leave the independents pool).
#   - is_partnered_merchant = TRUE so partnered-only subsets keep them.
#   - 'Portfolio Anchor' matches no brand.map regex, so anchors stay
#     independents.
#   - Deterministic (no RNG), so the draws behind every other synthetic
#     table are unchanged by this block.
anchor.zips.tbl <- metro.zips.dt[zip %in% metro.zips, .(zip = zip[1]),
                                 by = CBSA_name]
n.anchor.cbsa <- nrow(anchor.zips.tbl)
combo.mat <- as.matrix(expand.grid(rep(list(0:1), length(yipit.platforms))))
colnames(combo.mat) <- yipit.platforms
combo.mat <- combo.mat[rowSums(combo.mat) > 0, , drop = FALSE]
n.combos <- nrow(combo.mat)

anchors <- CJ(cbsa.idx = seq_len(n.anchor.cbsa), combo = seq_len(n.combos))
anchors[, zip := anchor.zips.tbl$zip[cbsa.idx]]
anchors[, restaurant_id := sprintf('anchor_%02d_%02d', cbsa.idx, combo)]
anchors[, merchant_name := sprintf('Portfolio Anchor %02d %02d', cbsa.idx, combo)]
anchors <- merge(anchors, geo.ll, by = 'zip', all.x = TRUE)
anchors[, latitude  := as.numeric(lat) + 0.010 + 0.0035 * ((combo - 1L) %%  4L)]
anchors[, longitude := as.numeric(lng) + 0.010 + 0.0035 * ((combo - 1L) %/% 4L)]
anchors[, `:=`(lat = NULL, lng = NULL)]

# Expand each anchor to its portfolio's platform rows, then to all months
anchor.listings <- anchors[, .(platform = yipit.platforms[combo.mat[combo, ] == 1L]),
                           by = .(restaurant_id, merchant_name, zip,
                                  latitude, longitude)]
n.anchor.rows <- nrow(anchor.listings)
anchor.listings <- anchor.listings[rep(seq_len(n.anchor.rows),
                                       each = length(time.str))]
anchor.listings[, observation_month := rep(time.str, times = n.anchor.rows)]

anchor.listings[, `:=`(
    postal_code           = zip,
    name                  = merchant_name,
    is_partnered_merchant = TRUE,
    chain                 = '',
    cuisine               = 'American'
)]
anchor.listings[, restaurant_url := fcase(
    platform == 'DoorDash',  sprintf('https://www.doordash.com/store/%s',     restaurant_id),
    platform == 'UberEats',  sprintf('https://www.ubereats.com/store/%s/%s',  restaurant_id, restaurant_id),
    platform == 'Grubhub',   sprintf('https://www.grubhub.com/restaurant/%s', restaurant_id),
    platform == 'Postmates', sprintf('https://postmates.com/merchant/%s',     restaurant_id),
    default = sprintf('https://example.com/store/%s', restaurant_id))]
anchor.listings[, zip := NULL]

listings <- rbind(listings, anchor.listings[, names(listings), with = FALSE])

cat(sprintf('Portfolio anchors: %d restaurants (%d CBSAs x %d portfolios), %d listing rows\n',
            nrow(anchors), n.anchor.cbsa, n.combos, nrow(anchor.listings)))

setcolorder(listings, c('restaurant_id', 'merchant_name', 'name', 'platform',
                        'observation_month', 'postal_code',
                        'latitude', 'longitude', 'is_partnered_merchant',
                        'restaurant_url', 'chain', 'cuisine'))

fwrite(listings, file.path(root, 'yipitdata', 'listings_v2.csv'))

# Per-month RDS files expected by process_yipit/add_brands.R
month.abbr <- c('jan','feb','mar','apr','may','jun',
                'jul','aug','sep','oct','nov','dec')
month.window <- list(
    `2020` = month.abbr,           # full year
    `2021` = month.abbr[1:5]       # jan-may, per platform.adoption.times()
)
listings[, ym := substr(as.character(observation_month), 1, 7)]
n.month.rds <- 0L
for (y in names(month.window)) {
    for (m.idx in seq_along(month.window[[y]])) {
        m.lbl <- month.window[[y]][m.idx]
        ym.k  <- sprintf('%s-%02d', y, m.idx)
        chunk <- listings[ym == ym.k]
        out   <- file.path(root, 'yipitdata',
                           sprintf('listings_%s%s.rds', m.lbl, y))
        saveRDS(as.data.frame(chunk), out)
        n.month.rds <- n.month.rds + 1L
    }
}
listings[, ym := NULL]

cat(sprintf('YipitData listings: %d rows + %d per-month RDS files\n',
            nrow(listings), n.month.rds))

# ---------------------------------------------------------------------------
# Edison consumer_panel.csv — one row per (ZIP x month x merchant)
# ---------------------------------------------------------------------------
# Edison labels match the strings filtered for in explore_yipit/
# demand_estimation_DiD.R: 'DoorDash', 'Uber', 'Grub Hub'.
edison.platforms <- c('DoorDash', 'Uber', 'Grub Hub', 'Postmates')
panel <- CJ(zip            = metro.zips,
            month          = time.str,
            merchant_name  = edison.platforms)

panel[, aov_feesandtips_excluded := exp(rnorm(.N, mean = 3,   sd = 0.30))]
panel[, avg_service_fee          := exp(rnorm(.N, mean = 1,   sd = 0.40))]
panel[, avg_delivery_fee         := exp(rnorm(.N, mean = 1,   sd = 0.40))]
panel[, avg_order_discount       := exp(rnorm(.N, mean = 0.5, sd = 0.50))]
panel[, avg_order_tip            := exp(rnorm(.N, mean = 1,   sd = 0.50))]
panel[, avg_order_tax            := 0.08 * aov_feesandtips_excluded]
panel[, aov_feesandtips_included :=
        aov_feesandtips_excluded + avg_service_fee + avg_delivery_fee +
        avg_order_tax + avg_order_tip - avg_order_discount]
panel[, orders_scaled := exp(rnorm(.N, mean = 4, sd = 1))]
panel[, observed_orders_for_orders_scaled_calculation := 0.1 * orders_scaled]
# Edison's panel has both ..._for_orders_scaled_calculation and
# ..._used_in_averagecalculations; explore_yipit/prepare_DiD_data.R references
# both column names.
panel[, observed_orders_used_in_averagecalculations :=
        observed_orders_for_orders_scaled_calculation]

# --- Inject cap-induced DiD variation -----------------------------------
# When (zip, month) is treated by a commission cap, scale the fee components
# down by x.star and orders_scaled up by y.star. The DiD in prepare_DiD_data.R
# should then recover an effect of cap on total_fee around -x.star and an
# effect on log(orders_scaled) around +y.star, giving an implied
# semi-elasticity of -y.star / x.star.
x.star <- 0.08    # 8% drop in total cost when capped
y.star <- 0.10    # 10% bump in sales when capped

caps <- fread('data/small_data/commission_caps.csv', encoding = 'UTF-8')
caps[, start_date := as.Date(start_date, format = '%Y-%m-%d')]
end1 <- suppressWarnings(as.Date(caps$end_date, format = '%Y-%m-%d'))
end2 <- suppressWarnings(as.Date(caps$end_date, format = '%d/%m/%Y'))
caps[, end_date := fifelse(!is.na(end1), end1,
                  fifelse(!is.na(end2), end2,
                          as.Date('2099-12-31')))]
caps <- caps[!is.na(start_date) & city != '' & !is.na(city)]

# Map (city, state) → ZIPs via uszips (real city/state from noncensus)
zip.city <- uszips[, .(zip, city, state = state_id)]
treated.spans <- merge(zip.city, caps[, .(city, state, start_date, end_date)],
                       by = c('city', 'state'), allow.cartesian = TRUE)

# Build the (zip, month) treated table by expanding spans to months
panel.months <- data.table(month = time.str, month_date = as.Date(time.str))
treated.zm <- treated.spans[, {
    panel.months[month_date >= start_date & month_date <= end_date, .(month)]
}, by = .(zip)]
treated.zm <- unique(treated.zm[, .(zip, month)])
treated.zm[, treated := TRUE]

panel <- merge(panel, treated.zm, by = c('zip', 'month'), all.x = TRUE)
panel[is.na(treated), treated := FALSE]

# Apply the shock: scale all three components that enter `total_fee`
# (avg_service_fee, avg_delivery_fee, avg_order_discount) by (1 - x.star),
# so that total_fee = service + delivery - discount drops by exactly x.star
# in proportion. Orders bump up by y.star.
panel[treated == TRUE, avg_service_fee    := avg_service_fee    * (1 - x.star)]
panel[treated == TRUE, avg_delivery_fee   := avg_delivery_fee   * (1 - x.star)]
panel[treated == TRUE, avg_order_discount := avg_order_discount * (1 - x.star)]
panel[treated == TRUE, aov_feesandtips_included :=
        aov_feesandtips_excluded + avg_service_fee + avg_delivery_fee +
        avg_order_tax + avg_order_tip - avg_order_discount]
panel[treated == TRUE, orders_scaled := orders_scaled * (1 + y.star)]
panel[treated == TRUE, observed_orders_for_orders_scaled_calculation :=
        0.1 * orders_scaled]

cat(sprintf('Consumer panel: cap-treated rows %d / %d (%.2f%%); x*=%.0f%%, y*=%.0f%%\n',
            sum(panel$treated), nrow(panel),
            100*sum(panel$treated)/nrow(panel),
            100*x.star, 100*y.star))

panel[, treated := NULL]

setcolorder(panel, c('zip', 'month', 'merchant_name',
                     'orders_scaled', 'observed_orders_for_orders_scaled_calculation',
                     'avg_service_fee', 'avg_delivery_fee', 'avg_order_discount',
                     'avg_order_tax', 'avg_order_tip',
                     'aov_feesandtips_included', 'aov_feesandtips_excluded'))

fwrite(panel, file.path(root, 'yipitdata', 'consumer_panel.csv'))

cat(sprintf('Edison consumer_panel: %d rows\n', nrow(panel)))

# ---------------------------------------------------------------------------
# Numerator — people, items, banners
# ---------------------------------------------------------------------------
# 1. people table
people <- copy(panelists)
people[, AGE              := pmin(pmax(round(rnorm(.N, 45, 15)), 18), 90)]
people[, HOUSEHOLD_INCOME := round(exp(rnorm(.N, 11, 0.7)))]
people[, HOUSEHOLD_SIZE   := rpois(.N, 2) + 1L]
people[, MARITAL_STATUS   := sample(c('Married', 'Single', 'Divorced', 'Widowed'),
                                    .N, replace = TRUE)]
people[, EDUCATION        := sample(c('High School', 'Some College',
                                      'College', 'Graduate'),
                                    .N, replace = TRUE)]
people[, GENDER           := sample(c('Male', 'Female'), .N, replace = TRUE)]
people[, RACE             := sample(c('White', 'Black', 'Asian', 'Hispanic', 'Other'),
                                    .N, replace = TRUE)]
people[, HISPANIC_FLAG    := as.integer(RACE == 'Hispanic')]
people[, URBANICITY       := sample(c('Urban', 'Suburban', 'Rural'),
                                    .N, replace = TRUE)]

# Numerator-style derived demographic buckets used by representativeness.R
# (INCOME_BUCKET_LONG) and combine_data.R (AGE_BUCKET, INCOME_BUCKET,
# MARITAL_STATUS).
age.bucket <- function(x) {
    cut(x,
        breaks = c(-Inf, 20, 24, 34, 44, 54, 64, Inf),
        labels = c('18-20', '21-24', '25-34', '35-44',
                   '45-54', '55-64', '65+'),
        right = TRUE)
}
people[, AGE_BUCKET := as.character(age.bucket(AGE))]

inc.long <- function(x) {
    dplyr::case_when(
        x <  20000              ~ 'Under $20k',
        x >= 20000 & x <  40000 ~ '$20k-40k',
        x >= 40000 & x <  60000 ~ '$40k-60k',
        x >= 60000 & x <  80000 ~ '$60k-80k',
        x >= 80000 & x <= 125000 ~ '$80k-125k',
        x >  125000             ~ 'Over $125k'
    )
}
people[, INCOME_BUCKET_LONG := inc.long(HOUSEHOLD_INCOME)]
# combine_data.R only checks INCOME_BUCKET != 'low'; anything else passes.
people[, INCOME_BUCKET := ifelse(HOUSEHOLD_INCOME < 40000, 'low', 'high')]

# Add STATE for the panelist's POSTAL_CODE
people <- merge(people, nz[, .(zip, STATE = state)],
                by.x = 'POSTAL_CODE', by.y = 'zip', all.x = TRUE)

# Zero-pad POSTAL_CODE to 5 chars; insert a handful of POBOX sentinel codes so
# prepare_regression_data.R's fread(ppl) -- with no colClasses hint -- is
# forced to read POSTAL_CODE as character, preserving leading zeros across all
# rows. Downstream filters drop the sentinels naturally.
people[, POSTAL_CODE := formatC(as.integer(POSTAL_CODE), width = 5, flag = '0')]
n.sentinel <- min(5L, nrow(people))
sentinel.idx <- sample.int(nrow(people), n.sentinel)
people[sentinel.idx, POSTAL_CODE := sprintf('POBOX%02d', seq_len(n.sentinel))]

fwrite(people, file.path(root, 'numerator',
                         'standard_nmr_feed_people_table.csv'), sep = '|')
# people_table_combined.csv: comma-separated mirror consumed by scripts that
# load it with read.dat (i.e., read.csv with default sep=',').
fwrite(people, file.path(root, 'numerator', 'people_table_combined.csv'))

# 2. item table
fwrite(items, file.path(root, 'numerator',
                        'standard_nmr_feed_item_table.csv'), sep = '|')

# 3. banner table — match the join keys combine_fact_summary.R uses
banner.tbl <- banners[, .(
    BANNER         = banner_id,
    RETAILER       = paste0('Retailer_', sprintf('%03d', seq_len(.N))),
    CHANNEL        = sample(c('QSR', 'FullService', 'Cafe'), .N, replace = TRUE),
    PARENT_CHANNEL = 'Food'
)]
fwrite(banner.tbl, file.path(root, 'numerator',
                             'standard_nmr_feed_banner_table.csv'), sep = '|')

cat(sprintf('Numerator: people %d rows, items %d rows, banners %d rows\n',
            nrow(people), nrow(items), nrow(banner.tbl)))

# ---------------------------------------------------------------------------
# Numerator — static panel + connect tables (per-panelist date ranges)
# ---------------------------------------------------------------------------
make.date.ranges <- function(panelist.ids) {
    n.panelists <- length(panelist.ids)
    reps <- pmax(1L, rpois(n.panelists, 1))   # 1-3 windows per panelist
    rows <- data.table(USER_ID = rep(panelist.ids, reps))
    rows[, START_DATE := as.Date('2019-01-01') +
            sample(0:730, .N, replace = TRUE)]
    # END_DATE: START + Exp(180 days), capped at 2021-12-31
    span <- pmin(round(rexp(nrow(rows), rate = 1/180)),
                 as.integer(as.Date('2021-12-31') - rows$START_DATE))
    rows[, END_DATE := START_DATE + span]
    return(rows)
}

# Only ~70% of panelists are "static" so that combine_fact_summary.R sees a
# non-empty nonstatic stream; otherwise downstream dplyr joins fail because the
# zero-row csv reads each column as logical.
set.seed(905)
static.ids  <- sample(panelists$USER_ID,
                      size = ceiling(0.7 * nrow(panelists)), replace = FALSE)
connect.ids <- sample(panelists$USER_ID,
                      size = ceiling(0.8 * nrow(panelists)), replace = FALSE)
# Force all anchor panelists into the static panel and the connect panel.
static.ids  <- union(static.ids,  anchor.ids)
connect.ids <- union(connect.ids, anchor.ids)
static.tbl  <- make.date.ranges(static.ids)
connect.tbl <- make.date.ranges(connect.ids)
# Anchors get a static window spanning all of 2021 so their April-June
# 2021 anchor transactions land inside static_trans.
static.tbl[USER_ID %in% anchor.ids, START_DATE := as.Date('2021-01-01')]
static.tbl[USER_ID %in% anchor.ids, END_DATE   := as.Date('2021-12-31')]
connect.tbl[USER_ID %in% anchor.ids, START_DATE := as.Date('2021-01-01')]
connect.tbl[USER_ID %in% anchor.ids, END_DATE   := as.Date('2021-12-31')]
# EMAIL_CONNECT flags email-receipt connection on Numerator's panel; scripts
# in explore_numerator and prepare_est_data filter on this column.
connect.tbl[, EMAIL_CONNECT := sample(0:1, .N, replace = TRUE, prob = c(0.2, 0.8))]
connect.tbl[USER_ID %in% anchor.ids, EMAIL_CONNECT := 1L]
# assess_leftovers.R renames columns 1:2 of static_connect_table to
# (START_CONNECT, END_CONNECT) — so START_DATE, END_DATE must be first.
setcolorder(connect.tbl, c('START_DATE', 'END_DATE', 'USER_ID', 'EMAIL_CONNECT'))
# combined_static_table: comma-separated per collapse_static.R's
# read.csv(sep=',').  static_connect_table: pipe-separated per
# combine_months.R's read.dat(sep='|').
fwrite(static.tbl,  file.path(root, 'numerator', 'combined_static_table.csv'))
fwrite(connect.tbl, file.path(root, 'numerator', 'static_connect_table.csv'),
       sep = '|')

cat(sprintf('Numerator: static %d rows, connect %d rows\n',
            nrow(static.tbl), nrow(connect.tbl)))

# ---------------------------------------------------------------------------
# Numerator — qsr_transactions_master (item-level) + summary_data (basket)
# ---------------------------------------------------------------------------
# Stage A: one guaranteed basket per Stage-A panelist (>=1 transaction per ZIP)
# Stage B: panelist-level: each panelist gets ~Poisson(3) additional baskets

stage.A.users <- panelists.A$USER_ID
n.A <- length(stage.A.users)

# Additional baskets per panelist (Poisson(3))
add.baskets <- rpois(nrow(panelists), 3)
add.baskets.users <- rep(panelists$USER_ID, add.baskets)

basket.users <- c(stage.A.users, add.baskets.users)
n.baskets <- length(basket.users)

baskets <- data.table(
    BASKET_ID = sprintf('basket_%07d', seq_len(n.baskets)),
    USER_ID   = basket.users
)
baskets <- merge(baskets, panelists, by = 'USER_ID', all.x = TRUE)
baskets[, BANNER_ID := sample(banners$banner_id, .N, replace = TRUE)]

# Random transaction date within 2019-2021
baskets[, TRANSACTION_DATE := as.Date('2019-01-01') +
        sample(0:1095, .N, replace = TRUE)]
baskets[, TRANSACTION_DATE := format(TRANSACTION_DATE, '%Y-%m-%d')]

# Order method.
# DELIVERY_PROVIDER uses Numerator's canonical strings ('DoorDash', 'uberEats',
# 'GrubHub', 'Postmates', 'na'). The 'na' string is required so
# numerator_menu_pricing/prepare_menu_data.R retains some offline transactions
# (its subsetting keeps platform %in% c('na','dd','uber','gh')).
baskets[, ORDER_METHOD_TYPE := sample(c('Delivery', 'Pickup', 'DineIn'),
                                      .N, replace = TRUE)]
delivery.opts <- c('DoorDash', 'uberEats', 'GrubHub', 'Postmates', 'na')
baskets[, DELIVERY_PROVIDER := sample(delivery.opts, .N, replace = TRUE,
                                       prob = c(0.10, 0.10, 0.10, 0.05, 0.65))]
baskets[, ORDER_PROVIDER    := sample(delivery.opts, .N, replace = TRUE,
                                       prob = c(0.10, 0.10, 0.10, 0.05, 0.65))]

# Override the first basket of each anchor panelist: set DELIVERY_PROVIDER /
# TRANSACTION_DATE deterministically so the estimation sample has >=1
# (CBSA x online platform) cell populated.
baskets[, .basket.rank := seq_len(.N), by = USER_ID]
baskets <- merge(baskets, anchor.tbl[, .(USER_ID, platform.target, month.target)],
                 by = 'USER_ID', all.x = TRUE)
baskets[!is.na(platform.target) & .basket.rank == 1,
        DELIVERY_PROVIDER := platform.target]
baskets[!is.na(month.target) & .basket.rank == 1,
        TRANSACTION_DATE := sprintf('%s-15', month.target)]
baskets[, `:=`(.basket.rank = NULL,
               platform.target = NULL,
               month.target = NULL)]

# Merge BANNER/RETAILER/CHANNEL/PARENT_CHANNEL from banner table. Downstream
# scripts (process_numerator/{add_category,prepare_regression_data}.R,
# numerator_menu_pricing/*) reference BANNER_ID, while combine_fact_summary.R
# joins on BANNER. The real data carries both columns; we mirror that here.
baskets <- merge(baskets, banner.tbl, by.x = 'BANNER_ID',
                 by.y = 'BANNER', all.x = TRUE)
baskets[, BANNER := BANNER_ID]

# Items per basket: Poisson(2) + 1, so >=1
items.per.basket <- rpois(n.baskets, 2) + 1L

# Expand to item-level rows
qsr <- baskets[rep(seq_len(n.baskets), items.per.basket)]
qsr[, ITEM_ID := sample(items$ITEM_ID, .N, replace = TRUE)]
qsr <- merge(qsr, items[, .(ITEM_ID, LOWEST_CATEGORY_ID)], by = 'ITEM_ID', all.x = TRUE)
qsr[, ITEM_QUANTITY   := rpois(.N, 1) + 1L]
qsr[, ITEM_UNIT_PRICE := round(exp(rnorm(.N, 2, 0.5)), 2)]
qsr[, ITEM_TOTAL      := ITEM_QUANTITY * ITEM_UNIT_PRICE]

# BASKET_SUB_TOTAL / BASKET_TOTAL: summed at basket level, repeated on each
# item row; combine_fact_summary.R relies on these being present in
# qsr_transactions_master so the fact/summary intersection of columns keeps
# them.
qsr[, BASKET_SUB_TOTAL := sum(ITEM_TOTAL), by = BASKET_ID]
# Anchor baskets must clear combine_data.R's lower.lim=5.90 BASKET_SUB_TOTAL
# filter so they reach the connect_sample. Identify anchor baskets (anchor
# user + target-month date) and floor their subtotal at 25.
anchor.dates <- sprintf('%s-15', c('2021-04', '2021-05', '2021-06'))
qsr[USER_ID %in% anchor.ids & TRANSACTION_DATE %in% anchor.dates,
    BASKET_SUB_TOTAL := pmax(BASKET_SUB_TOTAL, 25)]
qsr[, BASKET_TOTAL     := round(BASKET_SUB_TOTAL * 1.085, 2)]  # tax/tip wedge

setnames(qsr, 'POSTAL_CODE', 'POSTAL_CODE')
# Zero-pad qsr POSTAL_CODE and insert POBOX sentinels so fread (without a
# colClasses hint) reads the column as character across the downstream chain
# (compute_price_indices.R left-joins on POSTAL_CODE against a character
# zip from fix.zip.codes()).
qsr[, POSTAL_CODE := formatC(as.integer(POSTAL_CODE), width = 5, flag = '0')]
n.qsr.sentinel <- min(5L, nrow(qsr))
qsr.sentinel.idx <- sample.int(nrow(qsr), n.qsr.sentinel)
qsr[qsr.sentinel.idx, POSTAL_CODE := sprintf('POBOX%02d', seq_len(n.qsr.sentinel))]
setcolorder(qsr, c('BASKET_ID', 'USER_ID', 'BANNER', 'BANNER_ID',
                   'RETAILER', 'CHANNEL', 'PARENT_CHANNEL',
                   'ITEM_ID', 'ITEM_QUANTITY', 'ITEM_UNIT_PRICE', 'ITEM_TOTAL',
                   'BASKET_SUB_TOTAL', 'BASKET_TOTAL',
                   'LOWEST_CATEGORY_ID',
                   'TRANSACTION_DATE', 'ORDER_METHOD_TYPE',
                   'DELIVERY_PROVIDER', 'ORDER_PROVIDER', 'POSTAL_CODE'))

fwrite(qsr, file.path(root, 'numerator', 'qsr_transactions_master.csv'))

# summary_data-combined.csv: basket-level
summary.tbl <- qsr[, .(
    BASKET_SUB_TOTAL = sum(ITEM_TOTAL),
    TRANSACTION_DATE = TRANSACTION_DATE[1],
    USER_ID          = USER_ID[1],
    BANNER           = BANNER[1],
    BANNER_ID        = BANNER_ID[1],
    RETAILER         = RETAILER[1],
    CHANNEL          = CHANNEL[1],
    PARENT_CHANNEL   = PARENT_CHANNEL[1],
    ORDER_METHOD     = ORDER_METHOD_TYPE[1],
    DELIVERY_METHOD  = DELIVERY_PROVIDER[1],
    POSTAL_CODE      = POSTAL_CODE[1]
), by = BASKET_ID]
summary.tbl[, BASKET_TOTAL := round(BASKET_SUB_TOTAL * runif(.N, 1.10, 1.30), 2)]
# Inject POBOX sentinels into summary too so identify_static_trans-summary.py's
# pandas.read_csv autodetects POSTAL_CODE as object (string), not int. Without
# this, combine_fact_summary.R's bind_rows() fails on a character/int mismatch.
n.sum.sentinel <- min(5L, nrow(summary.tbl))
sum.sentinel.idx <- sample.int(nrow(summary.tbl), n.sum.sentinel)
summary.tbl[sum.sentinel.idx, POSTAL_CODE := sprintf('POBOX%02d', seq_len(n.sum.sentinel))]
setcolorder(summary.tbl, c('BASKET_ID', 'USER_ID',
                           'BANNER', 'RETAILER', 'CHANNEL', 'PARENT_CHANNEL',
                           'ORDER_METHOD', 'DELIVERY_METHOD',
                           'TRANSACTION_DATE',
                           'BASKET_SUB_TOTAL', 'BASKET_TOTAL', 'POSTAL_CODE'))

fwrite(summary.tbl, file.path(root, 'numerator', 'summary_data-combined.csv'))

cat(sprintf('Numerator transactions: %d basket-rows, %d item-rows\n',
            nrow(summary.tbl), nrow(qsr)))

# ---------------------------------------------------------------------------
# Summary
# ---------------------------------------------------------------------------
cat('\n=== Synthetic data generation complete ===\n')
cat('Output tree: data/\n')
