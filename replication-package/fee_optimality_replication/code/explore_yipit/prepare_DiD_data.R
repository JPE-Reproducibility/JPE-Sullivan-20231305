# Prepare data for evaluating platform fee responses to commission caps using the YipitData data
# Run the regressions in Stata
library(FoodDeliveryTools)
library(EconTools)

partnered.only <- TRUE
resto.v2 <- TRUE

v2.suffix <- ifelse(resto.v2, '_v2', '')

# Specify paths
inpath.panel <- 'data/yipitdata/consumer_panel.csv'
if (partnered.only){
    inpath.resto <- sprintf('data/yipitdata/resto_panel%s.rds', v2.suffix)
    inpath.indep <- sprintf('data/yipitdata/resto_panel_indep%s.rds', v2.suffix)
    inpath.chain <- sprintf('data/yipitdata/resto_panel_chain%s.rds', v2.suffix)
} else {
    inpath.resto <- sprintf('data/yipitdata/resto_panel_nonp%s.rds', v2.suffix)
    inpath.indep <- sprintf('data/yipitdata/resto_panel_indep_nonp%s.rds', v2.suffix)
    inpath.chain <- sprintf('data/yipitdata/resto_panel_chain_nonp%s.rds', v2.suffix)
}

inpath.zipmap <- 'data/geo/zip_map.rds'

inpath.caps <- 'data/fee_caps/monthly_fee_caps.csv'
inpath.excl.chains <- 'data/fee_caps/zip_fee_caps.rds'
inpath.acs <- 'data/ACS/processed/ACS_data.csv'
inpath.covid <- 'data/COVID/covid_processed.csv'
inpath.mkt.shares.2019 <- 'output/explore_numerator/market_shares_2019.rds'

# Data with covariates
inpath.geo      <- 'data/geo/geo_with_zctas.csv'
inpath.zip.pop  <- 'data/ACS/processed/ACS_data.csv'
inpath.policy   <- 'data/USA-covid-policy-master/processed.csv'
inpath.covid    <- 'data/COVID/covid_processed.csv'
inpath.covid.m  <- 'data/COVID/covid_monthly.rds'
inpath.election <- 'data/ElectionData/dataverse_files/countypres_2000-2020.csv'

outpath.dat   <- 'data/yipitdata/consumer_panel_processed.csv'
outpath.zip3  <- 'data/yipitdata/consumer_panel_zip3.csv'
if (partnered.only){
    outpath.resto <- sprintf('data/yipitdata/resto_counts_processed%s.csv', v2.suffix)
} else {
    outpath.resto <- sprintf('data/yipitdata/resto_counts_processed_nonp%s.csv', v2.suffix)
}


# Load data
dat <- read.dat(inpath.panel, colClasses = c('zip' = 'character'))
caps.df <- read.dat(inpath.caps, colClasses = c('zip' = 'character'))
excl.chain <- readRDS(inpath.excl.chains)
geo <- load.geo(inpath.geo)
resto <- readRDS(inpath.resto)
indep <- readRDS(inpath.indep)
chain <- readRDS(inpath.chain)
policy <- read.dat(inpath.policy)
zipmap <- readRDS(inpath.zipmap)

# ZIP code demographics
demo <- read.dat(inpath.acs)
demo$zip <- fix.zip.codes(demo$zcta)
demo$share_young <- demo$share_20_to_24 + demo$share_25_to_29 + demo$share_30_to_34
demo$share_married <- demo$married
demo$share_highinc <- 1 - demo$share_low_inc/100
demo <- demo[, c('zip', 'share_young', 'share_married', 'share_highinc')]

## Process policy
policy$month <- paste0(policy$month, '-01')

## ZIP populations
zip.pop.data <- load.zip.pop(inpath.zip.pop)
zip.pop      <- zip.pop.data$zip.pop
zip.pop3     <- zip.pop.data$zip.pop3
## COVID-related data
covid.dat <- load.covid(inpath.covid.m, geo, zip.pop)
covid     <- covid.dat$covid
covid3    <- covid.dat$covid3
## Political data
political <- process.political.data(inpath.election, geo, zip.pop)
vote.by.zip <- process.political.data(inpath.election, geo, zip.pop, by.zcta = TRUE)

## Into 0 to 1 scale
for (x in grep('Index$', colnames(policy), value = TRUE)){
    policy[, x] <- policy[, x]/100
}

# Add some variables
dat$total_fee <- dat$avg_service_fee + dat$avg_delivery_fee - dat$avg_order_discount
dat$aov_incl_2 <- dat$aov_feesandtips_excluded + dat$total_fee + dat$avg_order_tip + dat$avg_order_tax
dat$diff <- dat$aov_feesandtips_included - dat$aov_incl_2

dat$total_fee2 <- dat$aov_feesandtips_included - dat$aov_feesandtips_excluded - dat$avg_order_tax - dat$avg_order_tip
dat$total_fee3 <- dat$avg_service_fee + dat$avg_delivery_fee

dat$pct_fee <- dat$total_fee/dat$aov_feesandtips_excluded
dat$pct_fee2 <- dat$total_fee/dat$aov_feesandtips_excluded
dat$pct_dfee <- dat$avg_delivery_fee/dat$aov_feesandtips_excluded
dat$pct_sfee <- dat$avg_service_fee/dat$aov_feesandtips_excluded

caps.df$commission <- caps.df$cap
caps.df$has.cap    <- caps.df$commission < 0.3
dat <- dplyr::inner_join(dat, caps.df, by = c('zip', 'month'))

geo <- geo[which(geo$is.zcta), ]
dat <- dplyr::left_join(dat, geo, by = 'zip')

# Merge in ZCTAs' pops
acs <- read.dat(inpath.acs, colClasses = c('zcta' = 'character'))
acs <- acs[, c('zcta', 'population')]
dat$pop <- NULL
colnames(acs) <- c('zip', 'population')
dat <- dplyr::left_join(dat, acs, by = 'zip')

# Compute ZIP market shares in Jan/Feb 2020
## Eventually use 2019
mkt.shares <- doBy::summaryBy(orders_scaled + observed_orders_for_orders_scaled_calculation ~ CBSA_name + merchant_name,
                              data = dat[which(dat$month %in% c('2020-01-01', '2020-02-01', '2020-03-01')), ],
                              FUN = sum, keep.names = TRUE)
## Overall sales by region
mkt.sales <- doBy::summaryBy(orders_scaled + observed_orders_for_orders_scaled_calculation ~ CBSA_name,
                             data = mkt.shares, FUN = sum, keep.names = TRUE)
mkt.sales$observed_orders_for_orders_scaled_calculation <- NULL
colnames(mkt.sales) <- c('CBSA_name', 'sales')
mkt.shares <- dplyr::left_join(mkt.shares, mkt.sales, by = 'CBSA_name')
mkt.shares$market_share <- mkt.shares$orders_scaled/mkt.shares$sales

# Compute HHI by market
HHI.df <- doBy::summaryBy(market_share ~ CBSA_name, data = mkt.shares,
                          FUN = function(x) sum(x^2))
colnames(HHI.df) <- c('CBSA_name', 'HHI')

# Largest market share
mkt.leader <- doBy::summaryBy(market_share ~ CBSA_name, data = mkt.shares,
                          FUN = max)

# Merges
dat <- dplyr::left_join(dat, mkt.shares[, c('CBSA_name', 'merchant_name', 'market_share')],
                        by = c('CBSA_name', 'merchant_name'))
dat <- dplyr::left_join(dat, HHI.df, by = 'CBSA_name')
dat <- dplyr::left_join(dat, mkt.leader, by = 'CBSA_name')
dat$mkt_leader <- 1*(dat$market_share == dat$market_share.max)

dat$market_share[which(is.na(dat$market_share))] <- -1
dat$mkt_leader[which(is.na(dat$mkt_leader))] <- 0

# Load in 2019 market shares
shares2019 <- readRDS(inpath.mkt.shares.2019)
shares2019 <- shares2019[which(!is.na(shares2019$CBSA_name)), ]
shares2019 <- shares2019[, c('CBSA_name', 'platform', 'share')]
## Re-code the platform variable
recoding <- c('dd' = 'DoorDash', 'uber' = 'Uber', 'gh' = 'Grub Hub')
for (k in 1:length(recoding)){
    shares2019$platform[which(shares2019$platform == names(recoding)[k])] <- recoding[k]
}
shares2019 <- rename.var(shares2019, 'platform', 'merchant_name')
## Rename the share variable
shares2019 <- rename.var(shares2019, 'share', 'share2019')
dat <- dplyr::left_join(dat, shares2019, by = c('CBSA_name', 'merchant_name'))
dat$share2019[which(is.na(dat$share2019))] <- -1

# Merge in COVID
covid <- read.dat(inpath.covid)
covid <- covid[, c('fips', 'month', 'new_cases_pc', 'new_cases')]
covid$month <- paste0(covid$month, '-01')
## Add cumulative COVID-19 cases
covid <- covid[order(covid$fips, covid$month), ]
current.county <- covid$fips[1]
current.cases  <- 0
covid$cumul_cases_pc <- 0
for (k in 1:nrow(covid)){
    if (covid$fips[k] != current.county){
        current.cases  <- 0
        current.county <- covid$fips[k]
    }

    current.cases <- current.cases + ifelse(is.na(covid$new_cases_pc[k]), 0, covid$new_cases_pc[k])
    covid$cumul_cases_pc[k] <- current.cases
}

covid$fips <- convert.fips(covid$fips)
dat <- dplyr::inner_join(dat, covid, by = c('fips', 'month'))

# Merge data into restaurant counts data.frame
resto <- dplyr::inner_join(resto, geo, by = 'zip')
resto <- rename.var(resto, 'month', 'month_id')
resto$month <- sprintf('%d-%s-01', resto$year,
                       ifelse(resto$month_id < 10, paste0('0', resto$month_id), as.character(resto$month_id)))
resto <- dplyr::inner_join(resto, covid, by = c('fips', 'month'))
resto <- dplyr::inner_join(resto, caps.df, by = c('zip', 'month'))
resto <- dplyr::inner_join(resto, acs, by = 'zip')

# Merge in independent and chain specific counts
indep <- rename.var(indep, 'month', 'month_id')
chain <- rename.var(chain, 'month', 'month_id')
for (k in setdiff(colnames(indep), c('zip', 'month_id', 'year'))){
    indep <- rename.var(indep, k, paste0(k, '_indep'))
    chain <- rename.var(chain, k, paste0(k, '_chain'))
}
resto <- dplyr::left_join(resto, indep, by = c('zip', 'month_id', 'year'))
resto <- dplyr::left_join(resto, chain, by = c('zip', 'month_id', 'year'))

for (k in grep('_(indep|chain)', colnames(resto))){
    idx <- which(is.na(resto[, k]))
    resto[idx, k] <- 0
}

dat <- dat[which(!is.na(dat$total_fee2)), ]
dat <- dat[which(!is.na(dat$population)), ]
## Remove Puerto Rico
dat <- dat[which(dat$state != 'PR'), ]
## Remove NA HHI
dat <- dat[which(!is.na(dat$HHI)), ]

# Merge in policy
dat <- dplyr::left_join(dat, policy, by = c('state', 'month'))

# Merge in vote share
elect.zip <- process.political.data(inpath.election, geo, zip.pop, by.zcta = TRUE)
dat <- dplyr::inner_join(dat, elect.zip, by = 'zip')

# Change variable name
dat <- rename.var(dat, 'observed_orders_used_in_averagecalculations', 'n_orders')

## Merge in controls
policy <- load.policy(inpath.policy)
dat <- dplyr::left_join(dat, policy, by = c('state', 'month'))

## Merge in excluding chains indicator
excl.chain <- excl.chain$zip.df
### Collapse to month level
excl.chain$month <- sub('-[0-9]{2}$', '-01', excl.chain$date)
excl.chain <- doBy::summaryBy(excl_chains ~ zip + month, excl.chain,
                              FUN = max, keep.names = TRUE)
dat <- dplyr::left_join(dat, excl.chain, by = c('zip', 'month'))

# Add in the number of orders used for average calculations
avg.order <- read.dat(inpath.panel, colClasses = c('zip' = 'character'))
avg.order <- avg.order[, c('month', 'merchant_name', 'zip', 
                           'observed_orders_used_in_averagecalculations')]
dat <- dplyr::inner_join(dat, avg.order, by = c('month', 'zip', 'merchant_name'))



#== Finish processing restaurant data ==#
resto <- dplyr::left_join(resto, policy, by = c('state', 'month'))
resto <- dplyr::left_join(resto, vote.by.zip, by = c('zip'))

resto <- resto[which(!is.na(resto$new_cases_pc)), ]
resto <- resto[which(!is.infinite(resto$new_cases_pc)), ]
resto <- resto[which(!is.infinite(resto$cumul_cases_pc)), ]
resto <- resto[which(!is.infinite(resto$cumul_cases_pc)), ]
resto <- resto[which(!is.na(resto$stringency)), ]
resto <- resto[which(!is.na(resto$democrat_share)), ]
## Re-define the month_id variable
resto$month_id <- resto$month_id + 12*(resto$year == 2021)

## Add information about ZIPs with caps that exclude chains
resto <- dplyr::left_join(resto, excl.chain, by = c('zip', 'month'))

write.table(resto, outpath.resto, quote = FALSE, sep = '|', row.names = FALSE)

#== Write sales panel to file after merging in restaurant information ==#
resto.sub <- resto[, c('zip', 'month', 'nplatforms', 'online',
                       'DoorDash', 'UberEats', 'Grubhub', 'Postmates', 'nresto')]

# Change so that it provides nearby restaurant counts
shr.vars <- c('online', 'nplatforms', 'DoorDash', 'UberEats', 'Grubhub', 'Postmates')

for (v in shr.vars){
    nv <- paste0('n_', v)
    resto.sub[, nv] <- resto.sub[, v]*resto.sub$nresto
}

# Initialize new variables
for (v in shr.vars){
    nv <- paste0('n_nearby_', v)
    resto.sub[, nv] <- NA
}

for (k in 1:nrow(resto.sub)){
    if (k %% 10000 == 0){
        pracma::fprintf('%0.2f pct\n', k/nrow(resto.sub)*100)
    }
    zip.k   <- resto.sub$zip[k]
    month.k <- resto.sub$month[k]
    nearby.k <- zipmap[[zip.k]]
    if (is.null(nearby.k)){
        next
    }
    idx <- which(resto.sub$zip %in% nearby.k & resto.sub$month == month.k)
    
    for (v in shr.vars){
        n.var <- paste0('n_', v)
        nearby.var <- paste0('n_nearby_', v)
        resto.sub[k, nearby.var] <- sum(resto.sub[idx, n.var])
    }
}


resto.sub <- rename.var(resto.sub, 'DoorDash',   'J_dd_share')
resto.sub <- rename.var(resto.sub, 'UberEats',   'J_uber_share')
resto.sub <- rename.var(resto.sub, 'Grubhub',    'J_gh_share')
resto.sub <- rename.var(resto.sub, 'Postmates',  'J_pm_share')
resto.sub <- rename.var(resto.sub, 'nresto',     'J_tot')
resto.sub <- rename.var(resto.sub, 'online',     'J_online_share')
resto.sub <- rename.var(resto.sub, 'nplatforms', 'nplatform_avg')

# Merge in restaurant data
dat <- dplyr::left_join(dat, resto.sub, by = c('zip', 'month'))

# Merge in demographic data
dat <- dplyr::left_join(dat, demo, by = 'zip')

# Save data
write.table(dat, outpath.dat, quote = FALSE, sep = '|', row.names = FALSE)

#== Collapse consumer data to ZIP3 ==#
dat$zip3 <- substr(dat$zcta, 1, 3)
y.fee <- c('aov_feesandtips_included', 'aov_feesandtips_excluded',
           'avg_service_fee', 'avg_delivery_fee', 'avg_order_tax',
           'avg_order_discount', 'avg_order_tip', 'total_fee',
           'aov_incl_2', 'total_fee2', 'pct_fee', 'pct_fee2',
           'pct_dfee', 'pct_sfee')
y.other <- c('StringencyIndex', 'new_cases_pc',
             'HHI', 'commission', 'cap')
x <- c('zip3', 'month', 'merchant_name')
w.fee <- 'n_orders'
w.other <- 'population'
dat.agg.fee   <- collapse.weighted(y.fee, x, w.fee,   dat)

## Add in additional variables
caps.df <- rename.var(caps.df, 'zip', 'zcta')
caps.df <- dplyr::left_join(caps.df, zip.pop, by = 'zcta')
caps.df <- caps.df[which(!is.na(caps.df$population)), ]
caps.agg <- collapse.weighted(c('cap', 'commission'), c('zip3', 'month'),
                             'population', caps.df)
caps.agg$population <- caps.agg$population/1e6
covid3$population <- NULL
ctrl <- dplyr::inner_join(caps.agg, covid3, by = c('zip3', 'month'))
ctrl <- dplyr::inner_join(ctrl, political, by = 'zip3')

geo$zip3 <- substr(geo$zcta, 1, 3)
geo.sub <- geo[, c('zip3', 'state', 'CBSA_name')]
geo.sub <- geo.sub[!duplicated(geo.sub$zip3), ]
ctrl <- dplyr::inner_join(ctrl, geo.sub, by = 'zip3')
ctrl <- dplyr::inner_join(ctrl, policy, by = c('state', 'month'))
ctrl$new_cases_pc_pop <- NULL

## Also get minimum cap in each zip3
min.cap <- doBy::summaryBy(cap + commission ~ zip3 + month,
                           data = caps.df, FUN = min)

## Combine datasets
dat.agg <- dplyr::left_join(dat.agg.fee, ctrl,  by = c('zip3', 'month'))
dat.agg <- dplyr::left_join(dat.agg, min.cap, by = c('zip3', 'month'))
dat.agg <- dat.agg[which(dat.agg$population > 0), ]
write.dat(dat.agg, outpath.zip3, sep = '|')
