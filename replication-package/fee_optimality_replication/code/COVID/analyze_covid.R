# Produce a county-level dataset of COVID-19 cases and
# food delivery order volumes
library(EconTools)
library(FoodDeliveryTools)

inpath.covid <- 'data/COVID/time_series_covid19_confirmed_US.csv'
inpath.buy   <- 'data/yipitdata/consumer_panel.csv'
inpath.geo   <- 'data/geo/geo_with_zctas.csv'

outpath.monthly <- 'data/COVID/covid_monthly.rds'
outpath <- 'data/COVID/covid_processed.csv'

covid <- read.dat(inpath.covid, colClasses = c('FIPS' = 'character'))
buy   <- read.dat(inpath.buy)
geo   <- read.dat(inpath.geo)

buy$zip <- fix.zip.codes(buy$zip)
geo$zip <- fix.zip.codes(geo$zip)

# Fix FIPS codes
covid$FIPS <- sub('.0', '', covid$FIPS, fixed = TRUE)
covid <- covid[which(nchar(covid$FIPS) >= 4), ]
covid$FIPS <- convert.fips(as.numeric(covid$FIPS))

# Relabel column names in the COVID data
colnames(covid) <- gsub('^X', 'date', colnames(covid))

covid <- tidyr::pivot_longer(covid, cols = tidyr::starts_with('date'),
                             names_to = 'date', values_to = 'cases')
covid <- as.data.frame(covid)
# Delete some variables
delete.vars <- c('UID', 'iso2', 'iso3', 'code3', 'Country_Region', 'Lat', 'Long_')
covid <- covid[, setdiff(colnames(covid), delete.vars)]

# Aggregate to month level
## Day of the month
covid$day   <- sub('date[0-9]*\\.', '', covid$date)
covid$day   <- sub('\\.[0-9]+$', '', covid$day)
covid$day   <- ifelse(nchar(covid$day) == 1, paste0('0', covid$day), covid$day)
## Month
covid$month <- sub('\\.[0-9]+\\.[0-9]+$', '', covid$date)
covid$month <- sub('^date', '', covid$month)
covid$month <- ifelse(nchar(covid$month) == 1, paste0('0', covid$month), covid$month)

covid$year  <- paste0('20', sub('date[0-9]+\\.[0-9]+\\.', '', covid$date))
covid$date <- sprintf('%s-%s-%s', covid$year, covid$month, covid$day)
covid$date <- as.Date(covid$date)

# Add a month variable that also identifies the year
covid$month <- paste0(covid$year, '-', covid$month)

# Keep only the first day of each month that belongs to the data
covid <- covid[order(covid$Combined_Key, covid$date), ]
covid.monthly <- covid[!duplicated(covid[, c('Combined_Key', 'month')]), ]

# new_cases at month t = cumulative cases at start of month t+1 minus start of month t,
# i.e. cases that accrued during month t (each retained row is the first day of its month).
idx.1 <- 2:nrow(covid.monthly)
covid.monthly$new_cases <- c(covid.monthly$cases[idx.1], NA) - covid.monthly$cases
# Truncate negatives (occur when JHU issued downward corrections to cumulative counts).
covid.monthly$new_cases[which(covid.monthly$new_cases < 0)] <- 0
lead.county <- c(covid.monthly$Combined_Key[idx.1], NA)
covid.monthly$new_cases[which(covid.monthly$Combined_Key != lead.county)] <- NA

# Save data
covid.monthly <- rename.var(covid.monthly, 'FIPS', 'fips')
saveRDS(covid.monthly, outpath.monthly)

# Process transactions data
buy <- buy[which(buy$merchant_name %in% c('Uber', 'DoorDash', 'Grub Hub', 'Postmates')), ]
buy$order_total <- buy$orders_scaled*buy$aov_feesandtips_excluded
buy <- buy[, c('month', 'merchant_name', 'zip', 'order_total', 'orders_scaled')]
buy <- doBy::summaryBy(order_total + orders_scaled ~ month + zip,
                       data = buy, FUN = function(x) sum(x, na.rm = TRUE), keep.names = TRUE)


## Collapse to county
buy <- dplyr::inner_join(buy, geo[, c('zip', 'fips', 'county')], by = 'zip')
buy <- doBy::summaryBy(order_total + orders_scaled ~ month + fips,
                       data = buy, id = c('county'), keep.names = TRUE,
                       FUN = function(x) sum(x, na.rm = TRUE))
geo.fips <- doBy::summaryBy(pop ~ fips,
                            data = geo, id = c('county', 'county_pop', 'county_name'), keep.names = TRUE,
                            FUN = function(x) sum(x, na.rm = TRUE))
# Merge in geography
keep.vars <- c('fips', 'county', 'pop', 'county_pop', 'county_name')
buy <- dplyr::inner_join(buy, geo.fips[, keep.vars], by = 'fips')

# Merge together the COVID and transactions datasets
buy$fips <- convert.fips(buy$fips)
buy$month <- sub('-01$', '', buy$month)

## INNER JOIN
## Check any fips that aren't in both DFs
covid.monthly <- dplyr::inner_join(covid.monthly, buy, by = c('fips', 'month'))

covid.monthly$new_cases_pc   <- covid.monthly$new_cases/covid.monthly$pop
covid.monthly$orders_pc      <- covid.monthly$orders_scaled/covid.monthly$pop
covid.monthly$order_total_pc <- covid.monthly$order_total/covid.monthly$pop

# Save
covid.monthly$county.x <- NULL
covid.monthly$county <- covid.monthly$county.y
covid.monthly$county.y <- NULL
write.dat(covid.monthly, file = outpath, quote = TRUE)

