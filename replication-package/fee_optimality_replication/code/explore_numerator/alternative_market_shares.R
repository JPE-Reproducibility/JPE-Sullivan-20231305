# Compute market shares by CBSA in 2019 using the baskets data
library(EconTools)

inpath.stat <- 'data/numerator/all_baskets_static-combined.rds'
inpath.non  <- 'data/numerator/all_baskets_nonstatic-combined.rds'
inpath.geo <- 'data/geo/geo_with_zctas.csv'

outpath <- 'output/explore_numerator/market_shares_2019.rds'

dat <- readRDS(inpath.stat)
non <- readRDS(inpath.non)
geo <- read.dat(inpath.geo, colClasses = c('zip' = 'character'))

dat <- dat[which(dat$platform != 'outside'), ]
non <- non[which(non$platform != 'outside'), ]

# Drop later dates
dat <- dat[grep('^2019', dat$month), ]
non <- non[grep('^2019', non$month), ]

# Combine datasets
dat <- dplyr::bind_rows(dat, non)

# Keep observations with valid ZIPs
dat <- rename.var(dat, 'POSTAL_CODE', 'zip')
dat <- dat[which(!is.na(dat$zip)), ]
dat <- dplyr::left_join(dat, geo, by = 'zip')

# Merge with geo data
sales.by.metro <- doBy::summaryBy(BASKET_TOTAL ~ CBSA_name + platform, data = dat,
                                  FUN = function(x) sum(x, na.rm = TRUE),
                                  keep.names = TRUE)
tot.sales <- doBy::summaryBy(BASKET_TOTAL ~ CBSA_name, data = dat,
                             FUN = function(x) sum(x, na.rm = TRUE),
                             keep.names = TRUE)
colnames(tot.sales) <- c('CBSA_name', 'tot_sales')

sales.by.metro <- dplyr::left_join(sales.by.metro, tot.sales, by = 'CBSA_name')
sales.by.metro$share <- sales.by.metro$BASKET_TOTAL/sales.by.metro$tot_sales

saveRDS(sales.by.metro, outpath)
