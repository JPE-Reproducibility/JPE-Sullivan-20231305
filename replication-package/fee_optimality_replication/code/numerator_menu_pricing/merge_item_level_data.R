# Merge data into the item-level price panel in order to conduct
# regression analysis in Stata

library(EconTools)
library(FoodDeliveryTools)

# Transactions
inpath.buy <- 'data/numerator/item_level_price_panel.rds'
# Number of listings
inpath.list <- 'data/numerator/nlistings_for_price_analysis.rds'
# Caps
inpath.caps <- 'data/numerator/commission_cap_for_price_analysis.rds'
# Geographical
inpath.geo <- 'data/geo/geo_with_zctas.csv'
# Item characteristics 
inpath.item.char <- 'data/numerator/item_characteristics.rds'
# Item prices and sales
inpath.item.ps <- 'data/numerator/item_prices_and_sales.rds'

# Path to outputs
outpath <- 'data/numerator/item_level_price_panel_merged.csv'

# Read the data
buy      <- readRDS(inpath.buy)
listings <- readRDS(inpath.list)
caps     <- readRDS(inpath.caps)
geo      <- load.geo(inpath.geo)

i.char <- readRDS(inpath.item.char)
i.ps   <- readRDS(inpath.item.ps)

buy <- dplyr::inner_join(buy, caps, by = c('zip', 'month'))
buy <- dplyr::left_join(buy, listings, by = c('zip3', 'month', 'platform'))
buy$n_listings[is.na(buy$n_listings)]     <- 0
buy$log_listings[is.na(buy$log_listings)] <- 0

# Merge in geographical data
geo.vars <- c('zip', 'state', 'pop')
buy <- dplyr::left_join(buy, geo[, geo.vars], by = 'zip')

# Merge in item-level data
buy$LOWEST_CATEGORY_ID <- NULL
buy <- dplyr::left_join(buy, i.char, by = 'ITEM_ID')
remove(list = 'i.char')
gc()

i.ps$in_item_price_data <- 1
buy$ITEM <- sprintf('%s-%s', buy$BANNER_ID, buy$ITEM_ALT)
buy <- dplyr::left_join(buy, i.ps, by = 'ITEM')

buy$in_item_price_data[is.na(buy$in_item_price_data)] <- 0

write.dat(buy, outpath, quote = TRUE)
