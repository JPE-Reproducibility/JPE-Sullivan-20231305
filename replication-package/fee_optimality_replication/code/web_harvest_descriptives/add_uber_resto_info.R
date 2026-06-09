# Add data from YipitData to the Uber order details data
library(geosphere)
library(data.table)
library(dplyr)
library(EconTools)

# Specify paths
inpath.uber.details <- 'data/web_harvesting/uber_details/geocoded_details.csv'
inpath.YD <- 'data/yipitdata/listings_v2.csv'
dropvars <- c('open_hours', 'open_at_observation_time',
              'offers_pickup', 'phone_number',
              'estimated_min_order_value_cents', 'metro_tier',
              'restaurant_tags', 'estimated_delivery_fee_cents',
              'address_country', 'reference_observation_in_month',
              'fulfills_own_deliveries')

## To outputs
outdir <- 'data/web_harvesting/analysis_data'
create.dir(outdir)
outpath <- sprintf('%s/uber_with_resto_info.csv', outdir)

df.uber   <- read.dat(inpath.uber.details)
YD <- fread(inpath.YD, drop = dropvars)
YD <- YD[platform == 'UberEats']

# Limit to recent observations
YD <- YD[grep('^2021', YD$observation_month), ]

colnames(YD)[colnames(YD) == 'postal_code'] <- 'zip'
YD$zip <- fix.zip.codes(YD$zip)


determine.code.1 <- function(link, prestring){
    link <- strsplit(link, '/')[[1]]
    if (prestring %in% link){
        idx <- which(link == prestring)
        idx <- idx[length(idx)]
        code <- link[idx + 1]
    } else {
        code <- 'missing'
    }
    return(code)
}

determine.code.2 <- function(link){
    link <- strsplit(link, '/')[[1]]
    code <- link[length(link)]
    return(code)
}



## Attempt 1: merge using URL codes
df.uber$code  <- sapply(df.uber$link, determine.code.1, prestring = 'food-delivery')
df.uber$code2 <- sapply(df.uber$link, determine.code.2)

YD <- as.data.frame(YD)
YD$restaurant_url <- sub('en-US', 'en-US\\/', YD$restaurant_url)
YD$restaurant_url <- gsub('\\/\\/', '\\/', YD$restaurant_url)

YD$code <- sapply(YD$restaurant_url, determine.code.1, prestring = 'store')
idx <- which(YD$code %in% c('missing', 'food-delivery', ''))
# Empty-idx assignment from sapply() returns list(0) which would coerce
# YD$code into a list column; skip the assignment when there's nothing to do.
if (length(idx) > 0) {
    YD$code[idx] <- sapply(YD$restaurant_url[idx], determine.code.1, prestring = 'food-delivery')
}
idx <- which(YD$code %in% c('missing', 'food-delivery', ''))
if (length(idx) > 0) {
    YD$code[idx] <- sapply(YD$restaurant_url[idx], determine.code.1, prestring = 'en-US')
}
YD$code2 <- sapply(YD$restaurant_url, determine.code.2)

# drop duplicates
YD.no.dupl <- YD[!duplicated(YD$code2), ]
YD.no.dupl <- rename.var(YD.no.dupl, 'latitude', 'latitude_YD')
YD.no.dupl <- rename.var(YD.no.dupl, 'longitude', 'longitude_YD')
YD.no.dupl <- rename.var(YD.no.dupl, 'code', 'code_YD')
YD.no.dupl <- rename.var(YD.no.dupl, 'zip', 'zip_YD')
YD.no.dupl <- rename.var(YD.no.dupl, 'name', 'name_YD')
df.uber <- left_join(df.uber, YD.no.dupl, by = 'code2')


write.dat(df.uber, outpath, quote = TRUE)


