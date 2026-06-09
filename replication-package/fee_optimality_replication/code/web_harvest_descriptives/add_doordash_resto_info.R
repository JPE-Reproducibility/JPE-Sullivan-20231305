## Merge YipitData restaurant data into the DoorDash details dataset
library(data.table)
library(dplyr)
library(EconTools)

## Merge YD into DoorDash
inpath.dd.details   <- 'data/web_harvesting/doordash_details/geocoded_details.csv'
inpath.YD <- 'data/yipitdata/listings_v2.csv'
dropvars <- c('open_hours', 'open_at_observation_time',
              'offers_pickup', 'phone_number',
              'estimated_min_order_value_cents', 'metro_tier',
              'restaurant_tags', 'estimated_delivery_fee_cents',
              'address_country', 'reference_observation_in_month',
              'fulfills_own_deliveries')

outpath <- 'data/web_harvesting/analysis_data/doordash_with_resto_info.csv'

determine.id <- function(link, sep.char = '/'){
    link.parts <- strsplit(link, sep.char)
    id <- link.parts[[1]]
    id <- id[length(id)]
    return(id)
}

df.dd   <- read.csv(inpath.dd.details, stringsAsFactors = FALSE)
df.dd$link <- sub('\\/\\?utm_campaign=gpa', '', df.dd$link)
df.dd$restaurant_id <- sapply(df.dd$link, determine.id, sep.char = '-')

YD <- fread(inpath.YD, drop = dropvars)
YD <- YD[platform == 'DoorDash']
YD <- YD[grep('^2021', YD$observation_month), ]
YD <- YD[!duplicated(YD[, c('restaurant_id')]), ]

colnames(YD)[colnames(YD) == 'latitude']  <- 'latitude_YD'
colnames(YD)[colnames(YD) == 'longitude'] <- 'longitude_YD'
colnames(YD)[colnames(YD) == 'name']      <- 'name_YD'

df.dd <- left_join(df.dd, YD, by ='restaurant_id')

create.dir(dirname(outpath))
write.dat(df.dd, outpath, quote = TRUE)



