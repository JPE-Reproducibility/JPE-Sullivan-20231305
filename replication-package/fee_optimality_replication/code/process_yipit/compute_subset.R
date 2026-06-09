# Produce monthly subsets of the YipitData restaurant listings data
library(data.table)
library(dplyr)
library(EconTools)

main <- function(){
    inpath <- 'data/yipitdata/listings_v2.csv'
    inpath.geo <- 'data/geo/geo.csv'
    inpath.cbsa <- 'data/small_data/cbsa_codenames.csv'

    years <- c('2020', '2021')
    months <- list()
    months[['2020']] <- c('jan', 'feb', 'mar', 'apr', 'may', 'jun',
                          'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
    months[['2021']] <- c('jan', 'feb', 'mar', 'apr', 'may')

    # Load data
    df <- fread(inpath, keepLeadingZeros = TRUE)
    ## Delete certain columns to save memory
    df$restaurant_url <- NULL
    df$reference_observation_in_month <- NULL
    df$open_at_observation_time <- NULL
    df$metro_tier <- NULL
    df$phone_number <- NULL
    df$restaurant_tags <- NULL
    
    # Keep only US restaurants
    foreign <- c('France', 'India', 'Japan', 'Mexico', 'Norway')
    df <- df[which(!(df$address_country %in% foreign)), ]
    df$address_country <- NULL
    
    geo <- read.dat(inpath.geo)
    cbsa.ids <- read.dat(inpath.cbsa)

    geo$postal_code <- fix.zip.codes(geo$zip)
    geo.vars <- c('postal_code', 'CBSA_name')
    df <- left_join(df, geo[, geo.vars], by = 'postal_code')

    for (year in years){
        for (month in months[[year]]){
            month.id <- which(months[['2020']] == month)
            month.id <- ifelse(month.id <= 9, paste0('0',  as.character(month.id)), as.character(month.id))
            month.id <- sprintf('%s-%s-01', year, month.id)
            df.sub <- df[which(df$observation_month == month.id), ]
            outpath <- sprintf('data/yipitdata/listings_%s%s.rds', month, year)
            saveRDS(object = df.sub, file = outpath)
        }
    }
}

main()
