# Assign brands (i.e. chains) to individual restaurant locations
library(data.table)
library(FoodDeliveryTools)


main <- function(){
    data.dir <- 'data/yipitdata'
    r.times <- platform.adoption.times()
    years   <- r.times$years
    months  <- r.times$months

    for (year in years){
        for (month in months[[year]]){
            add.brands(data.dir, month, year)
        }
    }
}

add.brands <- function(data.dir, month, year){
    inpath  <- sprintf('%s/listings_%s%s.rds', data.dir, month, year)
    outpath <- sprintf('%s/brands_%s%s.rds',   data.dir, month, year)

    df <- readRDS(inpath)
    # data.table to data.frame conversion
    df <- as.data.frame(df)

    # Assign top brands
    df$brand <- NA

    for (k in 1:length(brand.map)){
        cat(sprintf('%d/%d\n', k, length(brand.map)))
        brand.pattern  <- brand.map[k]
        brand.name <- names(brand.map)[k]
        df <- assign.brand(df, brand.name, brand.pattern, restaurant.var = 'name')
    }

    saveRDS(df, outpath)
}

main()

