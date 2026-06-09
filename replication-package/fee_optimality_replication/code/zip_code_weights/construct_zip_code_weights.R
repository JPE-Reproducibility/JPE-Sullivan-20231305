# Compute the number of food delivery orders placed in each zip code
# as estimated in the YipitData consumer panel data
library(doBy)
library(EconTools)

main <- function(){
    inpath.yd   <- 'data/yipitdata/consumer_panel.csv'
    inpath.geo  <- 'data/geo/geo_with_zctas.csv'
    inpath.cbsa <- 'data/est_dat/cbsa_ids.csv'
    outpath     <- 'data/yipitdata/order_estimates_apr.csv'
    months      <- '2021-04-01'

    df       <- read.dat(inpath.yd,   colClasses = c('zip' = 'character'))
    geo      <- read.dat(inpath.geo,  colClasses = c('zip' = 'character'))
    cbsa.ids <- read.dat(inpath.cbsa, sep = '|')

    platforms <- c('DoorDash', 'Grub Hub', 'Postmates', 'Uber')
    idx <- which((df$month %in% months) & (df$merchant_name %in% platforms) & !is.na(df$zip))
    df.sub <- df[idx, ]
    df.sub <- summaryBy(orders_scaled ~ zip, data = df.sub,
                        FUN = function(x) sum(x, na.rm = TRUE), keep.names = TRUE)
    df.sub <- dplyr::left_join(df.sub, geo, by = 'zip')

    df.sub <- df.sub[which(df.sub$CBSA_name %in% cbsa.ids$CBSA_name), ]
    df.sub <- df.sub[, c('zip', 'orders_scaled', 'CBSA_name')]
    colnames(df.sub) <- c('zip', 'orders_YD', 'CBSA_name')
    df.sub <- dplyr::left_join(df.sub, cbsa.ids, by = 'CBSA_name')

    write.dat(df.sub, file = outpath, quote = TRUE)
}

main()
