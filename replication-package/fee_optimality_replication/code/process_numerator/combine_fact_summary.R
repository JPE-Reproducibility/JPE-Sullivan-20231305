library(dplyr)

library(EconTools)

main <- function(){


    # Use combined data (original + March 2022)?
    use.combo <- TRUE

    inpath.banner <- 'data/numerator/standard_nmr_feed_banner_table.csv'

    if (use.combo){
        inpath.fact.static <- 'data/numerator/qsr_baskets_static-combined.csv'
        inpath.fact.non    <- 'data/numerator/qsr_baskets_nonstatic-combined.csv'

        inpath.summary.static <- 'data/numerator/summary_baskets_static-combined.csv'
        inpath.summary.non    <- 'data/numerator/summary_baskets_nonstatic-combined.csv'

        outpath.static <- 'data/numerator/all_baskets_static-combined.csv'
        outpath.non    <- 'data/numerator/all_baskets_nonstatic-combined.csv'
    } else {
        inpath.fact.static <- 'data/numerator/qsr_baskets_static.csv'
        inpath.fact.non    <- 'data/numerator/qsr_baskets_nonstatic.csv'

        inpath.summary.static <- 'data/numerator/summary_baskets_static.csv'
        inpath.summary.non    <- 'data/numerator/summary_baskets_nonstatic.csv'

        outpath.static <- 'data/numerator/all_baskets_static.csv'
        outpath.non    <- 'data/numerator/all_baskets_nonstatic.csv'
    }

    banner <- read.csv(inpath.banner, sep = '|', stringsAsFactors = FALSE)

    all.static <- process.all.obs(inpath.fact.static, inpath.summary.static, banner)
    all.non    <- process.all.obs(inpath.fact.non,    inpath.summary.non,    banner)

    write.dat(all.static, outpath.static, quote = TRUE)
    write.dat(all.non, outpath.non, quote = TRUE)
}

process.all.obs <- function(inpath.fact, inpath.summary, banner){
    # Read data
    fact.dat    <- read.dat(inpath.fact)
    summary.dat <- read.dat(inpath.summary)
    # Process the summary data
    summary.dat <- prepare.summary(summary.dat, banner)
    summary.dat <- summary.dat[, intersect(colnames(summary.dat), colnames(fact.dat))]
    # Combine the datasets
    fact.dat$dataset    <- 'fact'
    summary.dat$dataset <- 'summary'
    fact.dat$BASKET_ID <- as.character(fact.dat$BASKET_ID)
    all.dat <- bind_rows(fact.dat, summary.dat)
    # Drop duplicated observations
    all.dat <- all.dat[order(all.dat$BASKET_ID, all.dat$dataset), ]
    all.dat <- all.dat[!duplicated(all.dat$BASKET_ID), ]

    return(all.dat)
}

prepare.summary <- function(summary.dat, banner){
    summary.dat <- left_join(summary.dat, banner, by = c('BANNER', 'RETAILER', 'CHANNEL', 'PARENT_CHANNEL'))
    summary.dat <- rename.var(summary.dat, 'ORDER_METHOD', 'ORDER_METHOD_TYPE')
    summary.dat <- rename.var(summary.dat, 'DELIVERY_METHOD', 'DELIVERY_METHOD_TYPE')
    return(summary.dat)
}

main()
