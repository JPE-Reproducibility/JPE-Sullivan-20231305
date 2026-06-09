# Collapse the static-panelist date ranges to one row per user.

library(EconTools)
library(doBy)

main <- function(){
    inpath      <- 'data/numerator/combined_static_table.csv'
    outpath     <- 'data/numerator/static_users-combined.rds'
    outpath.csv <- 'data/numerator/static_users-combined.csv'
    collapse.static(inpath, outpath, outpath.csv, sep = ',')
}

collapse.static <- function(inpath, outpath, outpath.csv, sep = '|'){
    static <- read.csv(file = inpath, stringsAsFactors = FALSE, sep = sep)
    static$START_DATE <- as.Date(static$START_DATE)
    static$END_DATE   <- as.Date(static$END_DATE)

    static.dates <- summaryBy(START_DATE + END_DATE ~ USER_ID,
                              data = static, FUN = c(min, max))
    static.dates$START_DATE.max <- NULL
    static.dates$END_DATE.min   <- NULL
    colnames(static.dates) <- c('USER_ID', 'START_DATE', 'END_DATE')
    origin <- as.Date('1970-01-01')
    static.dates$START_DATE <- as.Date(static.dates$START_DATE, origin = origin)
    static.dates$END_DATE   <- as.Date(static.dates$END_DATE,   origin = origin)

    saveRDS(object = static.dates, file = outpath)
    write.dat(static.dates, file = outpath.csv)
}

main()
