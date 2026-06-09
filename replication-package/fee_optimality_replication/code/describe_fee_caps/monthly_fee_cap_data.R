# Make a data.frame of fee caps at the month/ZCTA level
library(EconTools)
library(data.table)

inpath.caps  <- 'data/fee_caps/zip_fee_caps.rds'
outpath.caps <- 'data/fee_caps/monthly_fee_caps.csv'

caps <- readRDS(inpath.caps)
caps <- caps$zip.cap.data

## Caps to month
for (z in names(caps)){
    caps.z <- caps[[z]]
    caps.z$month <- sub('-[0-9]{2}$', '-01', caps.z$date)
    caps.z <- caps.z[!duplicated(caps.z$month), ]
    caps.z$pop  <- NULL
    caps.z$date <- NULL
    caps.z$zip <- z
    caps[[z]] <- caps.z
}
caps.df <- rbindlist(caps)
caps.df$cap[which(is.infinite(caps.df$cap))] <- 0.30

write.dat(caps.df, file = outpath.caps)
