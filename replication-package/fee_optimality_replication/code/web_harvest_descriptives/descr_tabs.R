# Produce platform-level descriptives of the web-harvested fee data
# (OA Table F2 in optimal_fees_OA.tex).
library(dplyr)
library(EconTools)

inpath.fee   <- 'data/web_harvesting/analysis_data/analysis_data.rds'
outpath.descr <- 'output/web_harvest_descriptives/overall_descr.csv'

dat <- readRDS(inpath.fee)

# Delivery-details rows (delivery fees & wait times)
produce.details.row <- function(df){
    start.date <- as.Date('2021-04-01')
    end.date   <- as.Date('2021-06-30')
    df <- df[which(df$day >= start.date & df$day <= end.date), ]

    row <- c()
    row['n_details'] <- nrow(df)
    row['dfee']      <- mean(df$delivery_fee, na.rm = TRUE)
    row['dtime']     <- mean(df$wait_time,    na.rm = TRUE)
    return(row)
}
details.descr <- list(
    dd   = produce.details.row(dat$dd.details),
    uber = produce.details.row(dat$uber.details),
    gh   = produce.details.row(dat$gh.details),
    pm   = produce.details.row(dat$pm.nd)
)
details.tab <- as.data.frame(Reduce(rbind, details.descr))
cnames <- c('n_details', 'dfee', 'dtime')
colnames(details.tab) <- cnames
for (cn in setdiff(cnames, 'n_details')){
    details.tab[[cn]] <- sprintf('%0.2f', details.tab[[cn]])
}
details.tab$n_details <- sprintf('%d', details.tab$n_details)
details.tab$platform <- c('DD', 'Uber', 'GH', 'PM')
details.tab <- details.tab[, c('platform', cnames)]

# Non-delivery-fee rows (service fees & regulatory-response fees)
produce.non.d.row <- function(df, platform){
    start.date <- as.Date('2021-04-01')
    end.date   <- as.Date('2021-06-30')
    df <- df[which(df$day >= start.date & df$day <= end.date), ]

    row <- c()
    row['n_nond'] <- nrow(df)

    # Service fees
    if (platform == 'uber'){
        row['sfee'] <- 0.15
    } else if (platform == 'dd') {
        row['sfee'] <- mean(as.numeric(df$service_fee), na.rm = TRUE)
    } else {
        row['sfee'] <- mean(as.numeric(df$service_fee_pct), na.rm = TRUE)
    }
    # Regulatory-response fees
    if (platform == 'uber'){
        row['reg'] <- mean(as.numeric(df$local_fee) + as.numeric(df$driver_benefits), na.rm = TRUE)
    } else if (platform == 'dd') {
        row['reg'] <- mean(as.numeric(df$reg_response) + as.numeric(df$spec_fee_amt), na.rm = TRUE)
    } else {
        row['reg'] <- mean(as.numeric(df$driver_benefits) + as.numeric(df$spec_fee_amt), na.rm = TRUE)
    }
    return(row)
}
non.d.descr <- list(
    dd   = produce.non.d.row(dat$dd.nd,   'dd'),
    uber = produce.non.d.row(dat$uber.nd, 'uber'),
    pm   = produce.non.d.row(dat$pm.nd,   'pm')
)
non.d.tab <- as.data.frame(Reduce(rbind, non.d.descr))
cnames <- c('n_nond', 'sfee', 'reg')
colnames(non.d.tab) <- cnames
for (cn in setdiff(cnames, 'n_nond')){
    non.d.tab[[cn]] <- sprintf('%0.2f', non.d.tab[[cn]])
}
non.d.tab$n_nond <- sprintf('%d', non.d.tab$n_nond)
non.d.tab$platform <- c('DD', 'Uber', 'PM')
non.d.tab <- non.d.tab[, c('platform', cnames)]

# Combine and write
descr.tab <- full_join(details.tab, non.d.tab, by = 'platform')
descr.tab[is.na(descr.tab)] <- '-'
write.dat(descr.tab, outpath.descr)
