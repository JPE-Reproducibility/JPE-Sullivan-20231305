# Produce a monthly panel of static panelists' spending by category.

library(data.table)
library(doBy)
library(tidyr)
library(dplyr)
library(EconTools)

main <- function(){
    inpath.price  <- 'data/prices/price_indices_v3.csv'
    inpath.static <- 'data/numerator/static_users-combined.rds'
    inpath.qsr    <- 'data/numerator/all_baskets_static-combined.rds'

    outpath.panel   <- 'data/numerator/static_panel_monthly-combined.rds'
    outpath.periods <- 'data/numerator/months_df-combined.rds'

    geo    <- read.dat(inpath.price)
    static <- readRDS(inpath.static)
    buy    <- readRDS(inpath.qsr)

    # Keep only static transactions
    buy <- buy[which(buy$static_trans), ]

    categories <- unique(buy$category)

    # Construct list of all months considered
    start.date <- as.Date('2019-01-01')
    end.date   <- as.Date('2021-12-31')
    days       <- seq(from = start.date, to = end.date, by = 1)
    periods    <- unique(sub('-[0-9]+$', '', as.character(days)))
    nperiods   <- length(periods)

    period.df <- data.frame(date = days)
    period.df$period <- sub('-[0-9]+$', '', as.character(days))
    period.level <- data.frame(period = periods, period.id = 1:nperiods)
    period.df <- merge(period.df, period.level, by = 'period')

    # Merge period into buy
    buy$date <- as.Date(buy$date)
    buy <- merge(x = buy, y = period.df, by = 'date',
                 all.x = TRUE, all.y = FALSE)

    # Collapse
    summ <- function(x) sum(x, na.rm = TRUE)
    static.panel <- summaryBy(BASKET_SUB_TOTAL + BASKET_TOTAL ~ USER_ID + category + period.id,
                              data = buy, id = 'period', FUN = c(length, summ))
    static.panel$BASKET_SUB_TOTAL.length <- NULL
    colnames(static.panel)[colnames(static.panel) == 'BASKET_TOTAL.length']   <- 'n_baskets'
    colnames(static.panel)[colnames(static.panel) == 'BASKET_TOTAL.summ']     <- 'basket_total'
    colnames(static.panel)[colnames(static.panel) == 'BASKET_SUB_TOTAL.summ'] <- 'basket_subtotal'

    # Reshape to wide
    static.panel <- merge(static.panel, static, by = 'USER_ID', all.x = TRUE, all.y = TRUE)

    wide.panel <- pivot_wider(static.panel, id_cols = c('USER_ID', 'period.id', 'period'),
                              names_from = category,
                              values_from = c(n_baskets, basket_subtotal, basket_total),
                              values_fill = list(n_baskets = 0, basket_subtotal = 0, basket_total = 0))

    # Fill in zeros and drop months when the panelist is not in the static panel
    all <- expand(wide.panel, USER_ID, period.id)
    all <- merge(x = all, y = period.level, by = 'period.id', all.x = TRUE, all.y = FALSE)
    all <- merge(x = all, y = static,       by = 'USER_ID',   all.x = TRUE, all.y = TRUE)
    all <- all[which(!is.na(all$period.id)), ]
    all$period <- as.character(all$period)
    START_MONTH <- sub('-[0-9]+$', '', as.character(all$START_DATE))
    END_MONTH   <- sub('-[0-9]+$', '', as.character(all$END_DATE))
    all <- all[which(all$period >= START_MONTH & all$period <= END_MONTH), ]
    all <- all[order(all$USER_ID, all$period.id), ]

    all$period <- NULL
    all$START_DATE <- NULL
    all$END_DATE <- NULL
    wide.panel$period <- NULL
    wide.panel <- right_join(wide.panel, all, by = c('USER_ID', 'period.id'))
    wide.panel <- wide.panel[order(wide.panel$USER_ID, wide.panel$period.id), ]
    val.cols <- setdiff(colnames(wide.panel), c('USER_ID', 'period.id'))
    for (vc in val.cols){
        wide.panel[is.na(wide.panel[, vc]), vc] <- 0
    }

    idx <- !grepl('_NA$', colnames(wide.panel))
    wide.panel <- wide.panel[, idx]

    saveRDS(wide.panel, file = outpath.panel)
    saveRDS(period.df, file = outpath.periods)
}

main()
