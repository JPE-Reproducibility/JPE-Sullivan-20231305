# Prepare two item-level helper datasets used by merge_item_level_data.R and
# the OA Figure D1 scatter plots in describe_prices.R:
#   - item_characteristics.rds: combined_item_table.rds augmented with an
#     ITEM_ALT identifier (a row index over the unique combinations of
#     non-ITEM_ID item columns).
#   - item_prices_and_sales.rds: per-ITEM platform-by-platform and
#     online-vs-offline mean/SD/count of ITEM_UNIT_PRICE.
#
# Reading the un-merged item_level_price_panel.rds (rather than the merged
# CSV produced downstream by merge_item_level_data.R) breaks the cycle that
# would otherwise exist between describe_prices.R and merge_item_level_data.R.

library(EconTools)

main <- function(){
    inpath.df   <- 'data/numerator/item_level_price_panel.rds'
    inpath.item <- 'data/numerator/combined_item_table.rds'

    outpath.char <- 'data/numerator/item_characteristics.rds'
    outpath.ps   <- 'data/numerator/item_prices_and_sales.rds'

    df   <- readRDS(inpath.df)
    item <- readRDS(inpath.item)

    # Generate an alternative ITEM identifier (a row index over unique
    # combinations of item columns other than ITEM_ID)
    item.no.dupl <- item
    item.no.dupl$ITEM_ID <- NULL
    item.no.dupl <- item.no.dupl[!duplicated(item.no.dupl), ]
    item.no.dupl$ITEM_ALT <- 1:nrow(item.no.dupl)
    join.vars <- intersect(colnames(item), colnames(item.no.dupl))
    item <- dplyr::left_join(item, item.no.dupl, join.vars)
    saveRDS(item, outpath.char)

    # Merge item into df (drop overlapping columns other than ITEM_ID first)
    df$LOWEST_CATEGORY_ID <- NULL
    common.names <- setdiff(intersect(names(df), names(item)), 'ITEM_ID')
    for (cn in common.names){
        df[, cn] <- NULL
    }
    df <- dplyr::left_join(df, item, by = 'ITEM_ID')

    # Drop platform-banner rows (BANNER_ID == 'doordash' / 'ubereats' /
    # 'grubhubcom') and unknown brands before aggregating prices.
    na.labels <- c('unknown', 'N/A')
    banner.pl <- c('doordash', 'ubereats', 'grubhubcom')
    df$brand.na <- (df$BRAND %in% na.labels)
    df$pbanner  <- (df$BANNER_ID %in% banner.pl)
    df.sub <- df[which(!df$pbanner & !df$brand.na), ]
    df.sub$online <- ifelse(df.sub$platform != 'na', 'on', 'off')
    df.sub$ITEM <- sprintf('%s-%s', df.sub$BANNER_ID, df.sub$ITEM_ALT)

    # Per-platform and online/offline mean/SD/count of ITEM_UNIT_PRICE
    pagg <- doBy::summaryBy(ITEM_UNIT_PRICE ~ platform + ITEM,
                            data = df.sub, FUN = c(mean, sd, length))
    pagg2 <- doBy::summaryBy(ITEM_UNIT_PRICE ~ online + ITEM,
                             data = df.sub, FUN = c(mean, sd, length))
    colnames(pagg)[3:5]  <- c('MEAN', 'SD', 'N')
    colnames(pagg2)[3:5] <- c('MEAN', 'SD', 'N')

    pagg  <- as.data.frame(tidyr::pivot_wider(pagg,  names_from = platform,
                                              values_from = c(MEAN, SD, N)))
    pagg2 <- as.data.frame(tidyr::pivot_wider(pagg2, names_from = online,
                                              values_from = c(MEAN, SD, N)))
    pagg2$tot.N <- rowSums(pagg2[, grep('^N', colnames(pagg2))], na.rm = TRUE)

    out <- dplyr::left_join(pagg, pagg2, by = 'ITEM')
    saveRDS(out, outpath.ps)
}

main()
