# Combine the item-level tables

inpath.a <- 'data/numerator/standard_nmr_feed_item_table.csv'
inpath.b <- 'data/numerator/2022-03-04/standard_nmr_feed_item_table.csv'
outpath <- 'data/numerator/combined_item_table.rds'

item.a <- data.table::fread(inpath.a, sep = '|')
item.b <- data.table::fread(inpath.b, sep = '|')
item.a <- as.data.frame(item.a)
item.b <- as.data.frame(item.b)

# Drop groceries
item.a <- item.a[!grepl('grocery', item.a$SECTOR_ID), ]
item.b <- item.b[!grepl('grocery', item.b$SECTOR_ID), ]

# Prefer the more recent dataset on overlapping items
item.a <- item.a[which(!(item.a$ITEM_ID %in% item.b$ITEM_ID)), ]

item.combo <- dplyr::bind_rows(item.a, item.b)

saveRDS(item.combo, outpath)
