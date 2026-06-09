# Produce a table of consumers who don't belong to the static panel.
library(doBy)
library(EconTools)

inpath.non  <- 'data/numerator/all_baskets_nonstatic-combined.rds'
outpath.csv <- 'data/numerator/nonstatic_users-combined.csv'
outpath.rds <- 'data/numerator/nonstatic_users-combined.rds'

nonstatic <- readRDS(inpath.non)

# Keep only users with at least two transactions
ntrans <- table(nonstatic$USER_ID)
panelists <- names(ntrans)[which(ntrans >= 2)]
nonstatic <- nonstatic[nonstatic$USER_ID %in% panelists, ]

nonstatic$TRANSACTION_DATE <- as.Date(nonstatic$TRANSACTION_DATE)

# Determine each panelist's first and last observed transaction date
origin <- as.Date('1970-01-01')
ns.tab <- summaryBy(TRANSACTION_DATE ~ USER_ID, nonstatic, FUN = c(min, max))
colnames(ns.tab) <- c('USER_ID', 'START_DATE', 'END_DATE')
ns.tab$START_DATE <- as.Date(ns.tab$START_DATE, origin = origin)
ns.tab$END_DATE   <- as.Date(ns.tab$END_DATE,   origin = origin)

# Save the table
write.csv(x = ns.tab, file = outpath.csv, row.names = FALSE, quote = FALSE)
saveRDS(ns.tab, file = outpath.rds)
