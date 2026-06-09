library(data.table)
library(EconTools)

inpath.connect <- 'data/numerator/static_connect_table.csv'
inpath.orders <- 'data/numerator/qsr_baskets_static-combined.csv'

outpath.dat <- 'data/numerator/daily_panel.csv'

connect <- read.dat(inpath.connect, sep = '|')
orders  <- fread(inpath.orders)

orders <- orders[which(orders$in_static == 1), ]
orders$TRANSACTION_DATE <- as.Date(orders$TRANSACTION_DATE)
orders <- as.data.frame(orders)

connect <- connect[which(connect$EMAIL_CONNECT == 1), ]
colnames(connect)[1:2] <- c('START_CONNECT', 'END_CONNECT')

connect$START_CONNECT <- as.Date(connect$START_CONNECT)
connect$END_CONNECT   <- as.Date(connect$END_CONNECT)

orders <- orders[which(orders$USER_ID %in% connect$USER_ID), ]

min.date <- doBy::summaryBy(START_CONNECT ~ USER_ID, connect, FUN = min, keep.names = TRUE)
max.date <- doBy::summaryBy(END_CONNECT   ~ USER_ID, connect, FUN = max, keep.names = TRUE)
connect.minmax <- dplyr::left_join(min.date, max.date, by = 'USER_ID')
connect.minmax$START_CONNECT <- as.Date(connect.minmax$START_CONNECT)
connect.minmax$END_CONNECT   <- as.Date(connect.minmax$END_CONNECT)


orders <- dplyr::left_join(orders, connect.minmax, by = 'USER_ID')

# Drop orders outside window
idx <- which(orders$TRANSACTION_DATE >= orders$START_CONNECT &
             orders$TRANSACTION_DATE <= orders$END_CONNECT)
orders <- orders[idx, ]

orders$online <- 1*(orders$DELIVERY_PROVIDER != 'na')

keep.cols <- c('USER_ID', 'TRANSACTION_DATE', 'online', 'START_CONNECT', 'END_CONNECT')
orders <- orders[, keep.cols]


orders <- as.data.table(orders)

# Step 1: Create full USER_ID/day panel between START_CONNECT and END_CONNECT
panel <- orders[, .(DATE = seq(min(START_CONNECT), max(END_CONNECT), by = "day")),
                by = USER_ID]

# Step 2: Merge with original orders to flag order types
orders[, ORDER_TYPE := ifelse(online == 1, "online", "offline")]
orders_flag <- dcast(orders[, .(USER_ID, TRANSACTION_DATE, ORDER_TYPE)],
                     USER_ID + TRANSACTION_DATE ~ ORDER_TYPE,
                     fun.aggregate = length, value.var = "ORDER_TYPE")

# Rename columns
setnames(orders_flag, c("online", "offline"), c("ONLINE_ORDER", "OFFLINE_ORDER"))

# Step 3: Merge order flags onto panel
setnames(orders_flag, "TRANSACTION_DATE", "DATE")
panel <- merge(panel, orders_flag, by = c("USER_ID", "DATE"), all.x = TRUE)
panel[is.na(ONLINE_ORDER), ONLINE_ORDER := 0]
panel[is.na(OFFLINE_ORDER), OFFLINE_ORDER := 0]

# Step 4: Create "lead" variables for orders in the next 3 or 4 days
setorder(panel, USER_ID, DATE)

# Create rolling lead indicators
panel[, ONLINE_NEXT3 := as.integer(Reduce(`+`, shift(ONLINE_ORDER, -1:-3, fill = 0)) > 0), by = USER_ID]
panel[, OFFLINE_NEXT3 := as.integer(Reduce(`+`, shift(OFFLINE_ORDER, -1:-3, fill = 0)) > 0), by = USER_ID]
panel[, ONLINE_NEXT7 := as.integer(Reduce(`+`, shift(ONLINE_ORDER, -1:-7, fill = 0)) > 0), by = USER_ID]
panel[, OFFLINE_NEXT7 := as.integer(Reduce(`+`, shift(OFFLINE_ORDER, -1:-7, fill = 0)) > 0), by = USER_ID]

panel <- panel[DATE >= as.Date("2019-01-01")]

norders <- doBy::summaryBy(online ~ USER_ID, data = orders, FUN = c(sum, length))
valid.users <- norders$USER_ID[which(norders$online.sum > 0 & norders$online.sum < norders$online.length)]
panel <- panel[USER_ID %in% valid.users]

# Restrict to dates between each user's first and last order (any type)
panel <- panel[, {
  first_order <- min(DATE[ONLINE_ORDER == 1 | OFFLINE_ORDER == 1])
  last_order  <- max(DATE[ONLINE_ORDER == 1 | OFFLINE_ORDER == 1])
  .SD[DATE >= first_order & DATE <= last_order]
}, by = USER_ID]

panel$month <- sub('-[0-9]{2}$', '', panel$DATE)
panel$year <- sub('-[0-9]{2}$', '', panel$month)
write.dat(panel, outpath.dat)



