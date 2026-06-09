# Compare Numerator and Edison panel market shares against Second Measure
# (OA Figures E1 and E2 in optimal_fees_OA.tex).
library(dplyr)
library(doBy)
library(tidyr)
library(RColorBrewer)
library(EconTools)

inpath.geo      <- 'data/geo/geo_with_zctas.csv'
inpath.cbsa.code <- 'data/small_data/cbsa_codenames.csv'
inpath.sm.cbsa  <- 'data/small_data/second_measure_by_cbsa_march_2021.csv'
inpath.nmr.cbsa <- 'output/explore_numerator/shrs_Q1-2021.csv'
inpath.YD       <- 'data/yipitdata/consumer_panel.csv'

outdir <- 'output/explore_numerator'
outpath.cbsa.shrs <- paste0(outdir, '/cbsa_shares_SM_NMR.pdf')
outpath.YD.shrs   <- paste0(outdir, '/cbsa_shares_SM_YD.pdf')

# Load data
geo        <- read.dat(inpath.geo, colClasses = c(zip = 'character', zcta = 'character'))
sm.cbsa    <- read.dat(inpath.sm.cbsa)
nmr.cbsa   <- read.dat(inpath.nmr.cbsa, sep = ';')
cbsa.codes <- read.dat(inpath.cbsa.code)
YD         <- read.dat(inpath.YD)

YD$zip <- fix.zip.codes(YD$zip)

# Plot 1: Numerator vs Second Measure
nmr.cbsa <- merge(nmr.cbsa, cbsa.codes, by = 'CBSA_name')
nmr.long <- pivot_longer(nmr.cbsa, cols = starts_with('basket_subtotal_'),
                         names_to = 'platform', values_to = 'share_numerator', values_drop_na = TRUE)
nmr.long$platform <- sub('basket_subtotal_', '', nmr.long$platform)
nmr.long <- inner_join(nmr.long, sm.cbsa, by = c('cbsa', 'platform'))

cols <- brewer.pal(4, 'Dark2')
PLs <- c(DoorDash = 'dd', Uber = 'uber', Grubhub = 'gh', Postmates = 'pm')
names(cols) <- PLs
pchs <- c(3, 4, 8, 20)
names(pchs) <- PLs

plot.width <- 5
ratio <- 1
pdf(outpath.cbsa.shrs, width = plot.width, height = plot.width*ratio)
plot(nmr.long$share_numerator, nmr.long$share, col = cols[nmr.long$platform],
     pch = pchs[nmr.long$platform], ylim = c(0, 0.8),
     xlab = 'Market share, Numerator panel (Q1 2021)',
     ylab = 'Market share, Second Measure (March 2021)', axes = FALSE)
axis(side = 1, xaxp = c(0, 0.8, 4))
axis(side = 2, yaxp = c(0, 0.8, 4))
text(x = 0.18, y = 0.7, labels = sprintf('Correlation = %0.2f', cor(nmr.long$share_numerator, nmr.long$share)))
grid()
legend(x = 0.6, y = 0.25, legend = names(PLs), col = cols, pch = pchs,
       box.lwd = 0, box.col = "white", bg = "white", cex = 0.85)
abline(a = 0, b = 1)
dev.off()

# Plot 2: Edison (YipitData) vs Second Measure
YD <- left_join(YD, geo[, c('zip', 'CBSA_name')], by = 'zip')
YD <- YD[which(YD$merchant_name %in% c('DoorDash', 'Uber', 'Grub Hub', 'Postmates')), ]
YD$sales <- YD$aov_feesandtips_included*YD$orders_scaled
YD.agg <- summaryBy(sales ~ CBSA_name + merchant_name, data = YD[which(YD$month == '2021-03-01'), ],
                    FUN = function(x) sum(x, na.rm = TRUE), keep.names = TRUE)
YD.agg <- YD.agg[which(YD.agg$CBSA_name %in% cbsa.codes$CBSA_name), ]
colnames(YD.agg) <- c('CBSA_name', 'platform', 'sales')
by.market <- summaryBy(sales ~ CBSA_name, data = YD.agg, FUN = sum, keep.names = TRUE)
colnames(by.market) <- c('CBSA_name', 'market_sales')
YD.agg <- left_join(YD.agg, by.market, by = 'CBSA_name')
YD.agg$share_YD <- YD.agg$sales/YD.agg$market_sales
YD.agg$platform <- c('DoorDash' = 'dd', 'Uber' = 'uber', 'Grub Hub' = 'gh', 'Postmates' = 'pm')[YD.agg$platform]

nmr.long <- left_join(nmr.long, YD.agg, by = c('CBSA_name', 'platform'))

pdf(outpath.YD.shrs, width = plot.width, height = plot.width*ratio)
plot(nmr.long$share_YD, nmr.long$share, col = cols[nmr.long$platform],
     pch = pchs[nmr.long$platform], ylim = c(0, 0.8),
     xlab = 'Market share, Edison panel',
     ylab = 'Market share, Second Measure', axes = FALSE)
axis(side = 1, xaxp = c(0, 0.8, 4))
axis(side = 2, yaxp = c(0, 0.8, 4))
text(x = 0.18, y = 0.7, labels = sprintf('Correlation = %0.2f', cor(nmr.long$share_YD, nmr.long$share)))
grid()
legend(x = 0.6, y = 0.25, legend = names(PLs), col = cols, pch = pchs,
       box.lwd = 0, box.col = "white", bg = "white", cex = 0.85)
abline(a = 0, b = 1)
dev.off()
