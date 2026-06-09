# Characterize the extent of restaurant multihoming
library(EconTools)

inpath.plat  <- 'data/yipitdata/plat_loc_level_w_infogroup2021_apr2021.rds'
inpath.loc   <- 'data/yipitdata/location_level_w_infogroup2021_apr2021.rds'
inpath.CBSAs <- 'data/small_data/cbsa_codenames.csv'

outdir <- 'output/describe_portfolio_choice'
create.dir(outdir)
outpath.dist <- sprintf('%s/portfolio_distribution_ig2021.pdf', outdir)

platforms <- readRDS(inpath.plat)
locs      <- readRDS(inpath.loc)
CBSAs     <- read.dat(inpath.CBSAs)

CBSAs <- CBSAs$CBSA_name

idx <- c(which(platforms$is_partnered_merchant), which(is.na(platforms$is_partnered_merchant)))
idx <- unique(idx)
platforms <- platforms[idx, ]
locs <- locs[which(locs$cbsa %in% CBSAs), ]
platforms <- platforms[which(platforms$loc.id %in% locs$loc.id), ]

PLs <- c("DD" = 'DoorDash',
         "Uber" = 'UberEats',
         "GH" = 'Grubhub',
         "PM" = 'Postmates')

for (PL in PLs){
    locs[, PL] <- 1*(locs$loc.id %in% platforms$loc.id[which(platforms$platform == PL)])
}
locs$G <- paste0(locs$DoorDash, locs$UberEats, locs$Grubhub, locs$Postmates)
locs.tab <- table(locs$G)
locs.tab <- locs.tab[c('0000', '1000', '0100', '0010', '0001',
                       '1100', '1010', '1001', '0110', '0101', '0011',
                       '1110', '1101', '1011', '0111', '1111')]
portfolio.names <- c('None',         'DD',           'Uber',   'GH',       'PM',
                     'DD, Uber',     'DD, GH',       'DD, PM', 'Uber, GH', 'Uber, PM', 'GH, PM',
                     'DD, Uber, GH', 'DD, Uber, PM', 'DD, GH, PM', 'Uber, GH, PM',
                     'All')

cols <- RColorBrewer::brewer.pal(5, 'Purples')
shrs <- locs.tab/sum(locs.tab)
shr.cols <- cols[sapply(1:16, function(k) sum(as.numeric(strsplit(names(locs.tab), '')[[k]])) + 1)]

# Plot
shrs.alt <- shrs
names(shrs.alt) <- portfolio.names
pdf(outpath.dist, height = 5, width = 5.6)
par(mar = c(4.2, 7, 2, 2))
barplot(rev(shrs.alt), xlim = c(0, 0.5), las = 1, col = 'white', axes = FALSE,
        horiz = TRUE, xlab = 'Share of restaurants')
for (k in 1:5){
    abline(v = k/10, col = 'lightgrey', lty = 2)
}
barplot(rev(shrs.alt), xlim = c(0, 0.5), las = 1,
        col = rev(shr.cols), add = TRUE, horiz = TRUE)
dev.off()
