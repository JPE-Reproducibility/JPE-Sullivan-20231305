# Plot Numerator-derived platform market shares by CBSA (Q2 2021)
# (Figure 1 in optimal_fees.tex).
library(RColorBrewer)
library(EconTools)

main <- function(){
    outdir <- 'output/explore_numerator'
    quarter <- 'Q2-2021'
    outpath.small <- sprintf('%s/market_shares_%s_small.pdf', outdir, quarter)
    make.plot(plot.width = 8, aspect.ratio = 1/1.5, outpath.shrs = outpath.small, quarter = quarter)
}

make.plot <- function(plot.width, aspect.ratio, outpath.shrs, quarter){
    inpath.nmr.cbsa <- sprintf('output/explore_numerator/shrs_%s.csv', quarter)

    PLs <- c(DD = 'dd', Uber = 'uber', GH = 'gh', PM = 'pm')

    df <- read.dat(inpath.nmr.cbsa, sep = ';')
    colnames(df) <- c('CBSA_name', 'uber', 'dd', 'gh', 'pm')
    df <- df[, c('CBSA_name', 'dd', 'uber', 'gh', 'pm')]

    df$CBSA_name <- sub('-.*$', '', df$CBSA_name)

    mkts <- df$CBSA_name

    df.t <- df
    rownames(df.t) <- df.t$CBSA_name
    df.t$CBSA_name <- NULL
    df.t <- t(df.t)

    for (k in 1:ncol(df.t)){
        df.t[, k] <- df.t[, k]/sum(df.t[, k])
    }

    cols <- brewer.pal(4, 'Pastel2')

    pdf(outpath.shrs, width = plot.width, height = plot.width*aspect.ratio)
    par(mar = c(5.1, 5, 1.5, 1.7) + 0.1)
    plt <- barplot(df.t,
                   col = cols,
                   space = 0.1,
                   font.axis = 1, cex.lab = 1.4,
                   xlim = c(0, ncol(df.t) + 4), axes = FALSE,
                   xlab = '', ylab = 'Market share', las = 0, xaxt = "n",
                   args.legend = list(x = ncol(df.t)/2,
                                      y = 0,
                                      bty = "n", horiz = TRUE))
    axis(side = 2, yaxp = c(0, 1, 5), cex.axis = 1.4)
    text(plt, par("usr")[3], labels = mkts,
         srt = 45, adj = c(1.1,1.1), xpd = TRUE, cex = 1.2)
    legend(x = ncol(df.t) + 1.5, y = 0.6, rev(names(PLs)), col = rev(cols), horiz = FALSE,
           bty = 'n', pch = 15, cex = 1.5)
    dev.off()
}

main()
