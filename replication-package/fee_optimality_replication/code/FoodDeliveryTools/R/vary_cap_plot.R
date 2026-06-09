make.vary.cap.plot <- function(Results.m, cap.levels, outpath,
                               breakeven.line = FALSE){

    Results.cap <- Results.m$cap
    ## For each level of commission cap, extract consumer welfare
    counties <- names(Results.m$BL)

    CW <- c()
    PP <- c()
    RP <- c()
    S  <- c()
    for (k in 1:length(cap.levels)){
        CW.k <- c()
        PP.k <- c()
        RP.k <- c()
        S.k  <- c()
        for (co in counties){
            CW.k[co] <- Results.cap[[co]][[k]]$CW$total.EU.dollar
            PP.k[co] <- sum(Results.cap[[co]][[k]]$p.profit)
            RP.k[co] <- Results.cap[[co]][[k]]$r.profit['total']
            S.k[co]  <- Results.cap[[co]][[k]]$S
        }
        CW[k] <- sum(CW.k)
        PP[k] <- sum(PP.k)
        RP[k] <- sum(RP.k)
        S[k]  <- sum(S.k)
    }
    w.dat.m <- data.frame(CW = CW, PP = PP, RP = RP)
    w.dat.m$tot <- rowSums(w.dat.m)

    ## plot changes relative to a cap of 30
    w.dat.m$cap <- cap.levels
    w.dat.m$S   <- S

    vary.cap.data.to.plot(w.dat.m, outpath, breakeven.line)

    return(w.dat.m)
}


vary.cap.data.to.plot <- function(w.dat.m, outpath, breakeven.line = FALSE){

    # Graphical parameters
    ## Plot height and width
    H <- 6.5
    W <- 8
    ## Line widths
    LWD <- 3
    ## Colours
    colours <- RColorBrewer::brewer.pal(4, 'Dark2')
    colours[1] <- 'black'
    ## Line styles
    LTYs <- c(1, 2, 6, 4)

    # Relative versions of the variables
    idx.30 <- which(w.dat.m$cap == 30)
    S0 <- w.dat.m$S[idx.30]
    for (k in c('CW', 'PP', 'RP', 'tot')){
        new.var <- paste0(k, '_relchg')
        w.dat.m[, new.var] <- (w.dat.m[, k] - w.dat.m[idx.30, k])/S0
    }

    # Determine breakeven point
    if (breakeven.line){
        dat.neg <- w.dat.m[which(w.dat.m$PP < 0), ]
        dat.pos <- w.dat.m[which(w.dat.m$PP > 0), ]

        if (nrow(dat.neg) > 0 & nrow(dat.pos) > 0){
            # linear interpolation
            idx0 <- which.max(dat.neg$PP)
            idx1 <- which.min(dat.pos$PP)
            y0 <- dat.neg$PP[idx0]
            y1 <- dat.pos$PP[idx1]
            x0 <- dat.neg$cap[idx0]
            x1 <- dat.pos$cap[idx1]

            BE <- x0 - y0*(x1 - x0)/(y1 - y0)
            ## Similarly extrapolate the total welfare effect
            tw0 <- dat.neg$tot_relchg[idx0]
            tw1 <- dat.pos$tot_relchg[idx1]
            tw.BE <- tw0 + (tw1 - tw0)/(x1 - x0)*(BE - x0)
        } else {
            BE <- NULL
        }
    } else {
        BE <- NULL
    }

    # Choose x-axis limits
    xlim0 <- floor(min(w.dat.m$cap)/5)*5
    xlim1 <- ceiling(max(w.dat.m$cap)/5)*5
    xlim <- c(xlim0, xlim1)

    # Choose y-axis limits
    combo <- c(w.dat.m$CW_relchg, w.dat.m$PP_relchg,
               w.dat.m$RP_relchg, w.dat.m$tot_relchg)
    ylim <- range(combo)

    diff <- ylim[2] - ylim[1]
    if (diff > 4){
        yfac <- 1
    } else {
        yfac <- 0.1
    }
    ypoints <- seq(from = floor(min(ylim)/yfac)*yfac,
                   to = ceiling(max(ylim)/yfac)*yfac,
                   by = yfac)

    pdf(outpath, width = W, height = H)
    par(mar = c(5, 5, 2, 2))
    plot(w.dat.m$cap, w.dat.m$tot_relchg, type = 'l', ylim = ylim,
         xlim = xlim, lwd = LWD, col = colours[1],
         axes = FALSE, cex.lab = 1.4,
         xlab = 'Regulated commission level (%)',
         ylab = 'Welfare change ($/baseline platform orders)')
    grid()
    lines(w.dat.m$cap, w.dat.m$RP_relchg, col = colours[2], lwd = LWD, lty = LTYs[2])
    lines(w.dat.m$cap, w.dat.m$PP_relchg, col = colours[3], lwd = LWD, lty = LTYs[3])
    lines(w.dat.m$cap, w.dat.m$CW_relchg, col = colours[4], lwd = LWD, lty = LTYs[4])

    abline(h = 0)
    axis(1, cex.axis = 1.2)
    axis(2, at = ypoints, cex.axis = 1.1)
    abline(v = 30, lty = 2, col = 'grey40')

    if (breakeven.line & !is.null(BE)){
        abline(v = BE, lty = 3, lwd = 3)
        text(x = BE, y = ylim[1],
             labels = sprintf('Break-even (%d%%)', round(BE)),
             pos = 4, font = 2, cex = 1.1)

        # Value of total welfare at the breakeven line
        which(w.dat.m$cap == round(BE))

        points(x = BE, y = tw.BE, pch = 16, cex = 1.75)
        text(x = BE, y = tw.BE + 0.20, labels = sprintf('$%.2f', tw.BE),
             pos = 4, cex = 1.1)
        segments(x0 = 0, x1 = BE, y0 = tw.BE, lty = 3, lwd = 3)
    }

    legend(x = 'topright', legend = c('Total welfare', 'Restaurant profits',
                                      'Platform profits', 'Consumer welfare'),
           lty = LTYs, col = colours, lwd = c(LWD, LWD, LWD, LWD),
           bg = 'white', cex = 1.5)

    dev.off()
}

