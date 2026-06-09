# Produce OA Figure D1: scatter plots of mean ITEM_UNIT_PRICE across
# (DoorDash, Uber, Grubhub) pairs and online vs offline.

library(EconTools)

main <- function(){
    inpath <- 'data/numerator/item_prices_and_sales.rds'

    outdir <- 'output/numerator_menu_pricing/descr/scatter_prices'
    create.dir(outdir)
    outpath.dd.ub  <- sprintf('%s/dd_uber.pdf', outdir)
    outpath.dd.gh  <- sprintf('%s/dd_gh.pdf',   outdir)
    outpath.ub.gh  <- sprintf('%s/uber_gh.pdf', outdir)
    outpath.off.on <- sprintf('%s/off_on.pdf',  outdir)

    pagg <- readRDS(inpath)

    pdf(outpath.dd.ub)
    produce.scatter(pagg, 'dd', 'uber', 50, 0.66)
    dev.off()

    pdf(outpath.dd.gh)
    produce.scatter(pagg, 'dd', 'gh', 50, 0.66)
    dev.off()

    pdf(outpath.ub.gh)
    produce.scatter(pagg, 'uber', 'gh', 50, 0.66)
    dev.off()

    pdf(outpath.off.on)
    produce.scatter(pagg, 'off', 'on', 50, 0.66)
    dev.off()
}

produce.scatter <- function(pagg, f1, f2, min.n = 50, max.coef = 1, max.ratio = 1.75, max.SE = 0.1){
    var1 <- pagg[, sprintf('MEAN_%s', f1)]
    var2 <- pagg[, sprintf('MEAN_%s', f2)]

    N1 <- pagg[, sprintf('N_%s', f1)]
    N2 <- pagg[, sprintf('N_%s', f2)]

    SD1 <- pagg[, sprintf('SD_%s', f1)]
    SD2 <- pagg[, sprintf('SD_%s', f2)]

    SE1 <- SD1/sqrt(N1)
    SE2 <- SD2/sqrt(N2)

    coef1 <- SD1/var1
    coef2 <- SD2/var2

    # Apply subsetting rules
    idx <- which(N1 >= min.n & N2 >= min.n &                       # Minimum sample size
                 coef1 <= max.coef & coef2 <= max.coef &            # Coefficient of variation
                 var1 <= max.ratio*var2 & var2 <= max.ratio*var1 & # Drop outliers in terms of ratio
                 SE1/var1 <= max.SE & SE2/var1 <= max.SE)
    var1 <- var1[idx]
    var2 <- var2[idx]

    name.map <- c('na' = 'Offline', 'uber' = 'Uber Eats',
                  'dd' = 'DoorDash', 'gh' = 'Grubhub',
                  'off' = 'Offline', 'on' = 'Online')
    nm1 <- paste0(name.map[f1], ' price')
    nm2 <- paste0(name.map[f2], ' price')

    if (length(var1) == 0 || all(is.na(var1)) || all(is.na(var2))) {
        # Subsetting filtered everything out (synthetic-data path).
        plot.new()
        text(0.5, 0.5, 'No items satisfy the subsetting rules')
        return(invisible())
    }
    ylim <- c(0, quantile(c(var1, var2), 0.99, na.rm = TRUE))
    plot(var1, var2, axes = FALSE, ylim = ylim, xlim = ylim,
         xlab = nm1, ylab = nm2)
    grid()
    axis(1)
    axis(2)

    reg <- lm(var2 ~ var1)
    r2  <- summary(reg)$r.squared
    text(x = max(ylim)*0.08, y = max(ylim)*0.95, labels = sprintf('R2    = %0.2f', r2))
    text(x = max(ylim)*0.08, y = max(ylim)*0.90, labels = sprintf('slope = %0.2f', reg$coefficients[2]))
    text(x = max(ylim)*0.08, y = max(ylim)*0.85, labels = sprintf('N     = %d', length(reg$residuals)))
    abline(0, 1)
    abline(reg, lty = 2, col = 'red')
}

main()
