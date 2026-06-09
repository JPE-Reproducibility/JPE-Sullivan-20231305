# Characterize the prices charged to consumers and restaurants in the
# YipitData consumer panel dataset.

library(RColorBrewer)
library(data.table)
library(EconTools)


main <- function(){
    inpath.dat <- 'data/yipitdata/consumer_panel.csv'
    inpath.caps <- 'data/fee_caps/zip_fee_caps.rds'
    outdir.plots <- 'output/explore_yipit/price_structure'
    create.dir(outdir.plots)

    # Specify paths to outputs
    ## All ZCTAs
    outpath.lvl <- sprintf('%s/price_levels.pdf', outdir.plots)
    outpath.shr <- sprintf('%s/price_shares.pdf', outdir.plots)
    ## ZCTAs w/ caps
    outpath.lvl.cap <- sprintf('%s/price_levels_cap.pdf', outdir.plots)
    outpath.shr.cap <- sprintf('%s/price_shares_cap.pdf', outdir.plots)
    ## ZCTAs w/o caps
    outpath.lvl.no.cap <- sprintf('%s/price_levels_no_cap.pdf', outdir.plots)
    outpath.shr.no.cap <- sprintf('%s/price_shares_no_cap.pdf', outdir.plots)

    df <- read.dat(inpath.dat, colClasses = c('zip' = 'character'))
    caps <- readRDS(inpath.caps)
    caps <- caps$zip.cap.data

    # Subset the data
    platforms <- c('DoorDash', 'Uber', 'Grub Hub', 'Postmates')
    df <- df[which(df$merchant_name %in% platforms), ]
    df <- df[which(df$zip != ''), ]

    months <- unique(df$month)

    # Merge in cap data
    ## Caps to month
    for (z in names(caps)){
        caps.z <- caps[[z]]
        caps.z$month <- sub('-[0-9]{2}$', '-01', caps.z$date)
        caps.z <- caps.z[!duplicated(caps.z$month), ]
        caps.z$pop  <- NULL
        caps.z$date <- NULL
        caps.z$zip <- z
        caps[[z]] <- caps.z
    }
    caps.df <- rbindlist(caps)
    caps.df$cap[which(is.infinite(caps.df$cap))] <- 0.30

    df <- dplyr::left_join(df, caps.df, by = c('month', 'zip'))

    # All
    prices <- list()
    shares <- list()
    # Cap
    prices.cap <- list()
    shares.cap <- list()
    # No cap
    prices.no.cap <- list()
    shares.no.cap <- list()
    
    cap.zips <- caps.df$zip[which(caps.df$month == '2021-05-01' & caps.df$cap < 0.3)]
    no.cap.zips <- caps.df$zip[which(caps.df$month == '2021-05-01' & caps.df$cap == 0.3)]
    idx.cap    <- which(df$zip %in% cap.zips)
    idx.no.cap <- which(df$zip %in% no.cap.zips)
    
    for (platform in platforms){
        all <- compute.price.levels.and.shares(months, df, platform)
        cap <- compute.price.levels.and.shares(months, df[idx.cap, ], platform)
        no.cap <- compute.price.levels.and.shares(months, df[idx.no.cap, ], platform)
        
        prices[[platform]]        <- all$prices.f
        prices.cap[[platform]]    <- cap$prices.f
        prices.no.cap[[platform]] <- no.cap$prices.f
        
        shares[[platform]]        <- all$shares.f
        shares.cap[[platform]]    <- cap$shares.f
        shares.no.cap[[platform]] <- no.cap$shares.f
    }

    price.df        <- combine.data.frame(prices)
    price.cap.df    <- combine.data.frame(prices.cap)
    price.no.cap.df <- combine.data.frame(prices.no.cap)
    
    share.df         <- combine.data.frame(shares)
    shares.cap.df    <- combine.data.frame(shares.cap)
    shares.no.cap.df <- combine.data.frame(shares.no.cap)
    
    ## Make the plot
    generate.plot(price.df,        platforms, outpath.lvl)
    generate.plot(price.cap.df,    platforms, outpath.lvl.cap)
    generate.plot(price.no.cap.df, platforms, outpath.lvl.no.cap)
    
    generate.plot(share.df,         platforms, outpath.shr,        use.shares = TRUE)
    generate.plot(shares.cap.df,    platforms, outpath.shr.cap,    use.shares = TRUE)
    generate.plot(shares.no.cap.df, platforms, outpath.shr.no.cap, use.shares = TRUE)
}

compute.price.levels.and.shares <- function(months, df, platform){
    # Compute the average price levels and price structure shares
    # for restaurants and consumers on a given platform
    
    ## Price levels
    prices.f <- lapply(months, function(month) mean.prices.per.month(month, df, platform))
    prices.f <- as.data.frame(Reduce(rbind, prices.f))
    prices.f$month <- months
    ## Price as share of order value
    shares.f <- lapply(months, function(month) mean.prices.per.month(month, df, platform,
                                                                               use.shares = TRUE))
    shares.f <- as.data.frame(Reduce(rbind, shares.f))
    shares.f$month <- months
    
    return(list(prices.f = prices.f,
                shares.f = shares.f))
}

mean.prices.per.month <- function(month, df, platform, use.shares = FALSE){

    df.sub <- df[which(df$month == month), ]
    df.sub <- df.sub[which(df.sub$merchant_name == platform), ]
    df.sub$p_c <- df.sub$avg_service_fee + df.sub$avg_delivery_fee - df.sub$avg_order_discount
    df.sub$p_r <- df.sub$cap
    if (use.shares & nrow(df.sub) == 0){
        df.sub <- as.data.frame(t(as.data.frame(c(orders_scaled = 1, p_c = 0, p_r = 0.30),
                                                nrow = 1)))
    } else if (use.shares){
        df.sub$p_c <- pmax(pmin(df.sub$p_c/df.sub$aov_feesandtips_excluded, 1.0),  0)
        df.sub$p_r <- df.sub$cap
    } else {
        df.sub$p_r <- df.sub$cap*df.sub$aov_feesandtips_excluded # What about in places with fee caps?
    }

    # Compute average across ZCTAs
    weights <- df.sub$orders_scaled/sum(df.sub$orders_scaled)
    mean_p_c <- sum(weights*df.sub$p_c, na.rm = TRUE)
    mean_p_r <- sum(weights*df.sub$p_r, na.rm = TRUE)
    return(c(p_c = mean_p_c, p_r = mean_p_r))
}


combine.data.frame <- function(prices){
    ## Combine the data.frames
    platforms <- names(prices)
    platform.labels <- tolower(sub(' ', '', platforms))
    names(platform.labels) <- platforms
    for (platform in platforms){
        f <- tolower(sub(' ', '', platform))
        colnames(prices[[platform]]) <- sub('p_', paste0(f, '_p_'), colnames(prices[[platform]]))
    }
    price.df <- Reduce(function(x, y) dplyr::inner_join(x, y, by = 'month'), prices)
    price.df <- price.df[which(price.df$month != '2021-05-01'), ]
    price.df <- price.df[, !grepl('postmates', colnames(price.df))]

    return(price.df)
}


generate.plot <- function(price.df, platforms, outpath.plot, use.shares = FALSE){
    cols <- brewer.pal(4, 'Dark2')
    pchs <- c(3, 4, 8, 20)

    platform.labels <- tolower(sub(' ', '', platforms))

    if (use.shares){
        ylim.max <- 0.40
        hline.factor <- 0.1
        axis.step <- ylim.max/0.1
    } else {
        ylim.max <- 10
        hline.factor <- 1
        axis.step <- ylim.max/2
    }
    ylim <- c(0, ylim.max)

    price.df$month <- as.Date(price.df$month)
    price.df <- price.df[order(price.df$month), ]

    fig.h <- 6.2
    fig.w <- 6.4
    
    if (use.shares){
        ylab <- 'Share of transaction subtotal'
    } else {
        ylab <- 'Average price per transaction'
    }

    pdf(outpath.plot, height = fig.h, width = fig.w)
    for (f in 1:length(platforms)){
        p.c.var <- paste0(platform.labels[f], '_p_c')
        p.r.var <- paste0(platform.labels[f], '_p_r')

        if (f == 1){
            plot(x = price.df$month, y = price.df[[p.c.var]], type = 'l', col = cols[f],
                 xlab = 'Month', ylab = ylab, ylim = ylim,
                 axes = FALSE, cex.lab = 1.5)
            points(price.df$month, price.df[[p.c.var]], pch = pchs[f], col = cols[f])
            axis(1, price.df$month, format(price.df$month, "%Y-%m"), cex.axis = 1.1)
            for (m in price.df$month){
                abline(v = m, col = 'grey85', lty = 2)
            }
            axis(2, yaxp = c(0, ylim.max, axis.step), cex.axis = 1.3)
            for (k in 1:ceiling(ylim.max/hline.factor)){
                abline(h = k*hline.factor, col = 'grey85', lty = 2)
            }
        } else {
            lines(x = price.df$month, y = price.df[[p.c.var]], col = cols[f])
            points(x = price.df$month, y = price.df[[p.c.var]],
                   col = cols[f], pch = pchs[f])
        }

        lines(x = price.df$month, y = price.df[[p.r.var]], lty = 2,
              col = cols[f], pch = pchs[f])
        points(x = price.df$month, y = price.df[[p.r.var]],
               col = cols[f], pch = pchs[f])
    }
    legend(x = 'bottomright', legend = c('DoorDash (Restaurant)', 'DoorDash (Consumer)',
                                         'Uber Eats (Restaurant)', 'Uber Eats (Consumer)',
                                         'Grubhub (Restaurant)', 'Grubhub (Consumer)'),
           box.lwd = , box.col = "white", bg = "white",
           col = c(cols[1], cols[1], cols[2], cols[2], cols[3], cols[3]),
           pch = c(pchs[1], pchs[1], pchs[2], pchs[2], pchs[3], pchs[3]),
           lty = c(2, 1, 2, 1, 2, 1))
    dev.off()
}


main()
