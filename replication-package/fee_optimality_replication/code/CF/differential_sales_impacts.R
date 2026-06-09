# Measure differences in gains from joining platforms across restaurants
# and variation in restaurants' platform markups
library(Matrix)
library(EconTools)
library(FoodDeliveryTools)


nsim.suffix <- '_nsim50'

outdir <- sprintf('output/CF_feefirst/analysis%s', nsim.suffix)
create.dir(outdir)
outpath.sales <- sprintf('%s/sales_gains_platform_adopt.csv', outdir)
outpath.markups <- sprintf('%s/restaurant_gross_markups.pdf', outdir)

# Step 1: load the results
# Load cbsa codes

inpaths    <- collect.inpaths(nsim.suffix)
cbsa.codes <- read.dat(inpaths$inpath.cbsa.codes)
geo <- load.geo(inpaths$inpath.geo)
geo <- geo[geo$is.zcta, ]
FC.est <- readRDS(inpaths$inpath.FC.est)
load.pMC <- TRUE
eqm.objs <- load.all.eqm.objs.feefirst(nsim.suffix, load.pMC)

# Step 2: load the baseline equilibrium, including sales
base.dir <- sprintf('output/CF_feefirst/spec%s', nsim.suffix)
suffix <- '_take2'
BL.pattern <- 'cap30'
BL <- load.eqm.results(BL.pattern, base.dir, cbsa.codes, suffix = suffix)

# Step 3: compute change in sales from joining DoorDash
markets <- names(BL)

Tot.sales <- list()
Wgts <- list()

for (market in markets){
    eqm.objs.m <- eqm.objs$county[[market]]
    BL.m <- BL[[market]]
    counties <- names(BL.m)


    Tot.sales.m <- list()

    for (co in counties){

        eqm <- BL.m[[co]]

        opts <- eqm.objs[[market]][[co]]
        eqm.objs.j <- eqm.objs.m[[co]]
        dat.m <- eqm.objs.j$dat.m
        for (z in names(dat.m$fees)){
            dat.m$fees[[z]] <- eqm$C
        }
        for (z in names(dat.m$comm)){
            dat.m$comm[[z]] <- eqm$R
        }

        opts <- eqm.objs.j$opts
        Rhos <- eqm$FP$Rhos

        # Compute sales in each ZIP
        Sales.mats <- compute.zip.sales(dat.m, Rhos = Rhos, opts = opts)

        zips <- names(dat.m$J.G.1.m)

        zip.sales <- list()
        tot.sales <- list()
        zip.map <- dat.m$zip.map.1
        for (z in zips){
            # Determine which zip codes are in range
            Z.z <- zip.map[[z]]
            Z.z0 <- sub('[ci]$', '', Z.z)
            Z.z0 <- intersect(Z.z0, names(Sales.mats))
            Sales.j <- Reduce('+', lapply(Z.z0, function(z.z) Sales.mats[[z.z]][, , z]))
            zip.sales[[z]] <- Sales.j
            tot.sales[[z]] <- rowSums(Sales.j)
        }
        tot.sales <- do.call(rbind, tot.sales)
        rownames(tot.sales) <- zips
        Tot.sales.m[[co]] <- tot.sales
    }

    Tot.sales[[market]] <- do.call(rbind, Tot.sales.m)
    Wgts[[market]] <- sapply(rownames(Tot.sales[[market]]), function(z) sum(dat.m$J.G.1.m[[z]]))
}


# Next, compute the mean and standard deviation of the ratio within each market
Q <- c(0.25, 0.75)
mu <- c()
IQR <- list()
SD <- c()
for (market in markets){
    S <- Tot.sales[[market]]
    ratio <- S[, 16]/S[, 1]
    wgt.m <- Wgts[[market]]
    mu[market] <- weighted.mean(ratio, wgt.m)
    SD[market] <- sqrt(weighted.mean((ratio - mu[market])^2, wgt.m))
    IQR[[market]] <- sapply(Q, function(q) weighted.quantile(ratio, q, wgt.m))
}

metro <- sub('\\-.*$', '', markets)
tab <- data.frame(metro = metro,
                  means = sprintf('%0.2f', (mu - 1)*100),
                  stdevs = sprintf('%0.2f', SD*100))


write.dat(tab, outpath.sales)


# begin with portfolio 16, and then cmpute for other portfolios

# Also, compute restaurant markups by ZIPS and types
Markups <- list()
Wgts    <- list()
for (market in markets){
    eqm.objs.m <- eqm.objs$county[[market]]
    BL.m <- BL[[market]]
    counties <- names(BL.m)

    Markups[[market]] <- list()
    Wgts[[market]] <- list()
    for (co in counties){
        eqm <- BL.m[[co]]
        opts <- eqm.objs[[market]][[co]]
        eqm.objs.j <- eqm.objs.m[[co]]
        dat.m <- eqm.objs.j$dat.m
        for (z in names(dat.m$fees)){
            dat.m$fees[[z]] <- eqm$C
        }
        for (z in names(dat.m$comm)){
            dat.m$comm[[z]] <- eqm$R
        }

        opts <- eqm.objs.j$opts
        Rhos <- eqm$FP$Rhos

        Rhos <- eqm$FP$Rhos
        mc.on <- opts$resto.param$mc$on

        markup.co <- list()
        wgt.co    <- list()
        for (g in 2:16){
            markup.co[[g]] <- list()
            wgt.g <- c()
            for (k in names(mc.on)){
                rhos.kg <- Rhos[[k]][[g]]
                markup.co[[g]][[k]] <- rhos.kg[2:length(rhos.kg)] - mc.on[k]
                wgt.g[k] <- dat.m$J.G.1.m[[k]][g]
            }
            wgt.co[[g]] <- wgt.g
        }

        # Compute the markup on each platform for each restaurant
        markups.by.f <- list()
        weights.by.f <- list()
        for (f in 1:4){
            m.f <- c()
            w.f <- c()

            for (g in 2:16){

                f.in.g <- dat.m$G.mat[g, f + 1] == 1
                if (!f.in.g){
                    next
                }
                idx <- dat.m$fg.indices[[g]][[f + 1]] - 1
                for (k in names(mc.on)){
                    m.f <- c(m.f, markup.co[[g]][[k]][idx])
                    w.f <- c(w.f, wgt.co[[g]][k])
                }
            }

            markups.by.f[[f]] <- m.f
            weights.by.f[[f]] <- w.f
        }

        Markups[[market]][[co]] <- markups.by.f
        Wgts[[market]][[co]]    <- weights.by.f
    }
}

# Piece together for each platform
Markups.by.f <- list()
Wgts.by.f    <- list()

for (f in 1:4){
    Markups.by.f[[f]] <- list()
    Wgts.by.f[[f]]    <- list()
    for (market in markets){
        counties <- names(Markups[[market]])
        for (co in counties){
            Markups.by.f[[f]][[co]] <- Markups[[market]][[co]][[f]]
            Wgts.by.f[[f]][[co]]    <- Wgts[[market]][[co]][[f]]
        }
    }
    Markups.by.f[[f]] <- do.call(c, Markups.by.f[[f]])
    Wgts.by.f[[f]]    <- do.call(c, Wgts.by.f[[f]])
}

# Make a similar plot for platform markups
means <- c()
stdevs <- c()
IQR <- list()
Q <- c(0.05, 0.25, 0.5, 0.75, 0.95)
for (f in 1:4){
    x <- Markups.by.f[[f]]
    w <- Wgts.by.f[[f]]
    means[f] <- weighted.mean(x, w)
    stdevs[f] <- sqrt(weighted.mean((x - means[f])^2, w))
    IQR[[f]] <- sapply(Q, function(q) weighted.quantile(x, q, w))
}

nbar <- 4
combo <- Reduce(c, IQR)
ylim <- c(min(combo)*0.98, max(combo)*1.02)


colour <- 'slategray'
pdf(outpath.markups, width = 8, height = 5)
plot(1:nbar, -100*rep(1, times = nbar), ylim = ylim, xlim = c(0.5, nbar + 0.5),
     ylab = 'Gross restaurant markups ($)', axes = FALSE,
     xlab = '')
grid()
abline(h = 0, col = 'grey20')
for (f in 1:4){
    make.IQR.bar(f, IQR[[f]], colour = colour)
}

axis(2)
axis(1, at = 1:4, labels = c('DoorDash', 'Uber Eats',
                             'Grubhub', 'Postmates'))
dev.off()


