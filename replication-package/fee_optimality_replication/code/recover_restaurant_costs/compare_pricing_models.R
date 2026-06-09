# Compare predictions of pricing models

library(Matrix)
library(EconTools)
library(FoodDeliveryTools)

# Load data
nsim.suffix <- '_nsim50'
inpaths <- collect.inpaths(nsim.suffix)
dat <- readRDS(inpaths$inpath.dat)
rMC <- readRDS(inpaths$inpath.rMC)
NPP <- readRDS(inpaths$inpath.NPP)

outpath <- 'output/recover_restaurant_costs/compare_pricing_models.csv'

opts <- load.opts(verbose = TRUE)

num.param <- load.num.param()
# Solve both pricing models (preferred and NPP) with the same tolerance and
# learning rate, so the model comparison is not confounded by solver precision.
num.param$learn.rate.menu <- 0.25

markets <- names(dat)

r.grid <- c(0.30, 0.15)
ngrid <- length(r.grid)

# Initialize outputs for V2 model
RHO <- list()
W   <- list()

# Initialize outputs for NPP model
RHO.NPP <- list()
W.NPP   <- list()

for (market in markets){
    print(market)
    dat.m <- dat[[market]]

    # Commission-accounting parameter (vartheta)
    dat.m$phi <- rMC$phi

    # Marginal costs
    mc.on  <- rMC$costs[[market]]$costs.on
    mc.off <- rMC$costs[[market]]$costs.off
    mc.m <- list(on = mc.on, off = mc.off)
    opts$resto.param$mc <- mc.m

    RHO[[market]] <- list() # Prices
    W[[market]]   <- list() # Weights
    for (k in 1:ngrid){
        for (z in names(dat.m$comm)){
            dat.m$comm[[z]][] <- r.grid[k]
        }
        Rhos.k <- menu.price.eqm(dat.m, mc.m, num.param, verbose = TRUE)

        if (r.grid[k] == 0.30){
            sales.dat <- compute.zip.sales(dat.m, Rhos = Rhos.k, opts = opts, more.outputs = TRUE)
        }
        rho.k <- weight.rhos.v2(sales.dat$Sales.tots, Rhos.k, dat.m)
        NF <- ncol(rho.k)

        rho.bar <- c()
        w <- c()
        for (f in 1:NF){
            s.f <- sapply(rownames(rho.k), function(z) sum(sales.dat$S.tots[[z]][, f, ]))
            rho.bar[f] <- weighted.mean(rho.k[, f], s.f)
            w[f] <- sum(s.f)
        }
        RHO[[market]][[k]] <- rho.bar
        W[[market]][[k]] <- w
    }

    # Now solve for NPP
    phi2   <- NPP$phi
    mc.on  <- NPP$costs[[market]]$costs.on
    mc.off <- NPP$costs[[market]]$costs.off
    mc.m <- list(on = mc.on, off = mc.off)
    opts$resto.param$mc <- mc.m

    dat.m$phi2 <- phi2

    RHO.NPP[[market]] <- list() # Prices
    W.NPP[[market]]   <- list() # Weights

    for (k in 1:ngrid){
        for (z in names(dat.m$comm)){
            dat.m$comm[[z]][] <- r.grid[k]
        }
        Rhos.k <- menu.price.eqm.NPP(dat.m, mc.m, num.param, verbose = TRUE)

        if (r.grid[k] == 0.30){
            sales.dat <- compute.zip.sales(dat.m, Rhos = Rhos.k, opts = opts, more.outputs = TRUE)
        }
        rho.k <- weight.rhos.v2(sales.dat$Sales.tots, Rhos.k, dat.m)
        NF <- ncol(rho.k)

        rho.bar <- c()
        w <- c()
        for (f in 1:NF){
            s.f <- sapply(rownames(rho.k), function(z) sum(sales.dat$S.tots[[z]][, f, ]))
            rho.bar[f] <- weighted.mean(rho.k[, f], s.f)
            w[f] <- sum(s.f)
        }
        RHO.NPP[[market]][[k]] <- rho.bar
        W.NPP[[market]][[k]] <- w
    }
}

# Compare the results

## First, choose one market
market <- markets[1]
mat.V2  <- do.call(rbind, RHO[[market]])
mat.NPP <- do.call(rbind, RHO.NPP[[market]])

RHO.mat   <- lapply(RHO,     function(x) do.call(rbind, x))
NPP.mat   <- lapply(RHO.NPP, function(x) do.call(rbind, x))
W.mat     <- lapply(W,       function(x) do.call(rbind, x))
W.NPP.mat <- lapply(W.NPP,   function(x) do.call(rbind, x))

W.tot     <- Reduce('+', W.mat)
W.NPP.tot <- Reduce('+', W.NPP.mat)

RHO.agg <- Reduce('+', lapply(markets, function(m) RHO.mat[[m]]*W.mat[[m]]))/W.tot
NPP.agg <- Reduce('+', lapply(markets, function(m) NPP.mat[[m]]*W.NPP.mat[[m]]))/W.tot

RHO.chg <- (RHO.agg[2, ]/RHO.agg[1, ] - 1)*100
NPP.chg <- (NPP.agg[2, ]/NPP.agg[1, ] - 1)*100

tab <- data.frame(platform = c('Direct', 'DoorDash', 'Uber Eats', 'Grubhub', 'Postmates'),
                  baseline = sprintf('%0.2f', RHO.chg),
                  npp      = sprintf('%0.2f', NPP.chg))


write.dat(tab, outpath)

