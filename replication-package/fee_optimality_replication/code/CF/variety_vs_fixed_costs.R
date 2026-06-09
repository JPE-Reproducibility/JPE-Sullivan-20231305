# Produce a plot comparing the fixed costs and variety benefits of
# commission reductions

library(Matrix)
library(EconTools)
library(FoodDeliveryTools)

source('code/CF/welfare_calculations.R')


cap.levels <- 15:30
nsim.suffix <- '_nsim50'

# Specify paths
outdir <- sprintf('output/CF_feefirst/analysis%s', nsim.suffix)
outpath.by.level <- sprintf('%s/variety_vs_fixed_costs_by_cap_level.pdf', outdir)

# Load cbsa codes
inpaths    <- collect.inpaths(nsim.suffix)
cbsa.codes <- read.dat(inpaths$inpath.cbsa.codes)

geo <- load.geo(inpaths$inpath.geo)
geo <- geo[geo$is.zcta, ]

# Load fixed cost estimates
FC.est <- readRDS(inpaths$inpath.FC.est)

# Load eqm objects
load.pMC <- TRUE
eqm.objs <- load.all.eqm.objs.feefirst(nsim.suffix, load.pMC)

eqm.objs.co <- eqm.objs$county
markets <- names(eqm.objs.co)

# Specify directory containing results
base.dir <- sprintf('output/CF_feefirst/spec%s', nsim.suffix)

# Load results
suffix <- '_take2'
BL.pattern <- 'cap30'
BL <- load.eqm.results(BL.pattern, base.dir, cbsa.codes, suffix = suffix)
Cap <- list()
for (k in 1:length(cap.levels)){
    lvl <- cap.levels[k]
    cap.pattern <- sprintf('cap%d', lvl)
    Cap[[k]] <- load.eqm.results(cap.pattern, base.dir, cbsa.codes, suffix = suffix)
}

# Select alternative consumer fees under which to compute variety benefits
cap.idx <- which(cap.levels == 15)
Cap.select <- Cap[[cap.idx]]
# Compute baseline FC and Variety
markets <- names(BL)
FC.BL      <- c()
Variety.BL <- c()
Variety.hi <- c()



for (market in markets){
    counties <- names(BL[[market]])
    for (co in counties){
        dat.m <- eqm.objs.co[[market]][[co]]$dat.m
        kappa <- eqm.objs.co[[market]][[co]]$kappa
        # Change in fixed costs from a commission reduction
        J0 <- BL[[market]][[co]]$FP$J.G.1
        FC0 <- sum(sapply(names(J0), function(z) sum(J0[[z]]*kappa$K[[z]])))
        FC.BL[co] <- FC0

        # Change in variety benefit
        C    <- BL[[market]][[co]]$C
        Rhos <- BL[[market]][[co]]$FP$Rhos
        W0 <- compute.consumer.welfare(dat.m, C, J0, Rhos, no.logit = FALSE)
        Variety.BL[co] <- W0$total.EU.dollar

        C.hi <- Cap.select[[market]][[co]]$C
        Rhos <- Cap.select[[market]][[co]]$FP$Rhos
        W0 <- compute.consumer.welfare(dat.m, C.hi, J0, Rhos, no.logit = FALSE)
        Variety.hi[co] <- W0$total.EU.dollar
    }
}

FC.cap         <- list()
Variety.cap    <- list()
Variety.hi.cap <- list()

for (k in 1:length(cap.levels)){

    FC.cap.k         <- c()
    Variety.cap.k    <- c()
    Variety.hi.cap.k <- c()

    for (market in markets){
        counties <- names(BL[[market]])
        for (co in counties){
            dat.m <- eqm.objs.co[[market]][[co]]$dat.m
            kappa <- eqm.objs.co[[market]][[co]]$kappa

            # Change in fixed costs from a commission reduction
            J1 <- Cap[[k]][[market]][[co]]$FP$J.G.1
            FC1 <- sum(sapply(names(J1), function(z) sum(J1[[z]]*kappa$K[[z]])))

            # Change in variety benefit
            C    <- BL[[market]][[co]]$C
            Rhos <- BL[[market]][[co]]$FP$Rhos
            W1 <- compute.consumer.welfare(dat.m, C, J1, Rhos, no.logit = FALSE)
            W1 <- W1$total.EU.dollar

            FC.cap.k[co]      <- FC1
            Variety.cap.k[co] <- W1

            # Higher fees
            C.hi <- Cap.select[[market]][[co]]$C
            Rhos <- Cap.select[[market]][[co]]$FP$Rhos
            W1 <- compute.consumer.welfare(dat.m, C.hi, J1, Rhos, no.logit = FALSE)
            W1 <- W1$total.EU.dollar
            Variety.hi.cap.k[co] <- W1
        }
    }
    FC.cap[[k]]         <- FC.cap.k
    Variety.cap[[k]]    <- Variety.cap.k
    Variety.hi.cap[[k]] <- Variety.hi.cap.k
}

# Normalization factor
S.by.co <- c()
for (market in markets){
    counties <- names(BL[[market]])
    for (co in counties){
        S.by.co[co] <- sum(BL[[market]][[co]]$FP$Sales.platform[2:5])
    }
}
S.tot <- sum(S.by.co)




## Total change for each commission level
FC.chg   <- sapply(FC.cap, sum) - sum(FC.BL)
V.tot    <- sapply(Variety.cap, sum) - sum(Variety.BL)
V.hi.tot <- sapply(Variety.hi.cap, sum) - sum(Variety.hi)

FC.chg   <- c(FC.chg, 0)
V.tot    <- c(V.tot,  0)
V.hi.tot <- c(V.hi.tot,  0)
cap.levels <- c(cap.levels, 30)

pdf(outpath.by.level, height = 4.5, width = 6)
par(mar = c(5, 5, 1, 1))
plot(cap.levels, V.tot/S.tot, type = 'l', xlim = c(15, 30),
     axes = FALSE, ylim = c(0, 1.35),
     ylab = 'Welfare change ($/baseline platform orders)',
     xlab = 'Regulated commission level (%)', lwd = 1.5)
grid()
lines(cap.levels, FC.chg/S.tot, lty = 2, lwd = 2, col = 'firebrick')

lines(cap.levels, V.hi.tot/S.tot, lty = 3, lwd = 2, col = 'grey30')


abline(v = 30, lty = 3, lwd = 2)
abline(h = 0, lty = 1)
axis(1)
axis(2)
legend('topright',
       legend = c('Variety benefits (baseline consumer fees)',
                  'Variety benefits (high consumer fees)',
                  'Fixed costs'),
       lty = c(1, 3, 2), col = c('black', 'grey30', 'firebrick'),
       lwd = 2, bg = 'white')

dev.off()

