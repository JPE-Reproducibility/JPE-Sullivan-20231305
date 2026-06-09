library(Matrix)
library(FoodDeliveryTools)
library(EconTools)

# Compare restaurant side distortions
nsim.suffix <- '_nsim50'
inpaths <- collect.inpaths(nsim.suffix)

num.param <- load.num.param()
num.param$tol.FP <- 1e-8

# Directory
CF.dir <- sprintf('output/CF_feefirst/spec%s', nsim.suffix)
outdir <- sprintf('output/CF_feefirst/analysis%s', nsim.suffix)
outpath <- sprintf('%s/social_benefits_from_commission_reduction.pdf', outdir)

## For b.tilde vs b.bar
outpath.scatter   <- sprintf('%s/bar_versus_tilde_scatter.pdf', outdir)
outpath.no.weight <- sprintf('%s/bar_versus_tilde_scatter_no_weight.pdf', outdir)
outpath.IQR       <- sprintf('%s/bar_tilde_IQRs.pdf', outdir)
outpath.IQR.soc   <- sprintf('%s/bar_tilde_IQRs_soc.pdf', outdir)
outpath.dat       <- sprintf('%s/bar_tilde_data.csv', outdir)
outpath.J         <- sprintf('%s/correlate_with_J_effect.pdf', outdir)

cbsa.codes <- read.dat(inpaths$inpath.cbsa.codes)
rownames(cbsa.codes) <- cbsa.codes$cbsa
# Load eqm objects
load.pMC <- TRUE
eqm.objs <- load.all.eqm.objs.feefirst(nsim.suffix, load.pMC)
eqm.objs.co <- eqm.objs$county

# Load competitive equilibria
eqm.comp <- load.equilibria.priv.soc(CF.dir, cbsa.codes, monopoly = FALSE)
BL  <- eqm.comp$BL
soc <- eqm.comp$soc

markets <- names(eqm.objs.co)
assert.complete.names(names(BL), markets,
                      what = sprintf('baseline/socopt equilibria in %s (markets)', CF.dir),
                      hint = 'Re-run the CF solvers for this spec.')

# Initialize outputs
chg.consumer     <- c()
chg.price.c      <- c()
chg.variety      <- c()
chg.envelope     <- c()
chg.price.r      <- c()
chg.tot          <- c()
chg.other        <- c()
chg.FC           <- c()
chg.profit.S     <- c()
chg.profit.mkp   <- c()
chg.profit.rival <- c()

h <- 0.01 # Step size
f <- 1 # Platform of interest

for (market in markets){
    eqm.objs.m <- eqm.objs.co[[market]]
    counties <- names(eqm.objs.m)

    for (co in counties){

        eqm.objs.j <- eqm.objs.m[[co]]
        dat.m <- eqm.objs.j$dat.m
        kappa <- eqm.objs.j$kappa
        opts  <- eqm.objs.j$opts
        opts$verbose <- FALSE
        pMC.df <- eqm.objs.j$pMC.df

        ## Extract equilibrium fees under profit maximization
        eqm <- BL[[market]][[co]]
        C <- eqm$C
        R <- eqm$R

        # Compute changes in platform profits under changes to commissions
        R1 <- R
        R1[f] <- R1[f] - h

        # Load in fees and commissions
        dat.m1 <- dat.m
        for (z in names(dat.m$fees)){
            dat.m$fees[[z]]  <- C
            dat.m1$fees[[z]] <- C
        }
        for (z in names(dat.m$comm)){
            dat.m$comm[[z]]  <- R
            dat.m1$comm[[z]] <- R1
        }
        # Compare platform's sales change and reduction of margin on existing sales
        FP0 <- find.fixed.point(kappa, dat.m, opts, num.param, more.outputs = TRUE)
        FP1 <- find.fixed.point(kappa, dat.m1, opts, num.param, more.outputs = TRUE)
        pp  <- platform.profits(FP0, C, R,  kappa, dat.m, pMC.df, opts, num.param)[f]
        pp1 <- platform.profits(FP1, C, R1, kappa, dat.m, pMC.df, opts, num.param)[f]
        # Change in DoorDash profits from increased sales
        S  <- FP0$Sales.platform[f + 1]
        S1 <- FP1$Sales.platform[f + 1]
        profit.per.sale <- pp[f]/S
        chg.S <- FP1$Sales.platform[f + 1] - FP0$Sales.platform[f + 1]
        chg.profit.S[co] <- chg.S*profit.per.sale
        # Change in DoorDash profits from reduced margin
        profit.per.sale1 <- pp1[f]/S1
        chg.profit.mkp[co] <- (profit.per.sale1 - profit.per.sale)*S

        # Change in rival platforms' profits
        chg.pp.f     <- chg.profit.S[co] + chg.profit.mkp[co]
        chg.pp.rival <- sum(pp1 - pp) - chg.pp.f
        chg.profit.rival[co] <- chg.pp.rival

        # Change in consumer variety benefits
        CW0 <- compute.consumer.welfare(dat.m, C, FP0$J.G.1, FP0$Rhos,
                                        no.logit = FALSE)$total.EU.dollar
        CW1 <- compute.consumer.welfare(dat.m, C, FP1$J.G.1, FP0$Rhos,
                                        no.logit = FALSE)$total.EU.dollar
        CW2 <- compute.consumer.welfare(dat.m, C, FP1$J.G.1, FP1$Rhos,
                                        no.logit = FALSE)$total.EU.dollar
        chg.variety[co]  <- CW1 - CW0
        chg.consumer[co] <- CW2 - CW0
        ## Change in fixed costs
        FCs <- c()
        for (z in names(FP0$J.G.1)){
            J1 <- FP1$J.G.1[[z]]
            J0 <- FP0$J.G.1[[z]]
            Jdiff <- J1 - J0
            FCs[z] <- sum(Jdiff*kappa$K[[z]])
        }
        chg.FC[co] <- sum(FCs)

        # Compute direct benefits of commission reduction
        pi.BL     <- compute.rpi(dat.m, C, R,  FP0$J.G.1, kappa, FP0$Rhos, opts)['total']
        pi.p      <- compute.rpi(dat.m, C, R,  FP0$J.G.1, kappa, FP1$Rhos, opts)['total']
        pi.direct <- compute.rpi(dat.m, C, R1, FP0$J.G.1, kappa, FP0$Rhos, opts)['total']
        pi.tot    <- compute.rpi(dat.m, C, R1, FP1$J.G.1, kappa, FP1$Rhos, opts)['total']

        chg.envelope[co] <- pi.direct - pi.BL
        chg.price.r[co]  <- pi.p      - pi.BL
        chg.tot[co]      <- pi.tot    - pi.BL
        chg.other[co]    <- chg.tot[co] - chg.envelope[co]
    }
}

S.BL <- c()
R.priv <- c()
R.soc  <- c()
for (market in markets){
    eqm.objs.m <- eqm.objs.co[[market]]
    counties <- names(eqm.objs.m)
    counties <- intersect(counties, names(chg.other))
    for (co in counties){
        S.BL[co] <- sum(BL[[market]][[co]]$FP$Sales.platform[2])
        R.priv[co] <- BL[[market]][[co]]$R[f]
        R.soc[co]  <- soc[[market]][[co]]$R[f]
    }
}

# Gaps between socially and privately optimal
gap <- R.soc - R.priv

# Welfare change due to price reduction

rel.profit.S     <- chg.profit.S/S.BL
rel.profit.mkp   <- chg.profit.mkp/S.BL
rel.profit.rival <- chg.profit.rival/S.BL

rel.envelope <- chg.envelope/S.BL
rel.tot      <- chg.tot/S.BL
rel.other    <- chg.other/S.BL

rel.variety  <- chg.variety/S.BL
rel.consumer <- chg.consumer/S.BL

rel.effect <- rel.consumer + rel.tot

rel.FC <- chg.FC/S.BL

effects <- c(mean(rel.profit.S), mean(rel.profit.mkp), mean(rel.profit.rival),
             mean(rel.envelope), mean(rel.other),
             mean(rel.variety), mean(rel.consumer) - mean(rel.variety))
effects['total'] <- sum(effects)
colours <- wesanderson::wes_palette('Cavalcanti1', 4)
colours <-  c('grey30', colours[c(1, 1, 2, 2, 3, 3, 3)])

pdf(outpath, width = 5.5, height = 4.5)
par(mar = c(5, 14, 2, 2))
barplot(rev(effects), horiz = TRUE, col = colours,
        names.arg = c('Total', 'Consumers: price benefits', 'Consumers: variety benefits',
                      'Restaurants: competition effects',
                      'Restaurants: direct benefit',
                      'Rival platforms: sales effect',
                      'DoorDash: markup effect',
                      'DoorDash: sales effect'), las = 2,
        xlab = 'Welfare effect ($/order)',
        xlim = c(-0.42, 0.42))
grid()
abline(v = 0, col = 'grey20')
barplot(rev(effects), horiz = TRUE, col = colours,
        names.arg = rep('', times = length(effects)),
        xlab = '', add = TRUE, las = 2)
dev.off()

# Assess variability?
hist(rel.consumer)

EQM <- BL

# Now, explicitly compute b-tilde and b-bar
compute.variety.benefits <- function(EQM, eqm.objs.co, num.param){

    # Initailize outputs
    b.bar   <- c()
    b.tilde <- c()
    J.pre  <- list()
    J.post <- list()
    pop <- c()

    h <- 0.01
    
    num.param$tol.FP <- 5e-5

    markets <- names(eqm.objs.co)
    for (market in markets){
        eqm.objs.m <- eqm.objs.co[[market]]
        counties <- names(eqm.objs.m)
        for (co in counties){
            eqm.objs.j <- eqm.objs.m[[co]]
            dat.m <- eqm.objs.j$dat.m
            kappa <- eqm.objs.j$kappa
            opts  <- eqm.objs.j$opts
            opts$verbose <- FALSE
            pMC.df <- eqm.objs.j$pMC.df

            ## Extract equilibrium fees under profit maximization
            eqm <- EQM[[market]][[co]]
            C <- eqm$C
            R <- eqm$R

            R1 <- R
            R1[f] <- R1[f] - h
            for (z in names(dat.m$fees)){
                dat.m$fees[[z]] <- C
            }
            for (z in names(dat.m$comm)){
                dat.m$comm[[z]] <- R1
            }
            for (z in names(eqm$FP$J.G.1)){
                dat.m$J.G.1.m[[z]] <- eqm$FP$J.G.1[[z]]
            }

            FP1 <- find.fixed.point(kappa, dat.m, opts, num.param, more.outputs = TRUE)

            # Record restaurant change
            J.pre[[co]]  <- eqm$FP$J.G.1
            J.post[[co]] <- FP1$J.G.1

            W0 <- compute.consumer.welfare(dat.m, C, eqm$FP$J.G.1, Rhos = eqm$FP$Rhos, no.logit = FALSE)
            W1 <- compute.consumer.welfare(dat.m, C, FP1$J.G.1, Rhos = eqm$FP$Rhos, no.logit = FALSE)
            dW <- (W1$total.EU.dollar - W0$total.EU.dollar)
            b.bar[co] <- dW/eqm$FP$Sales.platform[f + 1]

            S0 <- compute.sales(eqm, dat.m)[f]
            eqm1 <- eqm
            eqm1$FP <- FP1
            eqm1$FP$Rhos <- eqm$FP$Rhos
            S1 <- compute.sales(eqm1, dat.m)[f]

            C.grid <- seq(from = 0, to = 0.50, by = 0.005)
            S.p <- c()
            for (k in 1:length(C.grid)){
                eqm1$C[f] <- C[f] + C.grid[k]
                S.p[k] <- compute.sales(eqm1, dat.m)[f]
            }
            b.tilde[co] <- C.grid[which.min(abs(S.p - S0))]

            # Population
            pop[co] <- sum(eqm.objs.co[[market]][[co]]$dat.m$buy.m$count)
        }
    }

    # Compute change in DoorDash uptake
    J.chg.rel <- c()
    J.chg.pc  <- c()
    G.mat <- eqm.objs.co[[1]][[1]]$dat.m$G.mat
    idx.f <- which(G.mat[, f + 1] == 1)
    for (co in names(J.pre)){
        J.pre.co  <- colSums(Reduce(rbind, J.pre[[co]]))
        J.post.co <- colSums(Reduce(rbind, J.post[[co]]))
        ## First, relative increase in restaurants on the platform
        J.chg.rel[co] <- sum(J.post.co[idx.f])/sum(J.pre.co[idx.f]) - 1
        ## Second, increase per capita
        J.chg.pc[co] <- sum(J.post.co[idx.f] - J.pre.co[idx.f])/pop[co]
    }

    outputs <- list()
    outputs$b.bar     <- b.bar
    outputs$b.tilde   <- b.tilde
    outputs$J.pre     <- J.pre
    outputs$J.post    <- J.post
    outputs$J.chg.rel <- J.chg.rel
    outputs$J.chg.pc  <- J.chg.pc

    return(outputs)
}

benefits.priv <- compute.variety.benefits(BL, eqm.objs.co, num.param)
benefits.soc  <- compute.variety.benefits(soc, eqm.objs.co, num.param)

# County sizes (population)
pop <- c()
for (market in markets){
    counties <- names(eqm.objs.co[[market]])
    for (co in counties){
        pop[co] <- sum(eqm.objs.co[[market]][[co]]$dat.m$buy.m$count)
    }
}

# Several things to save:
## Scatter plot
make.scatter <- function(x, y, weights = NULL){
    if (is.null(weights)){
        cex <- 1
    } else {
        cex <- weights
    }
    ymax <- max(c(x, y))
    par(mar = c(5.5, 5, 2, 2))
    plot(b.tilde, b.bar, pch = 8, ylim = c(0, ymax), xlim = c(0, ymax),
         axes = FALSE,
         xlab = 'Marginal benefit ($)',
         ylab = 'Average benefit ($)',
         cex = weights)
    grid()
    abline(0, 1, lty = 2, lwd = 1.5)
    axis(1)
    axis(2)
}

b.tilde <- benefits.priv$b.tilde
b.bar   <- benefits.priv$b.bar

W <- H <- 4.8
pdf(outpath.no.weight, width = W, height = H)
make.scatter(b.tilde, b.bar, weights = NULL)
dev.off()

pdf(outpath.scatter, width = W, height = H)
make.scatter(b.tilde, b.bar, weights = log(pop)/mean(log(pop)))
dev.off()


make.IQR.plot <- function(benefits, pop, outpath.plot){
    ## IQRs
    b.bar   <- benefits$b.bar
    b.tilde <- benefits$b.tilde

    qt <- c(0.10, 0.25, 0.50, 0.75, 0.90)
    diff <- b.bar - b.tilde
    Dist <- list()
    Dist[['tilde']] <- sapply(qt, function(q) weighted.quantile(b.tilde, q, pop))
    Dist[['bar']]   <- sapply(qt, function(q) weighted.quantile(b.bar,   q, pop))
    Dist[['diff']]  <- sapply(qt, function(q) weighted.quantile(diff,    q, pop))
    K <- length(Dist)
    combo <- do.call(c, Dist)
    ylim <- range(combo)

    pdf(outpath.plot, width = W, height = H)
    plot(1:K, -1e3*rep(1, times = 3), xlim = c(0.5, K + 0.5), ylim = ylim,
         axes = FALSE, xlab = '', ylab = 'Amount ($)')
    grid()
    abline(h = 0, col = 'grey20')
    for (k in 1:K){
        make.IQR.bar(k, Dist[[k]])
    }
    axis(1, at = 1:3, labels = c('Marginal', 'Average', 'Difference'))
    axis(2)
    dev.off()
}

make.IQR.plot(benefits.soc,  pop, outpath.IQR.soc)
make.IQR.plot(benefits.priv, pop, outpath.IQR)

# Spence
Spence <- benefits.soc$b.bar - benefits.soc$b.tilde
Displace <- benefits.soc$b.tilde - benefits.priv$b.tilde

## Data
dat <- data.frame(county = names(b.bar), b.bar = b.bar, b.tilde = b.tilde)
write.dat(dat, outpath.dat)

J.chg.rel <- benefits.priv$J.chg.rel
b.bar <- benefits.priv$b.bar
b.tilde <- benefits.priv$b.tilde
## J plot
pdf(outpath.J, height = H, width = W)
ymax <- max(b.bar)
xmax <- max(J.chg.rel)*100
plot(J.chg.rel*100, b.bar, pch = 8, axes = FALSE,
     xlab = 'Change in # restaurants on DoorDash (%)',
     ylab = 'Consumer benefit ($)',
     ylim = c(0, ymax), xlim = c(0, xmax))
grid()
points(J.chg.rel*100, b.tilde, pch = 18, col = 'royalblue')
axis(1)
axis(2)
legend('bottomright', pch = c(8, 18), col = c('black', 'royalblue'),
       legend = c('Average', 'Marginal'))
dev.off()
