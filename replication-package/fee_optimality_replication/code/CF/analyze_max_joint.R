# Analyze fees maximizing joint profits
library(Matrix)
library(FoodDeliveryTools)
library(EconTools)

source('code/CF/welfare_calculations.R')

nsim.suffix <- '_nsim50'

# Specify paths
outdir <- sprintf('output/CF_feefirst/analysis%s', nsim.suffix)
outpath <- sprintf('%s/joint_profit_max_effects.csv', outdir)
outpath.NC <- sprintf('%s/joint_profit_max_effects_NC.csv', outdir)
outpath.welfare <- sprintf('%s/joint_profit_max_welfare.csv', outdir)
CF.dir <- sprintf('output/CF_feefirst/spec%s', nsim.suffix)
CF.files <- list.files(CF.dir)

# Determine competitive and socially optimal equilibria
## Socially optimal
socopt   <- grep('^socopt', CF.files, value = TRUE)
## Privately optimal (baseline)
priv     <- grep('^baseline_[a-z]+\\.rds', CF.files, value = TRUE)
mjoint   <- grep('^maxjoint_[a-z]+\\_take2.rds', CF.files, value = TRUE)
## Privately optimal (no complementarity)
nocomp   <- grep('^NC', CF.files, value = TRUE)
nc.joint <- grep('^maxjointNC', CF.files, value = TRUE)
## Privately optimal (no multihoming)
nm       <- grep('^NM', CF.files, value = TRUE)
nm.joint <- grep('^maxjointNM', CF.files, value = TRUE)


## Socially optimal
socopt   <- sprintf('%s/%s', CF.dir, socopt)
## Privately optimal (baseline)
priv     <- sprintf('%s/%s', CF.dir, priv)
mjoint   <- sprintf('%s/%s', CF.dir, mjoint)
## Privately optimal (no complementarity)
nocomp   <- sprintf('%s/%s', CF.dir, nocomp)
nc.joint <- sprintf('%s/%s', CF.dir, nc.joint)
## Privately optimal (no multihoming)
nm       <- sprintf('%s/%s', CF.dir, nm)
nm.joint <- sprintf('%s/%s', CF.dir, nm.joint)

eqm.objs <- load.all.eqm.objs.feefirst(nsim.suffix, TRUE)
eqm.objs <- eqm.objs$county

## flatten the list
markets <- names(eqm.objs)
eqm.objs.co <- list()
for (market in names(eqm.objs)){
    counties <- names(eqm.objs[[market]])
    for (co in counties){
        eqm.objs.co[[co]] <- eqm.objs[[market]][[co]]
    }
}

# load results
Eqm.soc   <- list()
## Privately optimal (baseline)
Eqm.priv  <- list()
Eqm.joint <- list()
## Privately optimal (no complementarity)
Eqm.NC       <- list()
Eqm.joint.NC <- list()
## Privately optimal (no multihoming)
Eqm.NM       <- list()
Eqm.joint.NM <- list()

for (j in 1:length(priv)){
    Eqm <- readRDS(priv[j])
    counties <- names(Eqm)
    for (co in counties){
        Eqm.priv[[co]] <- Eqm[[co]]
    }
}
for (j in 1:length(mjoint)){
    Eqm <- readRDS(mjoint[j])
    counties <- names(Eqm)
    for (co in counties){
        Eqm.joint[[co]] <- Eqm[[co]]
    }
}
for (j in 1:length(socopt)){
    Eqm <- readRDS(socopt[j])
    counties <- names(Eqm)
    for (co in counties){
        Eqm.soc[[co]] <- Eqm[[co]]
    }
}
for (j in 1:length(nocomp)){
    Eqm <- readRDS(nocomp[j])
    counties <- names(Eqm)
    for (co in counties){
        Eqm.NC[[co]] <- Eqm[[co]]
    }
}
for (j in 1:length(nc.joint)){
    Eqm <- readRDS(nc.joint[j])
    counties <- names(Eqm)
    for (co in counties){
        Eqm.joint.NC[[co]] <- Eqm[[co]]
    }
}
for (j in 1:length(nm)){
    Eqm <- readRDS(nm[j])
    counties <- names(Eqm)
    for (co in counties){
        Eqm.NM[[co]] <- Eqm[[co]]
    }
}
for (j in 1:length(nm.joint)){
    Eqm <- readRDS(nm.joint[j])
    counties <- names(Eqm)
    for (co in counties){
        Eqm.joint.NM[[co]] <- Eqm[[co]]
    }
}

# Every equilibrium set must cover the full county set; a missing or stale
# market file is a hard error, not silent sample selection
counties.all <- names(eqm.objs.co)
nms <- list('baseline (priv)'    = names(Eqm.priv),
            'max-joint'          = names(Eqm.joint),
            'no-multihoming'     = names(Eqm.NM),
            'max-joint no-multi' = names(Eqm.joint.NM))
for (k in names(nms)){
    assert.complete.names(nms[[k]], counties.all,
                          what = sprintf('%s equilibria in %s (counties)', k, CF.dir),
                          hint = 'Re-run the CF solvers for this spec.')
}
counties.common <- counties.all


# Consumer fees
C.soc   <- list()
C.priv  <- list()
C.joint <- list()
C.nc    <- list()
C.ncj   <- list()
C.nm    <- list()
C.nmj   <- list()

# Restaurant commissions
R.soc   <- list()
R.priv  <- list()
R.joint <- list()
R.nc    <- list()
R.ncj   <- list()
R.nm    <- list()
R.nmj   <- list()

# Sales
S.soc   <- list()
S.priv  <- list()
S.joint <- list()
S.nc    <- list()
S.ncj   <- list()
S.nm    <- list()
S.nmj   <- list()

# Markups
M.soc   <- list()
M.priv  <- list()
M.joint <- list()

idx.f <- 2:5

num.param <- load.num.param()
for (co in counties.common){
    # Consumer fees
    C.soc[[co]]   <- Eqm.soc[[co]]$C
    C.priv[[co]]  <- Eqm.priv[[co]]$C
    C.joint[[co]] <- Eqm.joint[[co]]$C
    C.nc[[co]]    <- Eqm.NC[[co]]$C
    C.ncj[[co]]   <- Eqm.joint.NC[[co]]$C
    C.nm[[co]]    <- Eqm.NM[[co]]$C
    C.nmj[[co]]   <- Eqm.joint.NM[[co]]$C

    # Commissions
    R.soc[[co]]   <- Eqm.soc[[co]]$R
    R.priv[[co]]  <- Eqm.priv[[co]]$R
    R.joint[[co]] <- Eqm.joint[[co]]$R
    R.nc[[co]]    <- Eqm.NC[[co]]$R
    R.ncj[[co]]   <- Eqm.joint.NC[[co]]$R
    R.nm[[co]]    <- Eqm.NM[[co]]$R
    R.nmj[[co]]   <- Eqm.joint.NM[[co]]$R

    # Sales
    S.soc[[co]]   <- Eqm.soc[[co]]$FP$Sales.platform[idx.f]
    S.priv[[co]]  <- Eqm.priv[[co]]$FP$Sales.platform[idx.f]
    S.joint[[co]] <- Eqm.joint[[co]]$FP$Sales.platform[idx.f]
    S.nc[[co]]    <- Eqm.NC[[co]]$FP$Sales.platform[idx.f]
    S.ncj[[co]]   <- Eqm.joint.NC[[co]]$FP$Sales.platform[idx.f]
    S.nm[[co]]    <- Eqm.NM[[co]]$FP$Sales.platform[idx.f]
    S.nmj[[co]]   <- Eqm.joint.NM[[co]]$FP$Sales.platform[idx.f]

    # Markups
    ## Compute profits
    Es <- Eqm.soc[[co]]
    Ep <- Eqm.priv[[co]]
    Ej <- Eqm.joint[[co]]

    kappa  <- eqm.objs.co[[co]]$kappa
    dat.m  <- eqm.objs.co[[co]]$dat.m
    pMC.df <- eqm.objs.co[[co]]$pMC.df
    opts   <- eqm.objs.co[[co]]$opts
    pp.soc   <- platform.profits(Es$FP, Es$C, Es$R, kappa, dat.m, pMC.df, opts, num.param)
    pp.priv  <- platform.profits(Ep$FP, Ep$C, Ep$R, kappa, dat.m, pMC.df, opts, num.param)
    pp.joint <- platform.profits(Ej$FP, Ej$C, Ej$R, kappa, dat.m, pMC.df, opts, num.param)

    M.soc[[co]]   <- pp.soc/S.soc[[co]]
    M.priv[[co]]  <- pp.priv/S.priv[[co]]
    M.joint[[co]] <- pp.joint/S.joint[[co]]
}

C.soc   <- do.call(rbind, C.soc)
C.priv  <- do.call(rbind, C.priv)
C.joint <- do.call(rbind, C.joint)
C.nc    <- do.call(rbind, C.nc)
C.ncj   <- do.call(rbind, C.ncj)
C.nm    <- do.call(rbind, C.nm)
C.nmj   <- do.call(rbind, C.nmj)

R.soc   <- do.call(rbind, R.soc)
R.priv  <- do.call(rbind, R.priv)
R.joint <- do.call(rbind, R.joint)
R.nc    <- do.call(rbind, R.nc)
R.ncj   <- do.call(rbind, R.ncj)
R.nm    <- do.call(rbind, R.nm)
R.nmj   <- do.call(rbind, R.nmj)

S.soc   <- do.call(rbind, S.soc)
S.priv  <- do.call(rbind, S.priv)
S.joint <- do.call(rbind, S.joint)
S.nc    <- do.call(rbind, S.nc)
S.ncj   <- do.call(rbind, S.ncj)
S.nm    <- do.call(rbind, S.nm)
S.nmj   <- do.call(rbind, S.nmj)

M.soc   <- do.call(rbind, M.soc)
M.priv  <- do.call(rbind, M.priv)
M.joint <- do.call(rbind, M.joint)

C.diff.joint <- C.joint - C.priv
R.diff.joint <- R.joint - R.priv

C.diff.joint.NC <- C.ncj - C.nc
R.diff.joint.NC <- R.ncj - R.nc

C.diff.joint.NM <- C.nmj - C.nm
R.diff.joint.NM <- R.nmj - R.nm

weight.BL <- S.priv
weight.NC <- S.nc
weight.NM <- S.nm

C.soc.avg      <- weighted.mean(C.soc,            weight.BL)
C.priv.avg     <- weighted.mean(C.priv,           weight.BL)
C.joint.avg    <- weighted.mean(C.joint,          weight.BL)
C.priv.nc.avg  <- weighted.mean(C.nc,             weight.NC)
C.priv.ncj.avg <- weighted.mean(C.ncj,            weight.NC)
C.priv.nm.avg  <- weighted.mean(C.nm,             weight.NM)
C.priv.nmj.avg <- weighted.mean(C.nmj,            weight.NM)
C.diff1.avg    <- weighted.mean(C.priv - C.soc,   weight.BL)
C.diff2.avg    <- weighted.mean(C.joint - C.priv, weight.BL)
C.diff.NC.avg  <- weighted.mean(C.diff.joint.NC,  weight.NC)
C.diff.NM.avg  <- weighted.mean(C.diff.joint.NM,  weight.NM)

R.soc.avg      <- weighted.mean(R.soc,            weight.BL)
R.priv.avg     <- weighted.mean(R.priv,           weight.BL)
R.joint.avg    <- weighted.mean(R.joint,          weight.BL)
R.priv.nc.avg  <- weighted.mean(R.nc,             weight.NC)
R.priv.ncj.avg <- weighted.mean(R.ncj,            weight.NC)
R.priv.nm.avg  <- weighted.mean(R.nm,             weight.NM)
R.priv.nmj.avg <- weighted.mean(R.nmj,            weight.NM)
R.diff1.avg    <- weighted.mean(R.priv - R.soc,   weight.BL)
R.diff2.avg    <- weighted.mean(R.joint - R.priv, weight.BL)
R.diff.NC.avg  <- weighted.mean(R.diff.joint.NC,  weight.NC)
R.diff.NM.avg  <- weighted.mean(R.diff.joint.NM,  weight.NM)

M.soc.avg      <- weighted.mean(M.soc,            weight.BL)
M.priv.avg     <- weighted.mean(M.priv,           weight.BL)
M.joint.avg    <- weighted.mean(M.joint,          weight.BL)

# Formatting
C.soc.avg      <- sprintf('%0.2f',  C.soc.avg)
C.priv.avg    <- sprintf('%0.2f',  C.priv.avg)
C.diff1.avg   <- sprintf('%0.2f',  C.diff1.avg)
C.diff2.avg   <- sprintf('%0.2f',  C.diff2.avg)
C.joint.avg   <- sprintf('%0.2f',  C.joint.avg)
C.priv.nc.avg  <- sprintf('%0.2f', C.priv.nc.avg)
C.priv.ncj.avg <- sprintf('%0.2f', C.priv.ncj.avg)
C.priv.nm.avg  <- sprintf('%0.2f', C.priv.nm.avg)
C.priv.nmj.avg <- sprintf('%0.2f', C.priv.nmj.avg)
C.diff.NC.avg  <- sprintf('%0.2f',  C.diff.NC.avg)

R.soc.avg      <- sprintf('%0.1f', 100*R.soc.avg)
R.priv.avg     <- sprintf('%0.1f' ,100*R.priv.avg)
R.diff1.avg    <- sprintf('%0.1f', 100*R.diff1.avg)
R.diff2.avg    <- sprintf('%0.1f', 100*R.diff2.avg)
R.joint.avg    <- sprintf('%0.1f', 100*R.joint.avg)
R.priv.nc.avg  <- sprintf('%0.1f', 100*R.priv.nc.avg)
R.priv.ncj.avg <- sprintf('%0.1f', 100*R.priv.ncj.avg)
R.priv.nm.avg  <- sprintf('%0.1f', 100*R.priv.nm.avg)
R.priv.nmj.avg <- sprintf('%0.1f', 100*R.priv.nmj.avg)
R.diff.NC.avg  <- sprintf('%0.1f', 100*R.diff.NC.avg)

M.soc.avg     <- sprintf('%0.2f',  M.soc.avg)
M.priv.avg    <- sprintf('%0.2f',  M.priv.avg)
M.joint.avg   <- sprintf('%0.2f',  M.joint.avg)

# Produce the table
tab.joint <- data.frame(var   = c('Consumer fees (\\$)',
                                  'Restaurant commissions (\\%)',
                                  'Platform markup (\\$)'),
                        soc   = c(C.soc.avg, R.soc.avg, M.soc.avg),
                        priv  = c(C.priv.avg, R.priv.avg, M.priv.avg),
                        joint = c(C.joint.avg, R.joint.avg, M.joint.avg))


# How do fees change from priv to joint when there is no complementarity between portfolios
tab.NC <- data.frame(var     = c('Consumer fees (\\$)', 'Restaurant commissions (\\%)'),
                     privNC  = c(C.priv.nc.avg, R.priv.nc.avg),
                     jointNC = c(C.priv.ncj.avg, R.priv.ncj.avg),
                     privNM  = c(C.priv.nm.avg, R.priv.nm.avg),
                     jointNM = c(C.priv.nmj.avg, R.priv.nmj.avg))


# Save results
write.dat(tab.joint, outpath)
write.dat(tab.NC, outpath.NC)

# Add welfare analysis
Comp  <- list()
Joint <- list()
Soc   <- list()
scale <- c()
for (co in counties.common){
    # Extract data objects
    eqm.objs.j <- eqm.objs.co[[co]]
    dat.m  <- eqm.objs.j$dat.m
    opts   <- eqm.objs.j$opts
    pMC.df <- eqm.objs.j$pMC.df
    kappa  <- eqm.objs.j$kappa

    # Extract equilibria
    ## Competitive
    eqm.c <- Eqm.priv[[co]]
    ## Joint profit maximization
    eqm.j <- Eqm.joint[[co]]
    ## Socially optimal
    eqm.s <- Eqm.soc[[co]]

    # Compute welfare
    FC.est <- NULL
    welfare.c <- welfare.for.CF(dat.m, market, FC.est, eqm.c, opts, pMC.df, kappa)
    welfare.j <- welfare.for.CF(dat.m, market, FC.est, eqm.j, opts, pMC.df, kappa)
    welfare.s <- welfare.for.CF(dat.m, market, FC.est, eqm.s, opts, pMC.df, kappa)

    Comp[[co]] <- c(C = welfare.c$CW$total.EU.dollar,
                    P = sum(welfare.c$p.profit),
                    R = welfare.c$r.profit['total'])
    Joint[[co]] <- c(C = welfare.j$CW$total.EU.dollar,
                     P = sum(welfare.j$p.profit),
                     R = welfare.j$r.profit['total'])
    Soc[[co]] <- c(C = welfare.s$CW$total.EU.dollar,
                   P = sum(welfare.s$p.profit),
                   R = welfare.s$r.profit['total'])
    scale[co] <- sum(eqm.c$FP$Sales.platform[2:5])
}

Comp.mat  <- do.call(rbind, Comp)
Joint.mat <- do.call(rbind, Joint)
Soc.mat   <- do.call(rbind, Soc)

Comp.agg <- colSums(Comp.mat)
Joint.agg <- colSums(Joint.mat)
Soc.agg <- colSums(Soc.mat)

# Add totals
Comp.agg  <- c(Comp.agg,  total = sum(Comp.agg))
Joint.agg <- c(Joint.agg, total = sum(Joint.agg))
Soc.agg   <- c(Soc.agg,   total = sum(Soc.agg))

Comp.diff   <- (Comp.agg - Soc.agg)/sum(scale)
Joint.diff  <- (Joint.agg - Comp.agg)/sum(scale)

# Save table
welfare.tab <- data.frame(var = c('Consumer welfare', 'Restaurant profits',
                                  'Platform profits', 'Total welfare'),
                          val = c(Joint.diff[c(1, 3, 2, 4)]))
welfare.tab$val <- sprintf('%0.2f', welfare.tab$val)
write.dat(welfare.tab, outpath.welfare)

