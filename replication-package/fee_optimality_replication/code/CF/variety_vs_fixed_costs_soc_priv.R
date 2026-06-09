# Produce an exhibit comparing the fixed costs and variety benefits of
# commission reductions from the privately optimal fees

library(Matrix)
library(EconTools)
library(FoodDeliveryTools)

source('code/CF/welfare_calculations.R')

nsim.suffix <- '_nsim50'
# Specify paths
outdir <- sprintf('output/CF_feefirst/analysis%s', nsim.suffix)
outpath <- sprintf('%s/variety_fc_priv.csv', outdir)

eqm.objs <- load.all.eqm.objs.feefirst(nsim.suffix, TRUE)
eqm.objs <- eqm.objs$county

num.param <- load.num.param()

## flatten the list
markets <- names(eqm.objs)
eqm.objs.co <- list()
for (market in names(eqm.objs)){
    counties <- names(eqm.objs[[market]])
    for (co in counties){
        eqm.objs.co[[co]] <- eqm.objs[[market]][[co]]
    }
}

# Load equilibria
priv <- grep('^baseline_[a-z]+\\.rds', CF.files, value = TRUE)
priv <- sprintf('%s/%s', CF.dir, priv)
Eqm.priv <- list()
for (j in 1:length(priv)){
    Eqm <- readRDS(priv[j])
    counties <- names(Eqm)
    for (co in counties){
        Eqm.priv[[co]] <- Eqm[[co]]
    }
}


# For each county, compute the welfare gains from a 1% commission reduction
# as well as the fixed cost change
counties <- names(Eqm.priv)

# Commission perturbation amount
h.comm <- 0.01

Chg.Variety <- c()
Chg.FC      <- c()
Sales       <- c()
for (co in counties){

    #== Extract data ==#
    eqm.objs.j <- eqm.objs.co[[co]]
    dat.m <- eqm.objs.j$dat.m
    buy.m <- dat.m$buy.m
    kappa <- eqm.objs.j$kappa
    opts  <- eqm.objs.j$opts
    opts$verbose <- FALSE

    #== Extract equilibrium results ==#
    eqm <- Eqm.priv[[co]]
    C <- eqm$C
    R <- eqm$R

    #== Compute variety benefits and fixed cost changes ==#
    for (z in names(dat.m$fees)){
        dat.m$fees[[z]] <- C
    }
    for (z in names(dat.m$comm)){
        dat.m$comm[[z]] <- R
    }
    FP0 <- find.fixed.point(kappa, dat.m, opts, num.param, more.outputs = TRUE)
    W0  <- compute.consumer.welfare(dat.m, C, FP0$J.G.1, FP0$Rhos, no.logit = FALSE)
    W0  <- W0$total.EU.dollar
    FC0 <- sum(sapply(names(FP0$J.G.1), function(z) sum(FP0$J.G.1[[z]]*kappa$K[[z]])))

    W.perturb  <- c()
    FC.perturb <- c()

    # Try uniform reduction in commissions
    R.1 <- R
    R.1 <- R - h.comm
    for (z in names(dat.m$comm)){
        dat.m$comm[[z]] <- R.1
    }
    FP1 <- find.fixed.point(kappa, dat.m, opts, num.param, more.outputs = TRUE)

    # Compute welfare
    W1 <- compute.consumer.welfare(dat.m, C, FP1$J.G.1, FP0$Rhos, no.logit = FALSE)
    W1 <- W1$total.EU.dollar
    W.perturb <- W1

    # Compute fixed costs
    FC.perturb <- sum(sapply(names(FP1$J.G.1), function(z) sum(FP1$J.G.1[[z]]*kappa$K[[z]])))

    Chg.Variety[co] <- (W.perturb - W0)
    Chg.FC[co]     <- (FC.perturb - FC0)
    Sales[co]      <- sum(FP0$Sales.platform[2:5])
}

mean.var <- sum(Chg.Variety)/sum(Sales)
mean.fc  <- sum(Chg.FC)/sum(Sales)

sd.var  <- sqrt(weighted.mean((Chg.Variety/Sales - mean.var)^2, Sales))
sd.fc   <- sqrt(weighted.mean((Chg.FC/Sales - mean.fc)^2, Sales))
diff    <- Chg.Variety/Sales - Chg.FC/Sales
sd.diff <- sqrt(weighted.mean((diff - (mean.var - mean.fc))^2, Sales))

weighted.mean(Chg.Variety/Sales, Sales)

# Table
tab <- data.frame(var  = c('Variety', 'Fixed cost', 'Net'),
                  val  = c(mean.var, mean.fc, mean.var - mean.fc))
tab$val  <- sprintf('%0.2f', tab$val)

write.dat(tab, outpath)
