# Explain in the cross section of counties the level of consumer fees and
# restaurant commissions

library(Matrix)
library(FoodDeliveryTools)
library(EconTools)

#== Specify paths ==#
nsim.suffix <- '_nsim50'
inpaths <- collect.inpaths(nsim.suffix)
CF.dir <- sprintf('output/CF_feefirst/spec%s', nsim.suffix)
outdir <- sprintf('output/CF_feefirst/analysis%s', nsim.suffix)
inpath.dens <- 'data/ACS/processed/ACS_nearby.csv'

outpath.C <- sprintf('%s/explain_gap_consumer.csv', outdir)
outpath.R <- sprintf('%s/explain_gap_restaurant.csv', outdir)

#== Preliminaries ==#
# Numerical parameters
num.param <- load.num.param()
num.param$tol.FP <- 1e-8

#== Load data ==#
# Load data objects
load.pMC <- TRUE
eqm.objs <- load.all.eqm.objs.feefirst(nsim.suffix, load.pMC)
eqm.objs.co <- eqm.objs$county
# Load CBSA codes
cbsa.codes <- read.dat(inpaths$inpath.cbsa.codes)
rownames(cbsa.codes) <- cbsa.codes$cbsa
# Load competitive equilibria
eqm.comp <- load.equilibria.priv.soc(CF.dir, cbsa.codes, monopoly = FALSE)
BL  <- eqm.comp$BL
soc <- eqm.comp$soc
# Load geographical/demographic data
dens <- read.dat(inpath.dens, colClasses = c('zcta' = 'character'))
dens <- dens[, c('zcta', 'population')]
colnames(dens) <- c('zip', 'pop_nearby')
geo <- load.geo(inpaths$inpath.geo)
geo <- geo[geo$is.zcta, ]
# Enumerate markets
markets <- names(eqm.objs.co)


# Initialize outputs
C.soc  <- list()
C.priv <- list()
R.soc  <- list()
R.priv <- list()
Cannibal <- list()
Variety  <- list()
FC.fx    <- list()
Sales.fx <- list()
MP.cons  <- list()
MP.resto <- list()
R.markup <- list()
Sales.soc  <- list()
Sales.priv <- list()

# Some additional variables
Chg.NL      <- list()
Chg.Variety <- list()
Chg.FC      <- list()

h.cannibal <- 0.01
h.comm     <- 0.01
h.fee      <- 0.10

for (market in markets){
    eqm.objs.m <- eqm.objs.co[[market]]
    counties <- names(eqm.objs.m)
    for (co in counties){

        #== Load data ==#
        eqm.objs.j <- eqm.objs.m[[co]]
        dat.m <- eqm.objs.j$dat.m
        buy.m <- dat.m$buy.m
        kappa <- eqm.objs.j$kappa
        opts  <- eqm.objs.j$opts
        opts$verbose <- FALSE

        #== Load equilibrium results ==#
        eqm.p <- BL[[market]][[co]]
        eqm.s <- soc[[market]][[co]]

        #== Store platform fees ==#
        C.p <- eqm.p$C
        R.p <- eqm.p$R
        C.s <- eqm.s$C
        R.s <- eqm.s$R

        C.soc[[co]]  <- C.s
        C.priv[[co]] <- C.p
        R.soc[[co]]  <- R.s
        R.priv[[co]] <- R.p

        #== Cannibalization ==#
        sales.0 <-  compute.sales(eqm.s, dat.m, exclude.off = FALSE)
        cannibal.co <- c()
        C0 <- C.s
        NF <- length(C0)
        for (f in 1:NF){
            C1 <- C0
            C1[f] <- C1[f] + h.cannibal
            eqm.s$C <- C1
            sales.1 <- compute.sales(eqm.s, dat.m, exclude.off = FALSE)
            eqm.s$C <- C0
            cannibal.co[f] <- -(sales.1[1] - sales.0[1])/(sales.1[f + 1] - sales.0[f + 1])
        }
        Cannibal[[co]] <- cannibal.co

        #== Variety benefits from commission reductions ==#
        # This requires solving the market at perturbed commissions
        for (z in names(dat.m$fees)){
            dat.m$fees[[z]] <- C.s
        }
        for (z in names(dat.m$comm)){
            dat.m$comm[[z]] <- R.s
        }
        FP0 <- find.fixed.point(kappa, dat.m, opts, num.param, more.outputs = TRUE)
        W0 <- compute.consumer.welfare(dat.m, C.s, FP0$J.G.1, eqm.s$FP$Rhos, no.logit = FALSE)
        W0 <- W0$total.EU.dollar
        S0 <- FP0$Sales.platform[2:5]
        FC0 <- sum(sapply(names(FP0$J.G.1), function(z) sum(FP0$J.G.1[[z]]*kappa$K[[z]])))

        # Number of listings in each portfolio
        NL.per.G <- sapply(strsplit(sub('G', '', names(FP0$J.G.1[[1]])), ''), function(x) sum(as.numeric(x)))
        NL0 <- sum(sapply(names(FP0$J.G.1), function(z) sum(FP0$J.G.1[[z]]*NL.per.G)))

        W.perturb  <- c()
        S.perturb  <- c()
        FC.perturb <- c()
        NL.perturb <- c()

        for (f in 1:NF){
            R.s1 <- R.s
            R.s1[f] <- R.s1[f] - h.comm
            for (z in names(dat.m$comm)){
                dat.m$comm[[z]] <- R.s1
            }
            FP1 <- find.fixed.point(kappa, dat.m, opts, num.param, more.outputs = TRUE)
            S.perturb[f] <- FP1$Sales.platform[f + 1]

            # Compute consumer welfare
            W1 <- compute.consumer.welfare(dat.m, C.s, FP1$J.G.1, eqm.s$FP$Rhos, no.logit = FALSE)
            W1 <- W1$total.EU.dollar
            W.perturb[f] <- W1

            # Compute fixed costs
            FC.perturb[f] <- sum(sapply(names(FP1$J.G.1), function(z) sum(FP1$J.G.1[[z]]*kappa$K[[z]])))

            # Number of listings
            NL1 <- sum(sapply(names(FP1$J.G.1), function(z) sum(FP1$J.G.1[[z]]*NL.per.G)))
            NL.perturb[f] <- NL1
        }

        Chg.Variety[[co]] <- (W.perturb - W0)
        Chg.NL[[co]]      <- (NL.perturb - NL0)
        Chg.FC[[co]]      <- (FC.perturb - FC0)

        Variety[[co]]  <- (W.perturb - W0)/(NL.perturb - NL0)
        FC.fx[[co]]    <- (FC.perturb - FC0)/(NL.perturb - NL0)*1e5

        #== Seller-side market power ==#
        ## Elasticity of J.f with respect to commission rate
        for (z in names(dat.m$fees)){
            dat.m$fees[[z]] <- C.p
        }
        for (z in names(dat.m$comm)){
            dat.m$comm[[z]] <- R.p
        }
        FP0 <- find.fixed.point(kappa, dat.m, opts, num.param, more.outputs = TRUE)
        Jf <- c()
        for (f in 1:NF){
            Jf[f] <- sum(sapply(names(FP0$J.G.1), function(z) sum(FP0$J.G.1[[z]]*dat.m$G.mat[, f + 1])))
        }
        J.perturb <- c()
        S.perturb <- c()
        for (f in 1:NF){
            R.p1 <- R.p
            R.p1[f] <- R.p1[f] - h.comm
            for (z in names(dat.m$comm)){
                dat.m$comm[[z]] <- R.p1
            }
            FP1 <- find.fixed.point(kappa, dat.m, opts, num.param, more.outputs = TRUE)
            # Sales
            S.perturb[f] <- FP1$Sales.platform[f + 1]
            # Number of restaurants on platform f
            J.perturb[f] <- sum(sapply(names(FP0$J.G.1), function(z) sum(FP1$J.G.1[[z]]*dat.m$G.mat[, f + 1])))
        }

        Sales.fx[[co]] <- (S.perturb - S0)/(J.perturb - Jf)
        MP.resto[[co]] <- (J.perturb - Jf)/h.comm

        #== Buyer-side market power ==#
        for (z in names(dat.m$fees)){
            dat.m$fees[[z]] <- C.p
        }
        for (z in names(dat.m$comm)){
            dat.m$comm[[z]] <- R.p
        }
        # Fixed point or hold J fixed?
        # It is more consistent with the theory to hold J fixed
        S0 <- compute.sales(eqm.p, dat.m)
        Deriv <- compute.fee.deriv(eqm.objs.j, eqm.p)
        semi.elas.S <- -Deriv/S0
        MP.cons[[co]] <- 1/semi.elas.S

        # Restaurant markups
        mc.on <- opts$resto.param$mc$on
        rho.avg <- weight.rhos.by.resto(eqm.s$FP$Sales.tots, eqm.s$FP$Rhos, dat.m, mc.on)
        rho.avg <- rho.avg$markups.tot
        R.markup[[co]] <- rho.avg

        # Sales
        Sales.priv[[co]] <- eqm.p$FP$Sales.platform[2:5]
        Sales.soc[[co]]  <- eqm.s$FP$Sales.platform[2:5]
    }
}


J.priv <- list()
tot.J <- c()
for (market in markets){
    eqm.objs.m <- eqm.objs.co[[market]]
    counties <- names(eqm.objs.m)
    for (co in counties){
        eqm.p <- BL[[market]][[co]]
        dat.m <- eqm.objs.m[[co]]$dat.m
        J.co <- c()
        for (f in 1:NF){
            J.co[f] <- sum(sapply(names(eqm.p$FP$J.G.1), function(z) sum(eqm.p$FP$J.G.1[[z]]*dat.m$G.mat[, f + 1])))
        }
        J.priv[[co]] <- J.co
        tot.J[co] <-  sum(sapply(names(eqm.p$FP$J.G.1), function(z) sum(eqm.p$FP$J.G.1[[z]])))
    }
}


# Process fees
C.soc    <- do.call(rbind, C.soc   )
C.priv   <- do.call(rbind, C.priv  )
R.soc    <- do.call(rbind, R.soc   )
R.priv   <- do.call(rbind, R.priv  )
colnames(C.soc)    <- sprintf('C_soc_%d' ,    1:NF)
colnames(C.priv)   <- sprintf('C_priv_%d',    1:NF)
colnames(R.soc)    <- sprintf('R_soc_%d',     1:NF)
colnames(R.priv)   <- sprintf('R_priv_%d',    1:NF)

DFs.fees <- list(C.soc = C.soc, C.priv = C.priv, R.soc = R.soc, R.priv = R.priv)

for (k in 1:length(DFs.fees)){
    df <- DFs.fees[[k]]

    # Convert to data.frame
    df <- as.data.frame(df)

    # Add county for merging
    df$county    <- rownames(df)

    var.name <- sub('_1', '', colnames(df)[1])
    df.long <- tidyr::pivot_longer(df, cols = !county,
                                   names_to = 'platform',
                                   names_pattern = '.*_([1-4])$',
                                   values_to = var.name)
    DFs.fees[[k]] <- df.long
}

df.fees <- Reduce(function(x, y) dplyr::left_join(x, y, by = c('county', 'platform')), DFs.fees)

# Process restaurant markups
R.markup <- do.call(rbind, R.markup)
colnames(R.markup) <- sprintf('R_markup_%d', 1:NF)
R.markup <- as.data.frame(R.markup)
R.markup$county <- rownames(R.markup)

var.name <- sub('_1', '', colnames(R.markup)[1])
R.markup <- tidyr::pivot_longer(R.markup, cols = !county,
                               names_to = 'platform',
                               names_pattern = '.*_([1-4])$',
                               values_to = var.name)

# Process sales
Sales.soc  <- do.call(rbind, Sales.soc)
Sales.priv <- do.call(rbind, Sales.priv)
colnames(Sales.soc)  <- sprintf('Sales_soc_%d' , 1:NF)
colnames(Sales.priv) <- sprintf('Sales_priv_%d', 1:NF)
DFs.sales <- list(Sales.soc = Sales.soc, Sales.priv = Sales.priv)
for (k in 1:length(DFs.sales)){
    df <- DFs.sales[[k]]
    df <- as.data.frame(df)
    df$county <- rownames(df)
    var.name <- sub('_1', '', colnames(df)[1])
    df.long <- tidyr::pivot_longer(df, cols = !county,
                                   names_to = 'platform',
                                   names_pattern = '.*_([1-4])$',
                                   values_to = var.name)
    DFs.sales[[k]] <- df.long
}

df.sales <- Reduce(function(x, y) dplyr::left_join(x, y, by = c('county', 'platform')), DFs.sales)



# Other variables
Cannibal <- do.call(rbind, Cannibal)
Variety  <- do.call(rbind, Variety )
FC.fx    <- do.call(rbind, FC.fx   )
Sales.fx <- do.call(rbind, Sales.fx)
MP.cons  <- do.call(rbind, MP.cons )
MP.resto <- do.call(rbind, MP.resto)

Chg.Variety <- do.call(rbind, Chg.Variety)
Chg.FC      <- do.call(rbind, Chg.FC)
Chg.NL      <- do.call(rbind, Chg.NL)


colnames(Cannibal)    <- sprintf('Cannibal_%d' ,   1:NF)
colnames(Variety)     <- sprintf('Variety_%d',     1:NF)
colnames(FC.fx)       <- sprintf('FC_fx_%d',       1:NF)
colnames(Sales.fx)    <- sprintf('Sales_fx_%d',    1:NF)
colnames(MP.cons)     <- sprintf('MP_cons_%d' ,    1:NF)
colnames(MP.resto)    <- sprintf('MP_resto_%d',    1:NF)
colnames(Chg.Variety) <- sprintf('Chg_Variety_%d', 1:NF)
colnames(Chg.FC)      <- sprintf('Chg_FC_%d' ,     1:NF)
colnames(Chg.NL)      <- sprintf('Chg_NL_%d',      1:NF)


DFs <- list(Cannibal = Cannibal, Variety = Variety, FC.fx = FC.fx,
            Sales.fx = Sales.fx, MP.cons = MP.cons, MP.resto = MP.resto,
            Chg.Variety = Chg.Variety,
            Chg.FC = Chg.FC,
            Chg.NL = Chg.NL)

for (k in 1:length(DFs)){
    df <- DFs[[k]]

    # Convert to data.frame
    df <- as.data.frame(df)

    # Add county for merging
    df$county    <- rownames(df)

    var.name <- sub('_1', '', colnames(df)[1])
    df.long <- tidyr::pivot_longer(df, cols = !county,
                                   names_to = 'platform',
                                   names_pattern = '.*_([1-4])$',
                                   values_to = var.name)
    DFs[[k]] <- df.long
}

df.compare <- Reduce(function(x, y) dplyr::left_join(x, y, by = c('county', 'platform')), DFs)

df.compare$C_priv <- NULL
df.compare$C_soc  <- NULL
df.compare$R_priv <- NULL
df.compare$R_soc  <- NULL

df.compare <- dplyr::left_join(df.compare, df.fees,  by = c('county', 'platform'))
df.compare <- dplyr::left_join(df.compare, R.markup, by = c('county', 'platform'))
df.compare <- dplyr::left_join(df.compare, df.sales, by = c('county', 'platform'))

# Number of restaurants on each platform
J.priv <- do.call(rbind, J.priv)
colnames(J.priv) <- sprintf('J_priv_%d' , 1:NF)
J.priv <- as.data.frame(J.priv)
J.priv$county <- rownames(J.priv)
var.name <- 'J_priv'
J.priv <- tidyr::pivot_longer(J.priv, cols = !county,
                              names_to = 'platform',
                              names_pattern = '.*_([1-4])$',
                              values_to = var.name)
df.compare <- dplyr::left_join(df.compare, J.priv, by = c('county', 'platform'))

# Total number of restaurants
tot.J <- data.frame(county = names(tot.J), tot_J = tot.J)
df.compare <- dplyr::left_join(df.compare, tot.J, by = 'county')



# Define additional variables
df.compare$C_gap <- df.compare$C_priv - df.compare$C_soc
df.compare$R_gap <- 100*(df.compare$R_priv - df.compare$R_soc)
df.compare$R_priv_pct <- 100*df.compare$R_priv

df.compare$elas_cons <- 1/df.compare$MP_cons
df.compare$elas_J <- 1/df.compare$MP_resto

df.compare$log_elas_cons <- log(pmax(df.compare$elas_cons, 0.01))
df.compare$log_elas_J    <- log(pmax(df.compare$elas_J, 0.01))


df.compare$pfac <- as.factor(df.compare$platform)
df.compare$log_Cannibal <- log(pmax(df.compare$Cannibal, 1e-3))
df.compare$log_Variety <- log(pmax(df.compare$Variety, 1e-3))

df.compare$displace <- (df.compare$R_soc - df.compare$R_priv)*100

## Scaled changes
df.compare$Chg_FC_scaled      <- df.compare$Chg_FC/df.compare$Sales_priv
df.compare$Chg_Variety_scaled <- df.compare$Chg_Variety/df.compare$Sales_priv
df.compare$Chg_NL_scaled      <- df.compare$Chg_NL/df.compare$Sales_priv



# Run regression
## First platform specific
idx <- which(df.compare$platform <= 3)

# Add restaurant markup

df.compare$MP_cons_alt <- 1/df.compare$elas_cons
df.compare$Spence <- df.compare$R_priv


df.compare$inv_semi_elas_cons <- 1/df.compare$elas_cons*df.compare$C_priv
df.compare$semi_elas_cons <- 1/df.compare$MP_cons
df.compare$semi_elas_J    <- 1/pmax(df.compare$elas_J, 0.10)*df.compare$R_priv
df.compare$Variety.FC.ratio <- pmax(df.compare$Variety, 1e-3)/pmax(df.compare$FC_fx, 1e-3)
df.compare$Variety.S.ratio   <- pmax(df.compare$Variety, 1e-3)/pmax(df.compare$Sales_fx, 1e-3)

df.compare$Sales_alt <- pmax(df.compare$Sales_fx, 1e-3)
df.compare$Rev <- df.compare$Sales_alt*df.compare$C_priv
df.compare$log_FC <- log(pmax(df.compare$FC_fx, 1e-2))

df.compare$FC.alt      <- df.compare$FC_fx/1e5*100
df.compare$Variety.alt <- df.compare$Variety*100


df.compare$inv_sem_elas_J <- df.compare$semi_elas_J
df.compare$semi_elas_J <- df.compare$MP_resto/df.compare$J_priv
df.compare$deriv_J_share <- df.compare$MP_resto/df.compare$tot_J


run.reg <- function(dep.var, regressors, df.compare, weight.spec = 'none'){

    if (weight.spec == 'none'){
        W <- rep(1, nrow(df.compare))
    } else if (weight.spec == 'by_f'){
        counties <- unique(df.compare$county)
        W <- rep(0, nrow(df.compare))
        for (co in counties){
            idx.co <- which(df.compare$county == co)
            platforms.co <- df.compare$platform[idx.co]
            weights.co <- c()
            for (j in 1:length(idx.co)){
                weights.co[j] <- df.compare$Sales_priv[idx.co][j]
            }
            W[idx.co] <- weights.co/sum(weights.co)
        }
    } else if (weight.spec == 'sales'){
        W <- df.compare$Sales_priv
    }

    fmla.RHS <- Reduce(function(x, y) sprintf('%s + %s', x, y), regressors)
    fmla <- as.formula(sprintf('%s ~ %s', dep.var, fmla.RHS))
    reg <- lm(fmla, df.compare, weights = W)
    tab <- summary(reg)

    # Compute bivariate R^2 and partial R^2
    r2.bi <- c()
    for (reg in regressors){
        fmla.bi <- as.formula(sprintf('%s ~ %s', dep.var, reg))
        r2.bi[reg] <- summary(lm(fmla.bi, df.compare, weights = W))$r.squared
    }
    r2.part <- c()
    for (reg in regressors){
        regressors.sub <- setdiff(regressors, reg)
        fmla.RHS <- Reduce(function(x, y) sprintf('%s + %s', x, y), regressors.sub)
        fmla <- as.formula(sprintf('%s ~ %s', dep.var, fmla.RHS))
        r2.part[reg] <- summary(lm(fmla, df.compare, weights = W))$r.squared
    }
    outputs <- list(tab = tab, r2.bi = r2.bi, r2.part = r2.part)
    return(outputs)
}

process.tab <- function(tab, var.labels, r2.bi, r2.part){
    # Process the consumer fee table
    df <- as.data.frame(tab$coefficients[, 1:2])
    colnames(df) <- c('est', 'se')
    df$est <- sprintf('%0.2f', df$est)
    df$se  <- sprintf('\\footnotesize (%0.2f)', df$se)

    # Add R2
    R2.row <- c(est = sprintf('%0.2f', tab$r.squared), se = '')
    df <- rbind(df, R2.row)

    df$var <- c(var.labels, '$R^2$')
    df$r2_bi   <- c('', sprintf('%0.2f', r2.bi),   '')
    df$r2_part <- c('', sprintf('%0.2f', r2.part), '')
    return(df)
}



regressors <- c( 'semi_elas_cons', 'Cannibal', 'R_markup', 'R_priv_pct')
labels.C <- c('Intercept', 'Semi-elasticity',  'Cannibalization',
              'Gross restaurant markup', 'Privately optimal commission')
dep.var <- 'C_gap'
results.C.priv <- run.reg (dep.var, regressors, df.compare, weight.spec = 'by_f')


regressors <- c('semi_elas_J', 'Chg_Variety_scaled', 'Chg_FC_scaled', 'C_priv')
labels.R <- c('Intercept', 'Semi-elasticity', 'Variety change', 'Fixed cost change',
              'Privately optimal consumer fee')
dep.var <- 'R_gap'
results.R.priv <- run.reg (dep.var, regressors, df.compare, weight.spec = 'by_f')


tab.C <- process.tab(results.C.priv$tab, labels.C, results.C.priv$r2.bi,
                     results.C.priv$r2.part)

tab.R <- process.tab(results.R.priv$tab, labels.R, results.R.priv$r2.bi,
                     results.R.priv$r2.part)



# Save and place in the draft
write.dat(tab.C, outpath.C)
write.dat(tab.R, outpath.R)


