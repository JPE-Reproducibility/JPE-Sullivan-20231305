# Produce a table describing estimates of platforms' marginal costs and markups
library(Matrix)
library(FoodDeliveryTools)
library(EconTools)

# Specify paths
nsim.suffix <- '_nsim50'
base.dir <- sprintf('output/estimate_platform_costs/MC_H/spec%s', nsim.suffix)
outpath <- sprintf('%s/MC_results.csv', base.dir)

MC.H.paths <- list.files(base.dir)
MC.H.paths <- grep('^[a-z]+\\_full\\.rds$', MC.H.paths, value = TRUE)

inpaths <- collect.inpaths(nsim.suffix)
cbsa <- read.dat(inpaths$inpath.cbsa.codes)

MCs <- list()
Mus <- list()
for (k in 1:length(MC.H.paths)){
    path.k <- sprintf('%s/%s', base.dir, MC.H.paths[k])
    cbsa.k <- sub('_full.rds', '', MC.H.paths[k])

    MC.k <- readRDS(path.k)
    MCs[[k]] <- MC.k$MC.no.h
    Mus[[k]] <- MC.k$Mu
}

MC.mat <- do.call(rbind, MCs)
Mu.mat <- do.call(rbind, Mus)

# Summary of MCs and markups
tab.rows <- list()
platforms <- c('DD', 'Uber', 'GH', 'PM')
NF <- length(platforms)
for (f in 1:NF){
    # Extract estimates
    mc.f <- MC.mat[, f]
    mu.f <- Mu.mat[, f]
    # Marginal costs
    mean.MC <- mean(mc.f)
    QT.MC   <- quantile(mc.f, c(0.25, 0.5, 0.75))
    # Markups
    mean.MK <- mean(mu.f)
    QT.MK   <- quantile(mu.f, c(0.25, 0.5, 0.75))
    # Collate results
    tab.rows[[f]] <- c(mean.MC, QT.MC, mean.MK, QT.MK)
}
tab <- Reduce(rbind, tab.rows)

# Formatting
cnames <- c('mcMean', 'mc1', 'mc2', 'mc3', 'mkMean', 'mk1', 'mk2', 'mk3')
tab <- as.data.frame(tab)
colnames(tab) <- cnames
for (k in cnames){
    tab[, k] <- sprintf('%.2f', tab[, k])
}
tab$platform <- platforms

write.dat(tab, outpath)

