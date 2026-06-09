# Produce a table showing the CCP-GMM restaurant FC estimates
library(pracma)
library(FoodDeliveryTools)
library(EconTools)
library(scales)

include.PM <- TRUE
use.platform.names <- TRUE

nsim.suffix <- '_nsim50'

inpaths <- collect.inpaths(nsim.suffix)

## to data
base.dir <- 'output/restaurant_FC_estimation'

suffix <- ifelse(include.PM, '', '_excludePM')

outpath.kappa <- sub('\\.rds', '_kappas\\.csv', inpaths$inpath.FC.est)
outpath.sigma <- sub('\\.rds', '_sigmas\\.csv', inpaths$inpath.FC.est)
outpath.kappa <- sub('.csv', sprintf('%s.csv', suffix), outpath.kappa)
outpath.sigma <- sub('.csv', sprintf('%s.csv', suffix), outpath.sigma)
outpath.plot  <- sprintf('%s/cost_by_size%s.pdf', base.dir, suffix)
outpath.pool  <- sprintf('%s/cost_by_size_pool%s.pdf', base.dir, suffix)

inpath.rev <- sub('EPi', 'Rev', inpaths$inpath.pi)

# Load data and estimates
dat <- readRDS(inpaths$inpath.dat)
est <- readRDS(inpaths$inpath.FC.est)
EPi <- readRDS(inpaths$inpath.pi)
Rev <- readRDS(inpath.rev)

markets <- names(est$K)
G.vars <- grep('^G', colnames(dat[[1]]$resto.m), value = TRUE)

sigma.hat <- est$sigma.hat
nportfolios <- length(est$K.vals[[1]]$chain)
K.c <- lapply(markets, function(market) est$K.vals[[market]]$chain[2:nportfolios])
K.i <- lapply(markets, function(market) est$K.vals[[market]]$indep[2:nportfolios])
names(K.c) <- markets
names(K.i) <- markets

param.c <- as.data.frame(Reduce(rbind, K.c))
param.i <- as.data.frame(Reduce(rbind, K.i))
param.all <- rbind(param.c, param.i)

dat.m <- dat[[1]]
if (!include.PM){
    idx.keep <- which(dat.m$G.mat[2:dat.m$nportfolios, dat.m$nplatforms] == 0)
    nsizes <- 3
} else {
    idx.keep <- which(dat.m$G.mat[2:dat.m$nportfolios, 1] == 1)
    nsizes <- 4
}

param.c   <- param.c[, idx.keep]
param.i   <- param.i[, idx.keep]
param.all <- param.all[, idx.keep]

# Try weighting by the number of restaurants
## Within each portfolio, determine weights by restaurant counts
Wgt.c <- matrix(0, nrow = nrow(param.c), ncol = ncol(param.c))
Wgt.i <- matrix(0, nrow = nrow(param.c), ncol = ncol(param.c))
for (m in 1:length(markets)){
    market <- markets[m]
    dat.m <- dat[[market]]
    J.c <- dat.m$J.G.c.m
    J.i <- dat.m$J.G.i.m
    ## Aggregation
    J.c <- colSums(do.call(rbind, J.c))
    J.i <- colSums(do.call(rbind, J.i))

    Wgt.c[m, ] <- J.c[2:length(J.c)][idx.keep]
    Wgt.i[m, ] <- J.i[2:length(J.i)][idx.keep]
}

# Average Ks
avg.K.c <- 100*colSums(param.c*Wgt.c)/colSums(Wgt.c)
avg.K.i <- 100*colSums(param.i*Wgt.i)/colSums(Wgt.i)
avg.K <- 100*(colSums(param.c*Wgt.c) + colSums(param.i*Wgt.i))/(colSums(Wgt.c) + colSums(Wgt.i))

G.size <- sapply(strsplit(sub('G', '', G.vars), ''), function(x) sum(as.numeric(x)))
G.size <- G.size[2:length(G.size)][idx.keep]
avg.by.size.c <- sapply(1:nsizes, function(k) weighted.mean(avg.K.c[G.size == k], colSums(Wgt.c)[G.size == k]))
avg.by.size.i <- sapply(1:nsizes, function(k) weighted.mean(avg.K.i[G.size == k], colSums(Wgt.i)[G.size == k]))

wgt.c <- sapply(1:nsizes, function(k) sum(Wgt.c[, G.size == k]))
wgt.i <- sapply(1:nsizes, function(k) sum(Wgt.i[, G.size == k]))
wgt.tot <- wgt.c + wgt.i
avg.by.size <- avg.by.size.c*(wgt.c/wgt.tot) + avg.by.size.i*(wgt.i/wgt.tot)

# Standard errors from bootstrap
boot.dir <- 'data/bootstrap/dat/restoFC'
FC.files <- grep('_FC_', list.files(boot.dir), value = TRUE)

sigma.boot <- list()

avg.K.c.boot <- list()
avg.K.i.boot <- list()
avg.K.boot   <- list()

avg.by.size.c.boot <- list()
avg.by.size.i.boot <- list()
avg.by.size.boot   <- list()

FC.indep <- list()
FC.chain <- list()
for (fc.file in FC.files){
    b <- gsub('[a-zA-Z_\\.]', '', fc.file)
    inpath.boot <- sprintf('%s/%s', boot.dir, fc.file)
    FC.b <- readRDS(inpath.boot)
    FC.indep[[fc.file]] <- sapply(FC.b$K.vals, function(x) x$indep[2]*1e5)
    FC.chain[[fc.file]] <- sapply(FC.b$K.vals, function(x) x$chain[2]*1e5)
}

FC.indep <- Reduce(cbind, FC.indep)
FC.chain <- Reduce(cbind, FC.chain)


for (fc.file in FC.files){
    b <- gsub('[a-zA-Z_\\.]', '', fc.file)
    inpath.boot <- sprintf('%s/%s', boot.dir, fc.file)
    FC.b <- readRDS(inpath.boot)
    sigma.boot[[b]] <- FC.b$sigma.hat

    chain.K <- lapply(markets, function(x) FC.b$K.vals[[x]]$chain)
    indep.K <- lapply(markets, function(x) FC.b$K.vals[[x]]$indep)

    # Average Ks
    param.c.b <- as.data.frame(Reduce(rbind, chain.K))
    param.i.b <- as.data.frame(Reduce(rbind, indep.K))
    param.c.b <- param.c.b[, 2:ncol(param.c.b)][, idx.keep]
    param.i.b <- param.i.b[, 2:ncol(param.i.b)][, idx.keep]

    avg.K.c.boot[[b]] <- 100*colSums(param.c.b*Wgt.c)/colSums(Wgt.c)
    avg.K.i.boot[[b]] <- 100*colSums(param.i.b*Wgt.i)/colSums(Wgt.i)
    avg.K.boot[[b]]   <- 100*(colSums(param.c.b*Wgt.c) + colSums(param.i.b*Wgt.i))/(colSums(Wgt.c) + colSums(Wgt.i))

    avg.by.size.c.boot[[b]] <- sapply(1:nsizes, function(k) weighted.mean(avg.K.c.boot[[b]][G.size == k], colSums(Wgt.c)[G.size == k]))
    avg.by.size.i.boot[[b]] <- sapply(1:nsizes, function(k) weighted.mean(avg.K.i.boot[[b]][G.size == k], colSums(Wgt.i)[G.size == k]))
    avg.by.size.boot[[b]]   <- avg.by.size.c.boot[[b]]*(wgt.c/wgt.tot) + avg.by.size.i.boot[[b]]*(wgt.i/wgt.tot)
}

sigma.se <- Reduce(rbind, sigma.boot)
sigma.se <- apply(sigma.se, MARGIN = 2, FUN = sd)

avg.K.c.df <- Reduce(rbind, avg.K.c.boot)
avg.K.c.se <- apply(avg.K.c.df, MARGIN = 2, FUN = sd)

avg.K.i.df <- Reduce(rbind, avg.K.i.boot)
avg.K.i.se <- apply(avg.K.i.df, MARGIN = 2, FUN = sd)

avg.K.df <- Reduce(rbind, avg.K.boot)
avg.K.se <- apply(avg.K.df, MARGIN = 2, FUN = sd)

avg.by.size.c.df <- Reduce(rbind, avg.by.size.c.boot)
avg.by.size.c.se <- apply(avg.by.size.c.df, MARGIN = 2, FUN = sd)

avg.by.size.i.df <- Reduce(rbind, avg.by.size.i.boot)
avg.by.size.i.se <- apply(avg.by.size.i.df, MARGIN = 2, FUN = sd)

avg.by.size.df <- Reduce(rbind, avg.by.size.boot)
avg.by.size.se <- apply(avg.by.size.df, MARGIN = 2, FUN = sd)

# Make table with Ks
gvar.names <- c('DD',           'Uber',         'GH',     'PM',
                'DD, Uber',     'DD, GH',       'DD, PM', 'Uber, GH', 'Uber, PM', 'GH, PM',
                'DD, Uber, GH', 'DD, Uber, PM', 'DD, GH, PM', 'Uber, GH, PM', 'All')[idx.keep]
sub.idx <- which(gvar.names %in% c('DD', 'DD, Uber', 'DD, Uber, GH', 'All'))

K.tab <- data.frame(var    = gvar.names,
                    K_tot  = round(avg.K*1e3),
                    SE_tot = sprintf('\\footnotesize (%0.0f)', avg.K.se*1e3))

# Make table with Sigmas
sigma.SEs <- sprintf('\\footnotesize (%0.0f)', 100*sigma.se*1e3)

sigma.vals <- sprintf('%0.0f', sigma.hat*100*1e3)
sigma.vars <- c('$\\sigma_{\\omega}$',
                '$\\sigma_{rc}$')

SE.tab <- data.frame(var = sigma.vars,
                     val = sigma.vals,
                     se  = sigma.SEs)

write.dat(K.tab, file = outpath.kappa, sep = ';')
write.dat(SE.tab, outpath.sigma)


# Plot dimensions
W <- 4.3
H <- W

# Average by size plot
pdf(outpath.plot, W, H)
col.i <- 'grey50'
y.c <- c(0, as.numeric(avg.by.size.c*1e3))
y.i <- c(0, as.numeric(avg.by.size.i*1e3))

# Choose the y limit
ylim <- c(-0.15, 2.0)*1e3

plot(0:nsizes, y.c, ylim = ylim, type = 'l',
     lty = 3, lwd = 2, axes = FALSE,
     xlab = 'Number of platforms joined',
     ylab = 'Mean fixed cost ($)')
grid()
abline(h = 0)
points(0:nsizes, y.c, pch = 16, cex = 1.2)
lines(0:nsizes,  y.i, lty = 4, lwd = 2, col = col.i)
points(0:nsizes, y.i, pch = 18, col = col.i, cex = 1.6)

z <- qnorm(0.975)
w <- 0.07
for (k in 1:nsizes){
    se.c <- avg.by.size.c.se[k]*1e3
    yk <- y.c[k + 1]
    col.c.bar <- alpha('black', alpha = 0.5)
    segments(x0 = k, y0 = yk - z*se.c, y1 = yk + z*se.c,
             lwd = 2, col = col.c.bar)
    segments(x0 = k - w, x1 = k + w, y0 = yk - z*se.c,
             lwd = 2, col = col.c.bar)
    segments(x0 = k - w, x1 = k + w, y0 = yk + z*se.c,
             lwd = 2, col = col.c.bar)

    se.i <- avg.by.size.i.se[k]*1e3
    yk <- y.i[k + 1]

    col.i.bar <- alpha(col.i, alpha = 0.5)
    segments(x0 = k, y0 = yk - z*se.i, y1 = yk + z*se.i,
             lwd = 2, col = col.i.bar)
    segments(x0 = k - w, x1 = k + w, y0 = yk - z*se.i,
             lwd = 2, col = col.i.bar)
    segments(x0 = k - w, x1 = k + w, y0 = yk + z*se.i,
             lwd = 2, col = col.i.bar)
}
axis(1, at = 0:4, labels = 0:4)
axis(2)

legend(x = 'topleft', legend = c('Chain', 'Indep.'),
       col = c('black', col.i), pch = c(16, 18),
       bg = 'white')
dev.off()



# Average by size plot without chain/independent distinction
pdf(outpath.pool, W*1.1, H*1.1)
par(mar = c(4, 5, 1.5, 1.5))
y <- c(0, as.numeric(avg.by.size*1e3))
ylim <- c(-0.15, 4.0)*1e3

plot(0:nsizes, y, ylim = ylim, type = 'l',
     lty = 2, lwd = 2, axes = FALSE,
     xlab = 'Number of platforms joined',
     ylab = 'Mean fixed cost ($)')
grid()
abline(h = 0)
points(0:nsizes, y, pch = 16, cex = 1.2)
z <- qnorm(0.975)

w <- 0.07
col.bar <- 'grey50'
lty.bar <- 3
for (k in 1:nsizes){
    se <- avg.by.size.se[k]*1e3
    yk <- y[k + 1]
    segments(x0 = k, y0 = yk - z*se, y1 = yk + z*se,
             lwd = 2, col = col.bar, lty = lty.bar)
}
axis(1, at = 0:nsizes, labels = 0:nsizes)
axis(2)

dev.off()

# Average monthly revenues
Rev.mats <- lapply(Rev, function(x) do.call(rbind, x))
J.mats <- lapply(dat, function(x) do.call(rbind, x$J.G.1.m))
Rev.mat <- do.call(rbind, Rev.mats)
J.mat   <- do.call(rbind, J.mats)

common <- intersect(rownames(J.mat), rownames(Rev.mat))
Rev.mat <- Rev.mat[common, ]
J.mat   <- J.mat[common, ]

g <- 16
monthly.rev <- weighted.mean(Rev.mat[, g], J.mat[, g])*1e5

