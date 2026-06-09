# Produce a table describing the estimates of the pricing friction parameter
library(Matrix)
library(FoodDeliveryTools)
library(EconTools)

# Specify paths
inpath   <- 'output/recover_restaurant_costs/retaurant_costs.rds'
outpath  <- 'output/recover_restaurant_costs/MC_table.csv'

if (!file.exists(inpath)){
    stop(sprintf("required input missing: %s\n", inpath),
         "Run code/recover_restaurant_costs/recover_restaurant_costs_v2.R first.")
}
est <- readRDS(inpath)

phi <- est$phi


## Compute SE
boot.dir <- 'data/bootstrap/dat/restoMC'
if (!dir.exists(boot.dir)){
    stop(sprintf("friction bootstrap directory missing: %s\n", boot.dir),
         "Run code/bootstrap_restaurant_costs/bootstrap_friction.R first.")
}
inpaths.boot <- list.files(boot.dir)
inpaths.boot <- grep('friction', inpaths.boot, value = TRUE)
if (length(inpaths.boot) == 0){
    stop(sprintf("no friction bootstrap files found in %s\n", boot.dir),
         "Run code/bootstrap_restaurant_costs/bootstrap_friction.R first.")
}
b <- sub('^[a-z_]+', '', inpaths.boot)
b <- as.numeric(sub('.rds', '', b))
inpaths.boot <- sprintf('%s/%s', boot.dir, inpaths.boot)
boot.results <- lapply(inpaths.boot, readRDS)
phi.results <- sapply(boot.results, function(x) x$phi)

# Generate table
phi.tab <- data.frame(var = c('Estimate', 'SE', '2.5th percentile', '97.5th percentile',
                              'CI lower bound', 'CI upper bound'),
                      val = c(phi, sd(phi.results), 
                              quantile(phi.results, c(0.025, 0.975)),
                              phi - 1.96*sd(phi.results), 
                              phi + 1.96*sd(phi.results)))


write.dat(phi.tab, outpath)
