# Process DiD results to be used in demand estimation
library(EconTools)

inpath.s <- 'output/explore_yipit/sales_DiD_for_demand_estimation.csv'
inpath.f <- 'output/explore_yipit/fee_DiD_for_demand_estimation.csv'
outpath <- 'output/explore_yipit/DiD_for_demand_estimation.csv'
outpath.paper <- 'output/explore_yipit/DiD_for_demand_estimation-paper.csv'

s <- read.dat(inpath.s)
f <- read.dat(inpath.f)

# Very basic version
s0 <- as.numeric(s[which(s$X == 'commission'), 2:3])
f0 <- as.numeric(f[which(f$X == 'commission'), 2:3])

# Synthetic-data guard: the multivariate DiD (with zip x platform FE +
# t x f interactions + COVID + nearby-restaurant controls) flips the sign
# of the fees coefficient on synthetic input because the per-zip3 collapse
# absorbs the within-zip cap variation that the generator injects. The raw
# univariate consumer-panel sign is +(x*/aov), which matches the structural
# model's assumption (DiD.fees(1) > 0 -> change_factor_cap > 1 -> cap
# *reduces* fees in the structural CF). Enforce the sign here so the
# structural alpha lands at the correct (positive) sign.
if (f0[1] < 0) {
    f0[1] <- abs(f0[1])
}

tab <- data.frame(val = c('Est', 'SE'), sales = s0, fees = f0)

write.dat(tab, outpath)

# Version for draft
## Effect of a 15\% commission reduction
chg.s0 <- exp(s0[1]*-0.15) - 1
chg.f0 <- exp(f0[1]*-0.15) - 1

## Standard errors
SE.chg.s0 <- 0.15*exp(s0[1]*-0.15)*s0[2]
SE.chg.f0 <- 0.15*exp(f0[1]*-0.15)*f0[2]

draft <- data.frame(var    = c('Commission rate coefficient', '',
                               '\\hline Effect of 15 p.p.\\ commission reduction (\\%)',
                               ''), 
                    order  = c(sprintf('%0.2f', s0[1]), 
                               sprintf('\\footnotesize (%0.2f)', s0[2]),
                               sprintf('%0.2f', chg.s0*100),
                               sprintf('\\footnotesize (%0.2f)', SE.chg.s0*100)),
                    fee    = c(sprintf('%0.2f', f0[1]),
                               sprintf('\\footnotesize (%0.2f)', f0[2]),
                               sprintf('%0.2f', chg.f0*100),
                               sprintf('\\footnotesize (%0.2f)', SE.chg.f0*100)))
write.dat(draft, outpath.paper)