# Decompose platforms' average consumer fees into delivery, service,
# and regulatory-response components (OA Table F1 in optimal_fees_OA.tex).
library(EconTools)

inpath <- 'data/prices/price_indices_v3.csv'
prices <- read.csv(inpath, stringsAsFactors = FALSE)

# Service fees in `prices` are stored as rates; convert to dollars by applying
# the $30 reference basket size used throughout the paper. The Regulatory
# Response row mixes platform-specific columns because each platform reports
# regulatory fees under different field names.
decomp.dd <- c(mean(prices$dfee_dd_lasso),
               mean(prices$dd.service.fee)*30,
               mean(prices$dd.regulatory))
decomp.uber <- c(mean(prices$dfee_uber_lasso),
                 mean(prices$uber.service.fee)*30,
                 mean(prices$uber.driver + prices$uber.local))
decomp.gh   <- c(mean(prices$dfee_gh_lasso),
                 mean(prices$gh.service.fee)*30,
                 mean(prices$gh.driver))
decomp.pm   <- c(mean(prices$dfee_pm_lasso),
                 mean(prices$pm.service.fee)*30,
                 mean(prices$pm.driver + prices$pm.regulatory))

decomp <- data.frame(var = c('Delivery', 'Service', 'Regulatory Response'),
                     DD = decomp.dd, Uber = decomp.uber,
                     GH = decomp.gh, PM = decomp.pm)

for (k in setdiff(colnames(decomp), 'var')){
    decomp[, k] <- sprintf('%0.2f', decomp[, k])
}
write.dat(x = decomp, file = 'output/web_harvest_descriptives/fee_decomp.csv')
