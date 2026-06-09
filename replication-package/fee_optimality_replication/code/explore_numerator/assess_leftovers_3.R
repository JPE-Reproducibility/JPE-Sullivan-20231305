# Produce table investigating leftovers hypothesis
# (OA Table D2 in optimal_fees_OA.tex).

library(EconTools)
library(dplyr)

inpath.off7 <- 'output/explore_numerator/leftover_off_7.csv'
inpath.on7  <- 'output/explore_numerator/leftover_on_7.csv'
inpath.off3 <- 'output/explore_numerator/leftover_off_3.csv'
inpath.on3  <- 'output/explore_numerator/leftover_on_3.csv'

inpath.ybar <- 'output/explore_numerator/mean_depvar.csv'

outpath <- 'output/explore_numerator/leftovers.csv'

off7 <- read.dat(inpath.off7, skip = 1)
on7  <- read.dat(inpath.on7 , skip = 1)
off3 <- read.dat(inpath.off3, skip = 1)
on3  <- read.dat(inpath.on3 , skip = 1)
dvar <- read.dat(inpath.ybar, sep = '\t')

format.tab <- function(off7, suffix, ybar){
    rownames(off7) <- off7$X
    off7 <- off7[c('offline_order', 'online_order', 'N', 'N'), ]
    off7$X <- c('$\\text{direct}_{it}$',
                '$\\text{platform}_{it}$', 'Mean outcome', '$N$')
    b.a <- sprintf('%0.3f', c(off7$b[1:2], ybar))
    b.b <- sprintf('%d', off7$b[4])
    off7$b <- c(b.a, b.b)

    se.a <- c(sprintf('\\footnotesize (%0.4f)', off7$se[1:2]), '')
    se.b <- ''
    off7$se <- c(se.a, se.b)

    colnames(off7) <- c('X', paste0(colnames(off7)[2:3], suffix))

    return(off7)
}

off7 <- format.tab(off7, '_off7', dvar[1])
on7  <- format.tab(on7,  '_on7',  dvar[2])
off3 <- format.tab(off3, '_off3', dvar[3])
on3  <- format.tab(on3,  '_on3',  dvar[4])

tab <- dplyr::left_join(off7, off3, by = 'X')
tab <- dplyr::left_join(tab,  on7,  by = 'X')
tab <- dplyr::left_join(tab,  on3,  by = 'X')
tab[which(tab$X == '$N$'), 3:ncol(tab)] <- ''

write.dat(tab, outpath)

