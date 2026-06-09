# Produce a table of demand model parameter estimates
# that can be read into a LaTeX document

library(EconTools)
library(FoodDeliveryTools)

main <- function(){
    produce.table()
}

produce.table <- function(){

    # Load the estimates
    base.dir <- 'output/demand_estimation'
    inpath.tab <- sprintf('%s/est_SE_table-yipitdata.csv', base.dir)
    outpath.tab <- sprintf('%s/est_SE-yipitdata_formatted.csv', base.dir)
    tab <- read.dat(inpath.tab)

    # Make a table for inclusion in LateX documents
    idx.no.psi <- which(!grepl('^psi_', tab$theta_names))
    tab.sub <- tab[idx.no.psi, ]
    idx.no.mu <- which(!grepl('^mu_', tab.sub$theta_names))
    tab.sub <- tab.sub[idx.no.mu, ]

    # Formatting
    idx <- round(tab.sub$SE, 2) == 0
    est.a <- sprintf('%0.2f', tab.sub$est[!idx])
    est.b  <- sprintf('%0.3f', tab.sub$est[idx])
    tab.sub$est[!idx] <- est.a
    tab.sub$est[idx]  <- est.b

    SE.a <- sprintf('%0.2f', tab.sub$SE[!idx])
    SE.b <- sprintf('%0.3f', tab.sub$SE[idx])
    tab.sub$SE[idx] <- SE.b
    tab.sub$SE[!idx] <- SE.a

    # Ordering: primary parameters, lambdas, etas
    ## Eta parameters
    L  <- tab.sub[grep('^lambda', tab.sub$theta_names), ]
    idx.eta.lambda <- grepl('np_', tab.sub$theta_names)
    idx.eta.sd     <- grepl('(_eta|_idio)', tab.sub$theta_names)
    any.eta <- any(c(idx.eta.lambda, idx.eta.sd))
    ## Other parameters
    NL <- tab.sub[!grepl('^(lambda|alpha)', tab.sub$theta_names) & !idx.eta.lambda & !idx.eta.sd, ]
    Alpha <- tab.sub[grepl('^alpha', tab.sub$theta_names), ]
    tab.sub0 <- tab.sub
    tab.sub <- rbind(NL, L)
    tab.sub <- rbind(Alpha, tab.sub)
    if (any.eta){
        eta.params.lambda <- tab.sub0[idx.eta.lambda, ]
        eta.params.sd     <- tab.sub0[idx.eta.sd, ]
        tab.sub <- rbind(tab.sub, eta.params.sd, eta.params.lambda)
    }

    # Change names of various parameters
    ## Put Greek letters in math mode
    greek.letters <- paste0('^', c('alpha', 'gamma', 'lambda'))
    for (k in 1:nrow(tab.sub)){
        p <- tab.sub$theta_names[k]
        has.greek <- sapply(greek.letters, function(x) grepl(x, p))
        has.greek <- any(has.greek)
        if (has.greek){
            p <- sprintf('$\\%s$', p)
        }
        tab.sub$theta_names[k] <- p
    }

    # Fix etas
    if (any.eta){
        tab.sub$theta_names[which(tab.sub$theta_names == 'mu_eta')]     <- '$\\mu_{\\eta}$'
       if (any(tab.sub$theta_names == 'sd_idio')){
           tab.sub$theta_names[which(tab.sub$theta_names == 'sd_eta')]     <- '$\\sigma_{\\eta 1}$'
           tab.sub$theta_names[which(tab.sub$theta_names == 'sd_idio')]     <- '$\\sigma_{\\eta 2}$'

       } else {
           tab.sub$theta_names[which(tab.sub$theta_names == 'sd_eta')]     <- '$\\sigma_{\\eta}$'

       }
        tab.sub$theta_names[which(tab.sub$theta_names == 'np_young')]   <- '$\\lambda^{\\text{young}}_{\\eta}$'
        tab.sub$theta_names[which(tab.sub$theta_names == 'np_married')] <- '$\\lambda^{\\text{married}}_{\\eta}$'
        tab.sub$theta_names[which(tab.sub$theta_names == 'np_highinc')] <- '$\\lambda^{\\text{high income}}_{\\eta}$'
    }
    if ('tau' %in% tab.sub$theta_names){
        tab.sub$theta_names[which(tab.sub$theta_names == 'tau')] <- '$\\rho$'
    }

    # Fix price sensitivity suffixes
    tab.sub$theta_names <- sub('_low', '_{\\\\text{LowInc}}', tab.sub$theta_names)
    tab.sub$theta_names <- sub('alpha_young',   'alpha_{\\\\text{young}}',    tab.sub$theta_names)
    tab.sub$theta_names <- sub('alpha_married', 'alpha_{\\\\text{married}}',  tab.sub$theta_names)
    tab.sub$theta_names <- sub('alpha_highinc', 'alpha_{\\\\text{high inc}}', tab.sub$theta_names)

    # Fix taste for chains
    tab.sub$theta_names[which(tab.sub$theta_names == 'phi_chain')]   <- '$\\phi_{\\text{chain}}$'


    # Edit lambdas
    ## Demographic characteristics
    tab.sub$theta_names <- sub('lambda_1', 'lambda^{\\\\text{young}}',       tab.sub$theta_names)
    tab.sub$theta_names <- sub('lambda_2', 'lambda^{\\\\text{married}}',     tab.sub$theta_names)
    tab.sub$theta_names <- sub('lambda_3', 'lambda^{\\\\text{high income}}', tab.sub$theta_names)
    ## Platform names
    tab.sub$theta_names <- sub('-1', '_{\\\\text{DD}}',   tab.sub$theta_names)
    tab.sub$theta_names <- sub('-2', '_{\\\\text{Uber}}', tab.sub$theta_names)
    tab.sub$theta_names <- sub('-3', '_{\\\\text{GH}}',   tab.sub$theta_names)
    tab.sub$theta_names <- sub('-4', '_{\\\\text{PM}}',   tab.sub$theta_names)

    if (!any(grepl('text{DD}', tab.sub$theta_names, fixed = TRUE))){
        idx <- grepl('lambda', tab.sub$theta_names) &
               !grepl('\\eta', tab.sub$theta_names, fixed = TRUE)
        tab.sub$theta_names[idx] <- sub('\\$$', '_{\\\\text{platform}}$',
                                        tab.sub$theta_names[idx])
    }
    
    tab.sub$SE <- sprintf('\\small (%s)', tab.sub$SE)

    # Edit standard deviations
    tab.sub$theta_names[which(tab.sub$theta_names == 'sd_zeta1')] <- '$\\sigma_{\\zeta1}$'
    tab.sub$theta_names[which(tab.sub$theta_names == 'sd_zeta2')] <- '$\\sigma_{\\zeta2}$'
    tab.sub$theta_names[which(tab.sub$theta_names == 'sd_phi')]   <- '$\\sigma_{\\phi}$'

    write.dat(tab.sub, file = outpath.tab)

    ## Take out demo effects
    nondemo <- tab.sub[!grepl('lambda', tab.sub$theta_names), ]
    demo    <- tab.sub[grepl('lambda', tab.sub$theta_names), ]
    outpath.nd <- sub('.csv', '_nondemo.csv', outpath.tab)
    outpath.d  <- sub('.csv', '_demo.csv',    outpath.tab)
    write.dat(nondemo, file = outpath.nd)
    write.dat(demo,    file = outpath.d)
}

main()

