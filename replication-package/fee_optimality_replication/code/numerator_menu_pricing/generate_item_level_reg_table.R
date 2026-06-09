# Results from menu-item-level regressions
library(EconTools)


main <- function(){
    generate.table('')
    generate.table('_allsample')
}

generate.table <- function(suffix){

    # Specify paths
    base.dir <- 'output/numerator_menu_pricing/disagg_results'
    inpath.cap  <- sprintf('%s/TWFE_discr%s.csv', base.dir, suffix)
    inpath.comm <- sprintf('%s/TWFE_conti%s.csv', base.dir, suffix)
    ### VCOV
    inpath.cap.vc  <- sprintf('%s/TWFE_discr_vcov%s.csv', base.dir, suffix)
    inpath.comm.vc <- sprintf('%s/TWFE_conti_vcov%s.csv', base.dir, suffix)

    ## Path to output
    outpath.combo <- sprintf('%s/pricing_reg_combo%s.csv', base.dir, suffix)

    cap  <- read.dat(inpath.cap)
    comm <- read.dat(inpath.comm)

    cap.vc  <- process.vcov(inpath.cap.vc)
    comm.vc <- process.vcov(inpath.comm.vc)

    #== Commission cap ==#
    base.reg <- c('online', '1.has_cap', '1.has_cap#1.online')
    regressors <- c(base.reg, 'N')

    estimates <- list()
    for (x in regressors){
        idx <- grep(x, cap[, 1])[1]
        idx.se <- idx + 1
        estimates[[x]] <- c(cap[idx, 2], cap[idx.se, 2])
    }
    estimates <- as.data.frame(do.call(rbind, estimates))
    estimates$V1 <- gsub('[=*]+', '', estimates$V1)
    estimates$V2 <- gsub('[=()]+', '', estimates$V2)

    colnames(estimates) <- c('est', 'se')
    estimates$est <- sprintf('%0.4f', as.numeric(estimates$est))
    estimates$se <- sprintf('\\footnotesize (%0.4f)', as.numeric(estimates$se))

    base.labels <- c('Online platform',
                     'Commission cap',
                     'Commission cap $\\times$ online')
    estimates$var <- c(base.labels, '$N$')

    # Predicted online/offline ratio for online under 15% and 30% commissions
    phi.f <- as.numeric(estimates$est[which(estimates$var == 'Online platform')])
    gamma <- as.numeric(estimates$est[which(estimates$var == 'Commission cap $\\times$ online')])
    yhat.30 <- exp(phi.f)
    yhat.15 <- exp(phi.f + gamma)

    ## Standard errors
    idx.1 <- which(colnames(cap.vc) == 'online')
    idx.2 <- grep('1.has_cap.1.online', colnames(cap.vc))
    ### No cap
    G <- matrix(0, nrow = 1, ncol = nrow(cap.vc))
    G[1, idx.1]  <- exp(phi.f)
    SE.30 <- sqrt(G%*%cap.vc%*%t(G))
    ### Under cap
    G <- matrix(0, nrow = 1, ncol = nrow(cap.vc))
    G[1, idx.1] <- exp(phi.f + gamma)
    G[1, idx.2] <- exp(phi.f + gamma)
    SE.15 <- sqrt(G%*%cap.vc%*%t(G))

    formatted <- estimates[1:length(base.labels), ]
    n.row <- estimates['N', , drop = FALSE]
    n.row$se <- ''
    panel2 <- data.frame(est = sprintf('%0.3f', c(yhat.30, yhat.15)),
                         se  = sprintf('\\footnotesize (%0.3f)', c(SE.30,   SE.15)),
                         var = c('Online/offline ratio (30\\% comm.)',
                                 'Online/offline ratio (15\\% comm.)'))
    formatted <- dplyr::bind_rows(formatted, panel2)
    formatted <- dplyr::bind_rows(formatted, n.row)
    formatted$lineflag <- c(0, 0, 0, 1, 0, 1)

    #== Commission level ==#
    base.reg <- c('online', 'cap', 'online_cap')
    regressors <- c(base.reg, 'N')
    
    estimates.comm <- list()
    for (x in regressors){
        idx <- grep(x, comm[, 1])[1]
        idx.se <- idx + 1
        estimates.comm[[x]] <- c(comm[idx, 2], comm[idx.se, 2])
    }
    estimates.comm <- as.data.frame(do.call(rbind, estimates.comm))
    estimates.comm$V1 <- gsub('[=*]+', '', estimates.comm$V1)
    estimates.comm$V2 <- gsub('[=()]+', '', estimates.comm$V2)

    colnames(estimates.comm) <- c('est', 'se')
    estimates.comm$est <- sprintf('%0.4f', as.numeric(estimates.comm$est))
    estimates.comm$se <- sprintf('\\footnotesize (%0.4f)', as.numeric(estimates.comm$se))
    base.labels <- c('Online platform',
                     'Commission rate',
                     'Commission rate $\\times$ online')
    estimates.comm$var <- c(base.labels, '$N$')

    # Predicted online/offline ratio for DD under 15% and 30% commissions
    phi.f <- as.numeric(estimates.comm$est[which(estimates.comm$var == 'Online platform')])
    gamma <- as.numeric(estimates.comm$est[which(estimates.comm$var == 'Commission rate $\\times$ online')])
    yhat.30 <- exp(phi.f + gamma*0.30)
    yhat.15 <- exp(phi.f + gamma*0.15)


    ## Standard errors
    idx.1 <- grep('online',     colnames(comm.vc))
    idx.2 <- grep('online_cap', colnames(comm.vc))
    rate.1 <- 0.30
    rate.2 <- 0.15
    ### No cap
    G <- matrix(0, nrow = 1, ncol = nrow(comm.vc))
    G[1, idx.1] <- exp(phi.f + gamma*rate.1)
    G[1, idx.2] <- exp(phi.f + gamma*rate.1)*rate.1
    SE.30 <- sqrt(G%*%comm.vc%*%t(G))
    ### Cap
    G <- matrix(0, nrow = 1, ncol = nrow(comm.vc))
    G[1, idx.1] <- exp(phi.f + gamma*rate.2)
    G[1, idx.2] <- exp(phi.f + gamma*rate.2)*rate.2
    SE.15 <- sqrt(G%*%comm.vc%*%t(G))

    formatted.comm <- estimates.comm[base.reg, ]
    n.row <- estimates.comm['N', , drop = FALSE]
    n.row$se <- ''
    panel2 <- data.frame(est = sprintf('%0.3f', c(yhat.30, yhat.15)),
                         se  = sprintf('\\footnotesize (%0.3f)', c(SE.30,   SE.15)),
                         var = c('Online/offline ratio (30\\% comm.)',
                                 'Online/offline ratio (15\\% comm.)'))
    formatted.comm <- dplyr::bind_rows(formatted.comm, panel2)
    formatted.comm <- dplyr::bind_rows(formatted.comm, n.row)

    # Combine the tables
    colnames(formatted.comm) <- c('estComm', 'seComm', 'var')
    formatted <- dplyr::full_join(formatted, formatted.comm, by = 'var')

    formatted$est[is.na(formatted$est)]         <- '-'
    formatted$se[is.na(formatted$se)]           <- '-'
    formatted$estComm[is.na(formatted$estComm)] <- '-'
    formatted$seComm[is.na(formatted$seComm)]   <- '-'

    formatted$lineflag[is.na(formatted$lineflag)] <- 0

    rownames(formatted) <- formatted$var
    row.ord <- c('Online platform', 'Commission rate',
                 'Commission rate $\\times$ online', 'Commission cap',
                 'Commission cap $\\times$ online',
                 'Online/offline ratio (30\\% comm.)',
                 'Online/offline ratio (15\\% comm.)')
    formatted <- formatted[row.ord, ]

    write.dat(formatted, outpath.combo)
}

process.vcov <- function(inpath.vc){
    # Read and process the vcov matrix in "inpath.vc"
    vc  <- read.dat(inpath.vc)
    vc.nm <- vc[, 1]
    vc <- as.matrix(vc[, 2:ncol(vc)])
    rownames(vc) <- vc.nm
    return(vc)
}

main()
