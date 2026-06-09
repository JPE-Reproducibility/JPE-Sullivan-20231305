load.equilibria.priv.soc <- function(CF.dir, cbsa.codes, monopoly = FALSE){
    # Load equilibria
    CF.all <- list.files(CF.dir)
    if (monopoly){
        CF.BL  <- grep('^sub1000_baseline_[a-z]*\\.rds', CF.all, value = TRUE)
        CF.soc <- grep('^sub1000_socopt_[a-z]*\\.rds',   CF.all, value = TRUE)
    } else {
        CF.BL  <- grep('^baseline_[a-z]*\\.rds', CF.all, value = TRUE)
        CF.soc <- grep('^socopt_[a-z]*\\.rds',   CF.all, value = TRUE)
    }

    paths.BL  <- sprintf('%s/%s', CF.dir, CF.BL)
    paths.soc <- sprintf('%s/%s', CF.dir, CF.soc)

    codes.BL  <- sub('(sub1000_)?baseline_', '', sub('.rds', '', CF.BL))
    codes.soc <- sub('(sub1000_)?socopt_', '', sub('.rds', '', CF.soc))

    mkts.BL  <- cbsa.codes[codes.BL,  'CBSA_name']
    mkts.soc <- cbsa.codes[codes.soc, 'CBSA_name']

    # A missing market file must raise a hard error, not silently shrink
    # the set of markets entering the analysis
    assert.complete.names(mkts.BL, mkts.soc,
                          what = sprintf('baseline equilibria in %s (markets)', CF.dir),
                          hint = 'Re-run the CF solvers for this spec.')
    assert.complete.names(mkts.soc, mkts.BL,
                          what = sprintf('socially-optimal equilibria in %s (markets)', CF.dir),
                          hint = 'Re-run the CF solvers for this spec.')

    BL  <- lapply(paths.BL,  readRDS)
    soc <- lapply(paths.soc, readRDS)
    names(BL)  <- mkts.BL
    names(soc) <- mkts.soc

    outputs <- list(BL = BL, soc = soc)
    return(outputs)
}
