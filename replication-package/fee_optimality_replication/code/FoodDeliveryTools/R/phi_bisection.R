phi.bisection <- function(dat, rhos, opts, interval = c(0.0, 1.5), tol = 1e-3, b = NULL){
    # Estimate the phi parameter
    # b is the seed used in taking bootstrap draws of observations in the moment
    # condition

    max.iter <- 20

    converged <- FALSE
    for (iter in 1:max.iter){
        phi <- mean(interval)
        obj <- phi.objective.v2(phi, dat, rhos, opts, b = b)
        fval <- obj$fval

        pracma::fprintf('Iteration %d: phi = %f, moment value = %f\n',
                        iter, phi, fval)
        if (abs(fval) < tol){
            converged <- TRUE
            break
        } else if (fval < 0){
            interval <- c(phi, interval[2])
        } else {
            interval <- c(interval[1], phi)
        }
    }

    if (!converged){
        warning(sprintf(paste0('phi.bisection did not converge: |moment| = %g ',
                               'after %d iterations (tol = %g). The moment may ',
                               'not change sign over the search interval; do ',
                               'not use the returned phi without investigating.'),
                        abs(fval), max.iter, tol))
    }

    outputs <- list()
    outputs$phi <- phi
    outputs$costs <- obj$costs
    return(outputs)
}
