load.num.param <- function(...){
    # Load a default list of numerical parameters used throughout
    # the code
    num.param <- list(max.iter.p      = 1e5,  # Maximum iterations for pricing eqm
                      tol.p           = 0.01, # Tolerance for pricing eqm
                      tol.pr          = 0.1,   # Tolerance for commission rate (in % points)
                      max.iter.FP     = 100,
                      tol.FP          = 0.0001,
                      learn.rate.FP   = 0.80,   # Learning rate for fixed point
                      learn.rate.p    = 0.50,   # Learning rate for pricing eqm
                      learn.rate.menu = 1.00, # Learning rate for menu pricing
                      max.iter.menu   = 100,
                      tol.menu        = 0.01,
                      n.multi.sim     = 100,
                      pi.eps          = 0.02,
                      p.c.eps         = 1e-4,
                      p.r.eps         = 1e-4,
                      step.size.p     = 1e-8,
                      step.size.pr    = 0.01,
                      tol.pc.eqm      = 0.005,
                      max.iter.pc.eqm = 1000,
                      learn.rate.pc.eqm = 0.1,
                      tol.pr.eqm = 0.01,
                      max.iter.pr.eqm = 100,
                      learn.rate.pr.eqm = 0.1)

    x <- list(...)
    for (nm in names(x)){
        num.param[[nm]] <- x[[nm]]
    }
    return(num.param)
}
