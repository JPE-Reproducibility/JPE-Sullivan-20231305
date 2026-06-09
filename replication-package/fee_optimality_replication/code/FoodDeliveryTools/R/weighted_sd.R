weighted.sd <- function(x, w){
    mu.x <- weighted.mean(x, w)
    integrand <- (x - mu.x)^2
    var.x <- weighted.mean(integrand, w)
    sd.x <- sqrt(var.x)
    return(sd.x)
}
