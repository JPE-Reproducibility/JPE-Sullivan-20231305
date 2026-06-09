weighted.quantile <- function(x, q, w, tol = 1e-5){

    max.iter <- 1e5
    interval <- c(min(x), max(x))
    x0 <- mean(interval)
    for (iter in 1:max.iter){
        mass <- weighted.mean(x <= x0, w)
        if (interval[2] - interval[1] < tol){
            break
        } else if (mass < q){
            interval <- c(x0, interval[2])
        } else {
            interval <- c(interval[1], x0)
        }
        x0 <- mean(interval)
    }
    return(x0)
}
