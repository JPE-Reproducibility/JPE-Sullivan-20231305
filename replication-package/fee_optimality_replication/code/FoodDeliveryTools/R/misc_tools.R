compute.nbuy <- function(buy.m){
    # Compute the number of observations in buy.m per zip code
    nbuy.df <- doBy::summaryBy(id ~ zip, data = buy.m, FUN = length)
    nbuy <- nbuy.df[, 2]
    names(nbuy) <- nbuy.df[, 1]
    return(nbuy)
}
