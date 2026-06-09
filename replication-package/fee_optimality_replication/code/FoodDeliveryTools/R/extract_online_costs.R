extract.online.costs <- function(x){
    if (length(x) > 1){
        C <- list()
        for (k in 2:length(x)){
            if (!is.null(x[[k]])){
                C[[as.character(k)]] <- x[[k]][2:nrow(x[[k]]), 1]
            }
        }
    } else {
        C <- NULL
    }
    return(C)
}
