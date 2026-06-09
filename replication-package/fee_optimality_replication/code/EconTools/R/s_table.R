s.table <- function(x, K = NULL){
    y <- sort(table(x), decreasing = TRUE)
    if (!is.null(K)){
        y <- y[1:min(K, length(y))]
    }
    return(y)
}
