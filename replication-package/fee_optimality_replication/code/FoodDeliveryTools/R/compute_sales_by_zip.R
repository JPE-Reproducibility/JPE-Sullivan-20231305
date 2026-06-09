compute.sales.by.zip <- function(FP){
    # Compute sales by zip from the output of the
    #   find.fixed.point(more.outputs = TRUE)
    # function
    S <- FP$Sales.tots
    S.by.zip <- lapply(S, function(x) apply(x, 2, sum))
    S.by.zip <- Reduce(rbind, S.by.zip)
    rownames(S.by.zip) <- names(S)
    return(S.by.zip)
}
