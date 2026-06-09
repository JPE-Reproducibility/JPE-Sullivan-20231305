
collapse.weighted <- function(y, x, w, df){
    # Compute the weighted mean of each y variable for each
    # combination of x variables
    
    # To avoid numerical overflow...
    sum.weights <- sum(df[, w]) 
    df[, w] <- df[, w]/sum.weights
    
    # Generate versions of y variables interacted with weights
    y.w <- c()
    for (y.i in y){
        y.w.i <- paste0(y.i, '_w')
        y.w <- c(y.w, y.w.i)
        df[, y.w.i] <- df[, y.i]*df[, w]
    }
    # Generate formula
    fn <- function(v1, v2) sprintf('%s + %s', v1, v2)
    LHS <- Reduce(fn, c(y.w, w))
    RHS <- Reduce(fn, x)
    fmla <- as.formula(sprintf('%s ~ %s', LHS, RHS))
    df.agg <- doBy::summaryBy(fmla, data = df, FUN = sum, keep.names = TRUE)
    # Divide by weights
    for (y.i in y){
        y.w.i <- paste0(y.i, '_w')
        df.agg[, y.i] <- df.agg[, y.w.i]/df.agg[, w]
        df.agg[, y.w.i] <- NULL
    }
    df.agg[, w] <- df.agg[, w]*sum.weights
    return(df.agg)
}


