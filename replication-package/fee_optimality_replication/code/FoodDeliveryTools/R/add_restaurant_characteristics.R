add.restaurant.characteristics <- function(df, top.chains, top.cuisines){
    # Add chain and cuisine data to the data.frame `df`

    ## Chain
    df$chain[which(df$chain == '' | is.na(df$chain))] <- 'none'
    ### Only include top chains
    df$chain[which((df$chain != 'none') & !(df$chain %in% top.chains))] <- 'other_chain'

    ## Cuisine
    cuisines.split <- lapply(df$cuisine, strsplit, split = ', ')
    cuisines.split <- lapply(cuisines.split, function(x) x[[1]])
    cuisines.processed <- lapply(cuisines.split, function(x) sapply(x, process.cuisines))

    # Create cuisine indicators
    for (cuisine in top.cuisines){
        varname <- sprintf('Cuisine: %s', cuisine)
        idx <- which(!is.na(df$cuisine))
        df[, varname] <- NA
        df[idx, varname] <- sapply(idx, function(k) 1*(cuisine %in% cuisines.processed[[k]]))
    }

    return(df)
}

