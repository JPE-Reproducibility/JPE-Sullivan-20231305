fix.zip.codes <- function(x){
    # Convert x to a character-type zip code with leading zeros
    y <- sapply(x, fix.zip.x)
    return(y)
}

fix.zip.x <- function(x){
    x <- as.integer(x)
    if (is.na(x)){
        y <- NA
    } else if (x < 1000){
        y <- sprintf('00%d', x)
    } else if (x < 10000){
        y <- sprintf('0%d', x)
    } else {
        y <- as.character(x)
    }
    return(y)
}

