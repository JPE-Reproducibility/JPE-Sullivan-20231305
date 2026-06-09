
reformat.restaurant.names <- function(df){
    # Change some restaurants' names

    # Remove (R) and (C) symbols
    df$restaurant <- sub('[ ]*®', '', df$restaurant)
    df$restaurant <- sub('[ ]*©', '', df$restaurant)
    # Remove parentheticals
    df$restaurant <- sub("[ ]*\\(.*\\).*", '', df$restaurant)
    # Lower case
    df$restaurant <- tolower(df$restaurant)
    # Replace ft with fort (after tolower so 'Ft' is also caught)
    df$restaurant <- gsub('^ft\\.? ', 'fort ', df$restaurant)
    # Remove Chinese characters
    df$restaurant <- gsub('[ ]*[\u4E00-\u9FA5][ ]*', '', df$restaurant)

    # Switch apostrophes and some other characters to ascii
    df$restaurant <- gsub('’', "'", df$restaurant)
    df$restaurant <- gsub('‘', "'", df$restaurant)

    # Remove all non-ascii chararcters after converting accents
    df$restaurant <- stringi::stri_trans_general(str = df$restaurant, id = "Latin-ASCII")
    df$restaurant <- gsub('[^\x01-\x7F]', '', df$restaurant)
    return(df)
}
