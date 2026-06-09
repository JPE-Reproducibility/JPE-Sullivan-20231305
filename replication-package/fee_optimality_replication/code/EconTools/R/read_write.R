read.dat <- function(x, header = TRUE, sep = ',', quote = "\"",
                     dec = '.', fill = TRUE, comment.char = "", ...){
    dat <- read.table(x, header = header, sep = sep, quote = quote,
                      dec = dec, fill = fill, comment.char = comment.char,
                      stringsAsFactors = FALSE, ...)
    return(dat)
}

write.dat <- function(x, file, row.names = FALSE,
                      quote = FALSE, append = FALSE,
                      sep = ',', dec = '.', qmethod = 'double',
                      col.names = TRUE, ...){
    write.table(x = x, file = file,
                append = append, sep = sep, dec = dec, qmethod = qmethod,
              row.names = row.names, quote = quote,
              col.names = col.names, ...)
}

