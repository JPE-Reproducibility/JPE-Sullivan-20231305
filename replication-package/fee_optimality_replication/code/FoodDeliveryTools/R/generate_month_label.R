generate.month.label <- function(month, year, suffix = FALSE){
    # Generate a month label
    adopt.times <- platform.adoption.times()
    month.num <- which(adopt.times$months[['2020']] == month)
    month.num <- ifelse(month.num < 10, paste0('0', month.num), as.character(month.num))
    month.label <- sprintf('%s-%s', year, month.num)
    if (suffix){
        month.label <- paste0(month.label, '-01')
    }
    return(month.label)
}
