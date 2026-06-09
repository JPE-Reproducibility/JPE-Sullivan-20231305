platform.adoption.times <- function(){
    years <- c('2020', '2021')
    months <- list()
    months[['2020']] <- c('jan', 'feb', 'mar', 'apr', 'may', 'jun',
                          'jul', 'aug', 'sep', 'oct', 'nov', 'dec')
    months[['2021']] <- c('jan', 'feb', 'mar', 'apr', 'may')
    return(list(months = months,
                years = years))
}
