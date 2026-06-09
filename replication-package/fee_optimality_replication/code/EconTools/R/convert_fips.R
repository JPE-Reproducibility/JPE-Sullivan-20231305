convert.fips <- function(fips.numeric){
    # Convert FIPS codes from numerics/integers to strings
    fips.char <- ifelse(fips.numeric < 10000, 
                        sprintf('0%d', fips.numeric),
                        as.character(fips.numeric))
    return(fips.char)
}
