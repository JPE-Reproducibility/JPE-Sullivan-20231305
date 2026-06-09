chain.stat <- function(z){
    # Determine chain status of ZIP z
    is.chain <- grepl('c$', z)
    return(is.chain)
}
