subset.list <- function(L, z.sub){
    # L: a list
    # z.sub: names of members of L to keep
    z.sub <- intersect(z.sub, names(L))
    L.sub <- lapply(z.sub, function(z) L[[z]])
    names(L.sub) <- z.sub
    return(L.sub)
}
