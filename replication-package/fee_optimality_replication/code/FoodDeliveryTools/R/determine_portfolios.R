determine.portfolios <- function(G.mat, keep.platforms){
    # Compute a vector containing the indices of platform portfolios that
    # do not contain of the platforms excluded from "keep.platforms"

    # compute version without "outside platform"
    G.mat.sub <- G.mat[, 2:ncol(G.mat)]
    inactive.platforms <- which(keep.platforms == 0)
    keep.portfolios <- apply(G.mat.sub[, inactive.platforms], 1, function(x) all(x == 0))
    keep.portfolios <- which(keep.portfolios)
    return(keep.portfolios)
}
