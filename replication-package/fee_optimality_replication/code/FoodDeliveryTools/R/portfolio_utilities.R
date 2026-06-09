as.numeric.keep.names <- function(x){
    # x: data.frame with one row
    x.names <- colnames(x)
    x <- as.numeric(x)
    names(x) <- x.names
    return(x)
}

map.J.to.Jp <- function(J.G.m, zip.mat.m){
    # Similar to map.inside.to.range, except that it uses
    # the list `J.G.m` instead of the data.frame `inside.counts`
    if (length(J.G.m) == 1){
        Jp.G.m <- J.G.m
    } else {
        inside.counts <- plyr::ldply(J.G.m)
        rownames(inside.counts) <- inside.counts$.id
        keep.zips <- rownames(zip.mat.m)[rownames(zip.mat.m) %in% rownames(inside.counts)]
        inside.counts <- inside.counts[keep.zips, ]
        inside.counts$.id <- NULL
        inside.counts <- as.matrix(inside.counts)
        zip.mat.sub <- zip.mat.m[, keep.zips]

        if ('V1' %in% colnames(inside.counts)){
            # Manually override with platform portfolio names
            colnames(inside.counts) <- generate.platform.portfolio.names()
        }
        range.counts <- map.inside.to.range(inside.counts, zip.mat.sub)
        Jp.G.m <- generate.J.G(range.counts)
    }
    return(Jp.G.m)
}

generate.platform.portfolio.names <- function(){
    # Canonical portfolio column names (4 platforms -> 16 subsets), used when
    # the input matrix arrives without names.
    G.vars <- c("G0000", "G1000", "G0100", "G1100",
                "G0010", "G1010", "G0110", "G1110",
                "G0001", "G1001", "G0101", "G1101",
                "G0011", "G1011", "G0111", "G1111")
    return(G.vars)
}


map.inside.to.range <- function(inside.counts, zip.mat){
    # Map counts of restaurants inside zip codes to counts of restaurants
    # within range of zip codes
    idx.G <- grep('^(G|J_)', colnames(inside.counts))
    # Ensure zip codes align
    inside.counts <- inside.counts[order(rownames(inside.counts)), ]
    # Compute the range counts
    range.counts <- zip.mat%*%inside.counts[, idx.G]
    # Convert back to data.frame
    range.counts <- as.data.frame(as.matrix(range.counts))
    range.counts$zip <- rownames(range.counts)

    return(range.counts)
}

generate.J.G <- function(resto.m){
    # Generate a list of the restaurant counts for each zip code
    # in a particular market using the data in the `resto.m` data.frame
    idx.G <- grep('^G[01]+$', colnames(resto.m))
    portfolios <- colnames(resto.m)[idx.G]
    resto.G <- resto.m[, idx.G]
    zips <- resto.m$zip
    J.G.m <- as.list(as.data.frame(t(resto.G), stringsAsFactors = FALSE))
    fn <- function(x){
        # function for adding names
        names(x) <- portfolios
        return(x)
    }
    J.G.m <- lapply(J.G.m, fn)
    names(J.G.m) <- zips
    return(J.G.m)
}


generate.membership.mat <- function(portfolios, nplatforms = 5){
    # Generate matrix that provides platform membership of
    # each portfolio
    nportfolios <- length(portfolios)
    G.mat <- pracma::zeros(nportfolios, nplatforms - 1)
    for (k in 1:nportfolios){
        gk <- sub('G', '', portfolios[k])
        gk <- strsplit(gk, '')[[1]]
        gk <- as.numeric(gk)
        G.mat[k, ] <- gk
    }
    # Account for outside platform
    G.mat <- cbind(1, G.mat)
    return(G.mat)
}


