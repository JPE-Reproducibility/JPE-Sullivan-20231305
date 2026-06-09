rename.var <- function(df, ov, nv){
    # df: The data.frame
    # ov: Old variable name
    # nv: New variable name
    idx <- which(colnames(df) == ov)
    if (length(idx) == 0){
        warning(sprintf('Variable %s not found in df', ov))
    } else if (nv %in% colnames(df)){
        warning(sprintf('Variable %s already in df; not overwriting', nv))
    } else {
        colnames(df)[idx] <- nv
    }
    return(df)
}
