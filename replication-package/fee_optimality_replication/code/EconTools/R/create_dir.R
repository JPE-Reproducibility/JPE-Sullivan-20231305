create.dir <- function(dir.path){
    # Create the directory specified by `dir.path`
    # (including any missing parent directories) if it doesn't already exist.
    if (!dir.exists(dir.path)){
        dir.create(dir.path, recursive = TRUE)
    }
}

