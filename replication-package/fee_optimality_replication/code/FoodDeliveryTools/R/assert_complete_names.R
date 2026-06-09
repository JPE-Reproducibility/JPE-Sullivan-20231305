assert.complete.names <- function(found, expected, what, hint = NULL){
    # Stop with an informative error if `found` does not cover `expected`.
    #
    # Used by the CF aggregation scripts to ensure that a missing or stale
    # per-market result file raises a hard error instead of silently
    # shrinking the analysis sample (e.g. via intersect() on whatever file
    # names happened to load).
    #
    # Inputs
    #   found:    character vector of names actually loaded
    #   expected: character vector of names that must all be present
    #   what:     short description of the object being checked
    #   hint:     optional remediation hint appended to the error message
    missing <- setdiff(expected, found)
    if (length(missing) > 0){
        msg <- sprintf('%s: missing %d of %d expected entries (%s)',
                       what, length(missing), length(expected),
                       paste(missing, collapse = ', '))
        if (!is.null(hint)){
            msg <- sprintf('%s. %s', msg, hint)
        }
        stop(msg, call. = FALSE)
    }
    invisible(TRUE)
}
