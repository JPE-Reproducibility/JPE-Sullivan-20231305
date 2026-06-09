capture program drop save_vcov
program define save_vcov
    // Save e(V) to CSV with row & column names, no svmat needed
    // Usage: save_vcov using "path/to/file.csv" [, replace]
    version 18
    syntax using/, [REPLACE]

    // Grab the matrix
    tempname V
    matrix `V' = e(V)

    // Extract names and dimensions
    local rnames : rownames `V'
    local cnames : colnames `V'
    local R = rowsof(`V')
    local C = colsof(`V')

    // Helper: CSV-escape a token by doubling internal quotes and wrapping in quotes
    // (row/col names from Stata matrices won't contain spaces, but we still quote safely)
    // We'll use a local macro function via inline substitution each time.
    // Example: local safe = subinstr("name","""","""""",.)
    
    tempname fh
    file open `fh' using "`using'", write text `replace'

    // ---- Write header ----
    // First cell is blank to align rowname column
    file write `fh' `"`""'"'
    forvalues j = 1/`C' {
        local colname : word `j' of `cnames'
        local colname_esc = subinstr("`colname'","""","""""",.)
        file write `fh' "," `""`colname_esc'""'
    }
    file write `fh' _n

    // ---- Write rows ----
    forvalues i = 1/`R' {
        local rowname : word `i' of `rnames'
        local rowname_esc = subinstr("`rowname'","""","""""",.)
        file write `fh' `""`rowname_esc'""'
        forvalues j = 1/`C' {
            scalar __v = el(`V',`i',`j')
            // write comma + numeric with general format
            file write `fh' "," %21.0g (__v)
        }
        file write `fh' _n
    }

    file close `fh'
end
