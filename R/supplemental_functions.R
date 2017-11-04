#' Combine the levels or unique values of multiple columns into a vector
#'
#' \code{get_factors} is a function designed to collect the factor levels
#' and unique values within a series of columns and combine them into one
#' vector. This may be useful when trying to construct a table using
#' \code{htmlTable_td}.
#'
#' @param x A dataframe
#' @param cols The columns from which to grab unique values and factor levels
#' @param rev Specifies which columns will have a reversed order
#' @return Returns a vector of unique values and levels ordered in a manner
#' consistent with the columns from which they were derived.
#' @export
get_factors <- function(x, cols, rev = FALSE) {

    stopifnot(is.numeric(rev) | is.logical(rev))

    # Create a vector of logicals based on the input into rev
    if (is.logical(rev)) {
        if (length(rev) == 1) {
            rev <- rep(rev, length(cols))
        } else if (length(rev) != length(cols)) {
            stop(paste0("If rev is logical, it must be of length 1 or the same",
                        " lengths as cols"))
        }
    } else {
        tmp_rev <- rep(FALSE, length(cols))
        tmp_rev[rev] <- TRUE
        rev <- tmp_rev
    }

    names(rev) <- cols

    # Create a vector of levels (revered order of specific columns based on
    # rev argument)
    levs <- NULL
    for (col in cols) {
        if (is.factor(x[, col])) {
            tmp_lev <- levels(x[, col])
        } else {
            tmp_lev <- sort(unique(x[, col]))
        }

        if (rev[names(rev) == col]) {tmp_lev <- rev(tmp_lev)}
        levs <- c(levs, tmp_lev)
    }

    return(unique(levs))
}

#' Function to retrieve the first level from one or more columns in a data.frame
#'
#' \code{get_first_levels} is a function that may be used to retrieve the first
#' level from one or more columns in a data.frame
#'
#' @param x A dataframe
#' @param cols The columns from which to retrieve first levels
#' @return Returns a vector of first levels
#' @export
get_first_levels <- function(x, cols) {

    # Create a vector of levels (revered order of specific columns based on
    # rev argument)
    levs <- NULL
    for (col in cols) {
        if (is.factor(x[, col])) {
            levs <- c(levs, levels(x[, col])[1])
        } else {
            levs <- c(levs, sort(unique(x[, col]))[1])
        }
    }

    names(levs) <- cols
    return(levs)
}