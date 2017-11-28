#' Generate an htmlTable using a ggplot2-like interface
#'
#' Builds an \code{htmlTable} by mapping columns from the input data, \code{x},
#' to elements of an output \code{htmlTable} (e.g. rnames_td, header_td, etc.)
#'
#' @section Column-mapping parameters:
#'   The \code{htmlTable_td} function is designed to work like ggplot2 in that
#'   columns from \code{x} are mapped to specific parameters from the
#'   \code{htmlTable} function. At minimum, \code{x} must contain the names
#'   of columns mapping to \code{rnames_td}, \code{header_td}, and \code{rnames_td}.
#'   \code{header_td} and \code{rnames_td} retain the same meaning as in the
#'   htmlTable function. \code{value} contains the individual values that will
#'   be used to fill each cell within the output \code{htmlTable}.
#'
#'   A full list of parameters from \code{htmlTable} which may be mapped to
#'   columns within \code{x} include:
#'
#'   \itemize{
#'     \item \code{value}
#'     \item \code{header_td}
#'     \item \code{rnames_td}
#'     \item \code{rgroup_td}
#'     \item \code{cgroup1_td}
#'     \item \code{cgroup2_td}
#'     \item \code{tspanner_td}
#'   }
#'
#'   Note that unlike in \code{htmlTable} which contains \code{cgroup},
#'   and which may specify a variable number of column groups,
#'   \code{htmlTable_td} contains the parameters \code{cgroup1_td} and
#'   \code{cgroup2_td}. These parameters correspond to the inward most and outward
#'   most column groups respectively.
#'
#'   Also note that the coordinates of each \code{value} within \code{x} must be
#'   unambiguously mapped to a position within the output \code{htmlTable}.
#'   Therefore, the each row-wise combination the variables specified above
#'   contained in \code{x} must be unique.
#'
#' @section Hidden values:
#'   \code{htmlTable} Allows for some values within \code{rgroup_td},
#'   \code{cgroup}, etc. to be specified as \code{""}. The following parameters
#'   allow for specific values to be treated as if they were a string of length
#'   zero in the \code{htmlTable} function.
#'
#'   \itemize{
#'     \item \code{hidden_rgroup_td}
#'     \item \code{hidden_tspanner_td}
#'   }
#'
#' @param x Tidy data used to build the \code{htmlTable}
#' @param value The column containing values filling individual cells of the
#' output \code{htmlTable}
#' @param header_td The column in \code{x} specifying column headings
#' @param rnames_td The column in \code{x} specifying row names
#' @param rgroup_td The column in \code{x} specifying row groups
#' @param hidden_rgroup_td rgroup_td values that will be hidden.
#' @param cgroup1_td The column in \code{x} specifying the inner most column
#'  groups
#' @param cgroup2_td The column in \code{x} specifying the outer most column
#'  groups
#' @param tspanner_td The column in \code{x} specifying tspanner_td groups
#' @param hidden_tspanner_td tspanner_td values that will be hidden.
#' @param ... Additional arguments that will be passed to the inner
#' \code{htmlTable} function
#' @return Returns html code that will build a pretty table
#' @export
#' @seealso \code{\link[htmlTable]{htmlTable}}
#' @examples
#' \dontrun{
#' library(tidyverse)
#' mtcars %>%
#'     rownames_to_column %>%
#'     select(rowname, cyl, gear, hp, mpg, qsec) %>%
#'     gather(per_metric, value, hp, mpg, qsec) %>%
#'     group_by(cyl, gear, per_metric) %>%
#'     summarise(Mean = round(mean(value), 1),
#'               SD = round(sd(value), 1),
#'               Min = round(min(value), 1),
#'               Max = round(max(value), 1)) %>%
#'      gather(summary_stat, value, Mean, SD, Min, Max) %>%
#'      ungroup %>%
#'      mutate(gear = paste(gear, "Gears"),
#'             cyl = paste(cyl, "Cylinders")) %>%
#'      htmlTable_td(header_td = "gear",
#'                   cgroup1_td = "cyl",
#'                   cell_value = "value",
#'                   rnames_td = "summary_stat",
#'                   rgroup_td = "per_metric")
#' }
htmlTable_td <- function(x,
                          value = "value",
                          header_td = "header_td",
                          rnames_td = "rnames_td",
                          rgroup_td = NULL,
                          hidden_rgroup_td = NULL,
                          cgroup1_td = NULL,
                          cgroup2_td = NULL,
                          tspanner_td = NULL,
                          hidden_tspanner_td = NULL,
                          ...) {
    UseMethod("htmlTable_td")
}

#' @export
htmlTable_td.default <- function(x, ...) {
    stop("x must be of class data.frame")
}

#' @export
htmlTable_td.data.frame <- function(x,
                                     value = "value",
                                     header_td = "header_td",
                                     rnames_td = "rnames_td",
                                     rgroup_td = NULL,
                                     hidden_rgroup_td = NULL,
                                     cgroup1_td = NULL,
                                     cgroup2_td = NULL,
                                     tspanner_td = NULL,
                                     hidden_tspanner_td = NULL,
                                     ...) {

    argument_checker(x,
                     value = value,
                     header_td = header_td,
                     rnames_td = rnames_td,
                     rgroup_td = rgroup_td,
                     hidden_rgroup_td = NULL,
                     cgroup1_td = cgroup1_td,
                     cgroup2_td = cgroup2_td,
                     tspanner_td = tspanner_td,
                     hidden_tspanner_td = NULL)

    check_uniqueness(x,
                     header_td = header_td,
                     rnames_td = rnames_td,
                     rgroup_td = rgroup_td,
                     cgroup1_td = cgroup1_td,
                     cgroup2_td = cgroup2_td,
                     tspanner_td = tspanner_td)

    x <- remove_na_rows(x,
                        header_td = header_td,
                        rnames_td = rnames_td,
                        rgroup_td = rgroup_td,
                        cgroup1_td = cgroup1_td,
                        cgroup2_td = cgroup2_td,
                        tspanner_td = tspanner_td)

    # Create tables from which to gather row, column, and tspanner_td names
    # and indices
    row_ref_tbl <- x %>%
        get_row_tbl(rnames_td = rnames_td,
                    rgroup_td = rgroup_td,
                    tspanner_td = tspanner_td)

    # Hide row groups specified in hidden_rgroup_td
    if (!(is.null(hidden_rgroup_td))) {
        row_ref_tbl <- row_ref_tbl %>%
            dplyr::mutate_at(rgroup_td,
                             function(x){ifelse(x %in% hidden_rgroup_td, "", x)})
    }

    # Hide tspanner_tds specified in hidden_tspanner_td
    if (!(is.null(hidden_tspanner_td))) {
        row_ref_tbl <- row_ref_tbl %>%
            dplyr::mutate_at(tspanner_td,
                             function(x){ifelse(x %in% hidden_tspanner_td, "", x)})
    }

    col_ref_tbl <- x %>%
        get_col_tbl(header_td = header_td,
                    cgroup1_td = cgroup1_td,
                    cgroup2_td = cgroup2_td)

    # Format the values for display
    to_select <- c("r_idx", "c_idx", value)

    formatted_df <- x %>%
        add_col_idx(header_td = header_td,
                    cgroup1_td = cgroup1_td,
                    cgroup2_td = cgroup2_td) %>%
        add_row_idx(rnames_td = rnames_td,
                    rgroup_td = rgroup_td,
                    tspanner_td = tspanner_td) %>%
        dplyr::select(to_select) %>%
        dplyr::mutate_at(value, as.character) %>%
        # Spread will fill missing values (both explict and implicit) with the
        # same value, so we need to convert these values to a character if we want
        # them to show up correctly in the final table
        tidyr::spread(key = c_idx,
                      value = value,
                      fill = "") %>%
        dplyr::select(-r_idx)

    # Get names and indices for row groups and tspanner_tds
    htmlTable_args <- list(x = formatted_df,
                           rnames_td = row_ref_tbl %>% dplyr::pull(rnames_td),
                           header_td = col_ref_tbl %>% dplyr::pull(header_td),
                           ...)

    if (!is.null(rgroup_td)) {

        # This will take care of a problem in which adjacent row groups
        # with the same value will cause rgroup_td and tspanner_td collision
        comp_val <- row_ref_tbl %>% dplyr::pull(rgroup_td)

        if (!is.null(tspanner_td)) {
            comp_val <- paste0(comp_val,
                               row_ref_tbl %>% dplyr::pull(tspanner_td))
        }

        lens <- rle(comp_val)$lengths
        idx <- cumsum(lens)

        htmlTable_args$rgroup_td <- row_ref_tbl %>%
            dplyr::slice(idx) %>%
            dplyr::pull(rgroup_td)

        htmlTable_args$n.rgroup_td <- lens
    }

    if (!is.null(tspanner_td)) {
        htmlTable_args$tspanner_td <-
            rle(row_ref_tbl %>% dplyr::pull(tspanner_td))$value
        htmlTable_args$n.tspanner_td <-
            rle(row_ref_tbl %>% dplyr::pull(tspanner_td))$lengths
    }

    # Get names and indices for column groups
    if(!is.null(cgroup1_td)) {
        cgroup1_td_out <- rle(col_ref_tbl %>% dplyr::pull(cgroup1_td))$value
        n.cgroup1_td <- rle(col_ref_tbl %>% dplyr::pull(cgroup1_td))$lengths
        if(!is.null(cgroup2_td)) {
            cgroup2_td_out <- rle(col_ref_tbl %>% dplyr::pull(cgroup2_td))$value
            n.cgroup2_td <- rle(col_ref_tbl %>% dplyr::pull(cgroup2_td))$lengths
            len_diff <- length(cgroup1_td_out) - length(cgroup2_td_out)
            if (len_diff < 0) {
                stop("cgroup2_td cannot contain more categories than cgroup1_td")
            } else if (len_diff > 0) {
                cgroup2_td_out <- c(cgroup2_td, rep(NA, len_diff))
                n.cgroup2_td <- c(n.cgroup2_td, rep(NA, len_diff))
            }
            cgroup1_td_out <- rbind(cgroup2_td, cgroup1_td)
            n.cgroup1_td <- rbind(n.cgroup2_td, n.cgroup1_td)
        }
        htmlTable_args$cgroup <- cgroup1_td_out
        htmlTable_args$n.cgroup <- n.cgroup1_td
    }

    do.call(htmlTable::htmlTable, htmlTable_args)
}

# Removes rows containing NA values in any mapped columns from the tidy dataset
remove_na_rows <- function(x, ...) {
    cols <- as.character(get_col_vars(...))
    na.log <- x %>%
        dplyr::select(cols) %>%
        is.na

    na.row.sums <- na.log %>%
        rowSums

    keep.idx <- na.row.sums == 0
    removed <- sum(na.row.sums > 0)

    if (removed != 0) {
        na.col.sums <- na.log %>%
            colSums
        na.cols <- colnames(na.log)[na.col.sums > 0]
        warning(paste0("NA values were detected in the following columns of ",
                       "the tidy dataset: ",
                       paste(na.cols, collapse = ", "), ". ",
                       removed, " row(s) in the tidy dataset were removed."))
    }
    return(x %>% dplyr::filter(keep.idx))
}

# This checks to make sure that the mapping columns of the tidy dataset
# uniquely specify a given value
check_uniqueness <- function(x, ...) {
    # Get arguments
    args <- simplify_arg_list(...)
    cols <- as.character(args)
    dupes <- x %>%
        dplyr::select(cols) %>%
        duplicated
    if (sum(dupes) != 0) {

        stop(paste0("The input parameters ",
                    paste(paste0("\"", names(args), "\""), collapse = ", "),
                    " do not specify unique rows. The following rows ",
                    "are duplicated: ",
                    paste(which(dupes), collapse = ", ")))
    }
}

# Converts arguments from ... into a list and removes those that have been set
# to NULL
simplify_arg_list <- function(...) {
    x <- list(...)
    idx <- sapply(x, is.null)
    return(x[!idx])
}

# This function gets arguments from ..., removes those that are NULL,
# and then subsets those that should map tidy data columns to htmlTable
# parameters
get_col_vars <- function(...) {
    out <- simplify_arg_list(...)
    return(out[names(out) %in%
                   c("value", "header_td",
                     "rnames_td", "rgroup_td",
                     "cgroup1_td", "cgroup2_td",
                     "tspanner_td")])
}

# Checks a variety of assumptions about input arguments and prepares an
# appropriate error message if those assumptions are violated
argument_checker <- function(x, ...) {

    # Check if x is a grouped tbl_df
    if(dplyr::is.grouped_df(x)) {
        stop("x cannot be a grouped_df")
    }

    # Check that all the input are characters
    all_args <- simplify_arg_list(...)
    idx <- which(!sapply(all_args, is.character))

    if (length(idx) > 0) {
        stop("The following parameters must be of type character: ",
             paste(names(all_args)[idx], collapse = ", "))
    }

    # Check that all of the arguments that would be used map columns to
    # character attributes are of length 1
    col_vars <- get_col_vars(...)

    idx <- which(sapply(col_vars, length) > 1)
    if (length(idx) > 0) {
        stop("The following parameters must be of length 1: ",
             paste(names(col_vars)[idx], collapse = ", "))
    }

    # Find column variables that are not columns in the dataset
    idx <- which(!(as.character(col_vars) %in% colnames(x)))
    if (length(idx) > 0) {
        stop("The following arguments need values that correspond to column ",
             "names in x: ",
             paste0(names(col_vars), " = ",
                    as.character(col_vars),
                    collapse = ", "))
    }
}

get_col_tbl <- function(x,
                        header_td,
                        cgroup1_td = NULL,
                        cgroup2_td = NULL) {

    cols <- c(cgroup2_td, cgroup1_td, header_td)

    out <- x %>%
        dplyr::select(cols) %>%
        unique %>%
        dplyr::arrange_at(cols) %>%
        # This is necessary in order to not generate NA values when setting
        # hidden elements to ""
        dplyr::mutate_if(is.factor, as.character)

    out$c_idx <- 1:nrow(out)
    return(out)
}

get_row_tbl <- function(x,
                        rnames_td,
                        rgroup_td = NULL,
                        tspanner_td = NULL) {

    cols <- c(tspanner_td, rgroup_td, rnames_td)

    out <- x %>%
        dplyr::select(cols) %>%
        unique %>%
        dplyr::arrange_at(cols) %>%
        # This is necessary in order to not generate NA values when setting
        # hidden elements to ""
        dplyr::mutate_if(is.factor, as.character)

    out$r_idx <- 1:nrow(out)
    return(out)
}

add_col_idx <- function(x,
                        header_td,
                        cgroup1_td = NULL,
                        cgroup2_td = NULL) {

    cols <- c(cgroup2_td, cgroup1_td, header_td)

    col_idx_df <- x %>%
        get_col_tbl(header_td = header_td,
                    cgroup1_td = cgroup1_td,
                    cgroup2_td = cgroup2_td)

    out <- suppressWarnings(
        x %>%
            dplyr::left_join(col_idx_df, cols)
    )
    return(out)
}

add_row_idx <- function(x,
                        rnames_td,
                        rgroup_td = NULL,
                        tspanner_td = NULL) {

    cols <- c(tspanner_td, rgroup_td, rnames_td)

    row_idx_df <- x %>%
        get_row_tbl(rnames_td = rnames_td,
                    rgroup_td = rgroup_td,
                    tspanner_td = tspanner_td)

    out <- suppressWarnings(
        x %>%
            dplyr::left_join(row_idx_df, by = cols)
    )
    return(out)
}
