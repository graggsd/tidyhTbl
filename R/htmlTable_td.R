#' Generate an htmlTable from tidy data
#'
#' A wrapper script for \code{htmlTable} that will build an htmlTable using
#' tidy data and mapping elements of the table to specific columns.
#'
#' @param x The tidy data used to build the table
#' @param value The individual values which will fill each cell of the
#' table
#' @param header_td The column in \code{x} specifying column headings
#' @param rnames_td The column in \code{x} specifying row names
#' @param rgroup_td The column in \code{x} specifying row groups
#' @param cgroup1_td The column in \code{x} specifying the inner most column
#'  groups
#' @param cgroup2_td The column in \code{x} specifying the outer most column
#'  groups
#' @param tspanner_td The column in \code{x} specifying tspanner groups
#' @return Returns html code that will build a pretty table.
#' @export
#' @seealso \code{\link[htmlTable]{htmlTable}}
htmlTable_td <- function(x,
                         value = "value",
                         header_td = "header",
                         rnames_td = "rnames",
                         rgroup_td = NULL,
                         hidden_rgroup = NULL,
                         cgroup1_td = NULL,
                         cgroup2_td = NULL,
                         tspanner_td = NULL,
                         hidden_tspanner = NULL,
                         ...) {
    UseMethod("htmlTable_td")
}

#' @export
htmlTable_td.data.frame <- function(x,
                                    value = "value",
                                    header_td = "header",
                                    rnames_td = "rnames",
                                    rgroup_td = NULL,
                                    hidden_rgroup = NULL,
                                    cgroup1_td = NULL,
                                    cgroup2_td = NULL,
                                    tspanner_td = NULL,
                                    hidden_tspanner = NULL,
                                    ...) {

    argument_checker(x,
                     value = value,
                     header_td = header_td,
                     rnames_td = rnames_td,
                     rgroup_td = rgroup_td,
                     hidden_rgroup = NULL,
                     cgroup1_td = cgroup1_td,
                     cgroup2_td = cgroup2_td,
                     tspanner_td = tspanner_td,
                     hidden_tspanner = NULL)

    check_uniqueness(x,
                     value = value,
                     header_td = header_td,
                     rnames_td = rnames_td,
                     rgroup_td = rgroup_td,
                     cgroup1_td = cgroup1_td,
                     cgroup2_td = cgroup2_td,
                     tspanner_td = tspanner_td)

    # Change NA values to "" in all but the value column
    # This is to allow blank values to work similar to how they do in the
    # real package, but I am thinking about removing this feature to make
    # everything more explicit and allowing some specific rgroups tspanners
    # etc to be hidden instead.
    x <- x %>% convert_NA_values(setdiff(colnames(x), value))
    x <- x %>% convert_NA_values(value, fill = "NA")

    # Create tables from which to gather row, column, and tspanner names
    # and indices
    row_ref_tbl <- x %>%
        get_row_tbl(rnames_td = rnames_td,
                    rgroup_td = rgroup_td,
                    tspanner_td = tspanner_td)

    # Hide row groups specified in hidden_rgroup
    if (!(is.null(hidden_rgroup))) {
        idx <- row_ref_tbl[, rgroup_td] %in% hidden_rgroup
        row_ref_tbl[idx, rgroup_td] <- ""
    }

    # Hide tspanners specified in hidden_tspanner
    if (!(is.null(hidden_tspanner))) {
        idx <- row_ref_tbl[, tspanner_td] %in% hidden_tspanner
        row_ref_tbl[idx, tspanner_td] <- ""
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
        tidyr::spread(key = c_idx,
                      value = value,
                      fill = "") %>%
        dplyr::select(-r_idx)

    # Get names and indices for row groups and tspanners
    htmlTable_args <- list(x = formatted_df,
                           rnames = row_ref_tbl[, rnames_td],
                           header = col_ref_tbl[, header_td],
                           ...)
    if (!is.null(rgroup_td)) {
        # This will take care of a problem in which adjacent row groups
        # with the same value will cause rgroup and tspanner collision
        comp_val <- paste0(row_ref_tbl[, rgroup_td], row_ref_tbl[, tspanner_td])
        lens <- rle(comp_val)$lengths
        idx <- cumsum(lens)

        htmlTable_args$rgroup = row_ref_tbl[idx, rgroup_td]
        htmlTable_args$n.rgroup = lens
    }
    if (!is.null(tspanner_td)) {
        htmlTable_args$tspanner = rle(row_ref_tbl[, tspanner_td])$value
        htmlTable_args$n.tspanner = rle(row_ref_tbl[, tspanner_td])$lengths
    }
    # Get names and indices for column groups
    if(!is.null(cgroup1_td)) {
        cgroup1 <- rle(col_ref_tbl[, cgroup1_td])$value
        n.cgroup1 <- rle(col_ref_tbl[, cgroup1_td])$lengths
        if(!is.null(cgroup2_td)) {
            cgroup2 <- rle(col_ref_tbl[, cgroup2_td])$value
            n.cgroup2 <- rle(col_ref_tbl[, cgroup2_td])$lengths
            len_diff <- length(cgroup1) - length(cgroup2)
            if (len_diff < 0) {
                stop("cgroup2 cannot contain more categories than cgroup1")
            } else if (len_diff > 0) {
                cgroup2 <- c(cgroup2, rep(NA, len_diff))
                n.cgroup2 <- c(n.cgroup2, rep(NA, len_diff))
            }
            cgroup1 <- rbind(cgroup2, cgroup1)
            n.cgroup1 <- rbind(n.cgroup2, n.cgroup1)
        }
        htmlTable_args$cgroup <- cgroup1
        htmlTable_args$n.cgroup <- n.cgroup1
    }
    do.call(htmlTable::htmlTable, htmlTable_args)
}

check_uniqueness <- function(x, ...) {
    # Get arguments
    args <- simplify_arg_list(...)
    cols <- as.character(args)
    dupes <- x %>%
        select(cols) %>%
        duplicated
    if (sum(dupes) != 0) {

        stop(paste0("The input parameters ",paste(paste0("\"", names(args), "\""), collapse = ", "),
                    " do not specify unique rows. The following rows ",
                    "are duplicated: ",
                    paste(which(dupes), collapse = ", ")))
    }
}

simplify_arg_list <- function(...) {
    x <- list(...)
    idx <- sapply(x, is.null)
    return(x[!idx])
}

argument_checker <- function(x, ...) {

    # Check that all the input are characters
    all_args <- simplify_arg_list(...)
    idx <- which(!sapply(all_args, is.character))

    if (length(idx) > 0) {
        stop("The following parameters must be of type character: ",
             paste(names(all_args)[idx], collapse = ", "))
    }

    # Check that all of the arguments that would be used map columns to
    # character attributes are of length 1
    col_vars <- all_args[names(all_args) %in%
                             c("value", "header_td", "rnames_td", "rgroup_td",
                               "cgroup1_td", "cgroup2_td", "tspanner_td")]

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

    x <- x %>% convert_NA_values(cols)

    out <- x %>%
        dplyr::select(cols) %>%
        unique %>%
        dplyr::arrange_(.dots = cols) %>%
        as.data.frame(stringsAsFactors = FALSE)

    out$c_idx <- 1:nrow(out)

    # To facilitate use of rle
    for(col in cols) {
        out[, col] <- as.character(out[, col])
        out[, col][is.na(out[, col])] <- ""
    }

    return(out)
}

get_row_tbl <- function(x,
                        rnames_td,
                        rgroup_td = NULL,
                        tspanner_td = NULL) {

    cols <- c(tspanner_td, rgroup_td, rnames_td)

    x <- x %>% convert_NA_values(cols)

    out <- x %>%
        dplyr::select(cols) %>%
        unique %>%
        dplyr::arrange_(.dots = cols) %>%
        as.data.frame(stringsAsFactors = FALSE)
    out$r_idx <- 1:nrow(out)

    # To facilitate use of rle
    for(col in cols) {
        out[, col] <- as.character(out[, col])
        out[, col][is.na(out[, col])] <- ""
    }

    return(out)
}

add_col_idx <- function(x,
                        header_td,
                        cgroup1_td = NULL,
                        cgroup2_td = NULL) {

    cols <- c(cgroup2_td, cgroup1_td, header_td)

    x <- x %>% convert_NA_values(cols)

    col_idx_df <- x %>%
        get_col_tbl(header_td = header_td,
                    cgroup1_td = cgroup1_td,
                    cgroup2_td = cgroup2_td) %>%
        tidyr::unite_(col = "key", from = cols)

    out <- x %>%
        tidyr::unite_(col = "key", from = cols, remove = FALSE) %>%
        dplyr::left_join(col_idx_df, "key") %>%
        dplyr::select(-key)

    return(out)
}

add_row_idx <- function(x,
                        rnames_td,
                        rgroup_td = NULL,
                        tspanner_td = NULL) {

    cols <- c(tspanner_td, rgroup_td, rnames_td)

    x <- x %>% convert_NA_values(cols)

    row_idx_df <- x %>%
        get_row_tbl(rnames_td = rnames_td,
                    rgroup_td = rgroup_td,
                    tspanner_td = tspanner_td) %>%
        tidyr::unite_(col = "key", from = cols)

    out <- x %>%
        tidyr::unite_(col = "key", from = cols, remove = FALSE) %>%
        dplyr::left_join(row_idx_df, "key") %>%
        dplyr::select(-key)
    return(out)
}

# This function will be used to convert NA values to ""
convert_NA_values <- function(x, cols, fill = "") {
    for (col in cols) {
        if (is.factor(x[, col])) {
            levs <- levels(x[, col])
            x[, col] <- as.character(x[, col])
            x[, col][is.na(x[, col])] <- fill
            x[, col] <- factor(x[, col], levels = unique(c(levs, fill)))
        } else {
            x[, col][is.na(x[, col])] <- fill
        }
    }
    return(x)
}