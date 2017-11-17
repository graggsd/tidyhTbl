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
#' @examples
#' \dontrun{
#' mtcars %>%
#'     rownames_to_column %>%
#'     select(rowname, cyl, gear, hp, mpg, qsec) %>%
#'     gather(per_metric, value, hp, mpg, qsec) %>%
#'     group_by(cyl, gear, per_metric) %>%
#'     summarise(Mean = round(mean(value), 1), %>%
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
        dplyr::mutate_at(value, as.character) %>%
        # Spread will fill missing values (both explict and implicit) with the
        # same value, so we need to convert these values to a character if we want
        # them to show up correctly in the final table
        tidyr::spread(key = c_idx,
                      value = value,
                      fill = "") %>%
        dplyr::select(-r_idx)

    # Get names and indices for row groups and tspanners
    htmlTable_args <- list(x = formatted_df,
                           rnames = row_ref_tbl %>% dplyr::pull(rnames_td),
                           header = col_ref_tbl %>% dplyr::pull(header_td),
                           ...)
    if (!is.null(rgroup_td)) {
        # This will take care of a problem in which adjacent row groups
        # with the same value will cause rgroup and tspanner collision
        # comp_val <- paste0(row_ref_tbl[, rgroup_td], row_ref_tbl[, tspanner_td])

        comp_val <- row_ref_tbl %>% dplyr::pull(rgroup_td)

        if (!is.null(tspanner_td)) {
            comp_val <- paste0(comp_val, row_ref_tbl %>% dplyr::pull(tspanner_td))
        }

        lens <- rle(comp_val)$lengths
        idx <- cumsum(lens)

        htmlTable_args$rgroup <- row_ref_tbl %>%
            dplyr::slice(idx) %>%
            dplyr::pull(rgroup_td)

        htmlTable_args$n.rgroup <- lens
    }
    if (!is.null(tspanner_td)) {
        htmlTable_args$tspanner <-
            rle(row_ref_tbl %>% dplyr::pull(tspanner_td))$value
        htmlTable_args$n.tspanner <-
            rle(row_ref_tbl %>% dplyr::pull(tspanner_td))$lengths
    }

    # Get names and indices for column groups
    if(!is.null(cgroup1_td)) {
        cgroup1 <- rle(col_ref_tbl %>% dplyr::pull(cgroup1_td))$value
        n.cgroup1 <- rle(col_ref_tbl %>% dplyr::pull(cgroup1_td))$lengths
        if(!is.null(cgroup2_td)) {
            cgroup2 <- rle(col_ref_tbl %>% dplyr::pull(cgroup2_td))$value
            n.cgroup2 <- rle(col_ref_tbl %>% dplyr::pull(cgroup2_td))$lengths
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
    return(x[keep.idx,])
}

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

simplify_arg_list <- function(...) {
    x <- list(...)
    idx <- sapply(x, is.null)
    return(x[!idx])
}

get_col_vars <- function(...) {
    out <- simplify_arg_list(...)
    return(out[names(out) %in%
                   c("value", "header_td",
                     "rnames_td", "rgroup_td",
                     "cgroup1_td", "cgroup2_td",
                     "tspanner_td")])
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
        dplyr::arrange_(.dots = cols)

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
        dplyr::arrange_(.dots = cols)

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

    out <- x %>%
        dplyr::left_join(col_idx_df, cols)

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

    out <- x %>%
        dplyr::left_join(row_idx_df, by = cols)

    return(out)
}
