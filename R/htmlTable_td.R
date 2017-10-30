#' Generate an htmlTable from tidy data
#'
#' \code{htmlTable_td} is a wrapper script for \code{htmlTable} that will build
#' an htmlTable using tidy-format data and by mapping various elements of the
#' table to individual columns.
#'
#' @param x The tidy set of data from which to build the htmlTable
#' @param cell_value The individual values which will fill each cell of the
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
                         cell_value,
                         header_td,
                         rnames_td,
                         rgroup_td = NULL,
                         cgroup1_td = NULL,
                         cgroup2_td = NULL,
                         tspanner_td = NULL) {

    # Change NA values to "" in all but the cell_value column
    x <- x %>% convert_NA_values(setdiff(colnames(x), cell_value))
    x <- x %>% convert_NA_values(cell_value, fill = "NA")

    # Create tables from which to gather row, column, and tspanner names
    # and indices
    row_ref_tbl <- x %>%
        get_row_tbl(rnames_td = rnames_td,
                    rgroup_td = rgroup_td,
                    tspanner_td = tspanner_td)
    col_ref_tbl <- x %>%
        get_col_tbl(header_td = header_td,
                    cgroup1_td = cgroup1_td,
                    cgroup2_td = cgroup2_td)

    # Format the cell_values for display
    to_select <- c("r_idx", "c_idx", cell_value)
    formatted_df <- x %>%
        add_col_idx(header_td = header_td,
                    cgroup1_td = cgroup1_td,
                    cgroup2_td = cgroup2_td) %>%
        add_row_idx(rnames_td = rnames_td,
                    rgroup_td = rgroup_td,
                    tspanner_td = tspanner_td) %>%
        dplyr::select(to_select) %>%
        tidyr::spread(key = c_idx,
                      value = cell_value,
                      fill = "") %>%
        dplyr::select(-r_idx)

    # Get names and indices for row groups and tspanners
    htmlTable_args <- list(x = formatted_df,
                           rnames = row_ref_tbl[, rnames_td],
                           header = col_ref_tbl[, header_td],
                           padding.tspanner = "&nbsp;&nbsp;")
    if (!is.null(rgroup_td)) {
        # This will take care of a problem in which adjacent row groups
        # with the same value will will cause rgroup and tspanner collision
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

get_col_tbl <- function(x,
                        header_td,
                        cgroup1_td = NULL,
                        cgroup2_td = NULL) {

    cols <- c(cgroup2_td, cgroup1_td, header_td)

    x <- x %>% convert_NA_values(cols)

    out <- x %>%
        dplyr::select(cols) %>%
        unique %>%
        dplyr::arrange_(.dots = cols)

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
        dplyr::arrange_(.dots = cols)

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
            x[, col] <- factor(x[, col], levels = c(levs, fill))
        } else {
            x[, col][is.na(x[, col])] <- fill
        }
    }
    return(x)
}