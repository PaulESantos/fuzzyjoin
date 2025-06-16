"%||%" <- function(x, y) if (is.null(x)) y else x
#' Helper function to standardize 'by' argument for joins
#'
#' This function processes the 'by' argument used in join operations, handling
#' various input formats and providing comprehensive validation.
#'
#' @param by Join specification. Can be:
#'   - NULL: automatically detect common columns
#'   - character vector: column names (same in both tables)
#'   - named character vector: names are columns in x, values are columns in y
#'   - list: with elements x and y specifying columns (returned as-is)
#' @param x First data frame
#' @param y Second data frame
#' @param quiet Logical. If TRUE, suppress joining messages. Default is FALSE.
#'
#' @return List with elements:
#'   - x: character vector of column names in first data frame
#'   - y: character vector of column names in second data frame
#'
#' @details
#' The function handles several input formats:
#' - If by = NULL, finds intersection of column names
#' - If by is already a list with x and y elements, returns as-is
#' - If by is a character vector, processes named/unnamed elements
#' - Validates that all specified columns exist in their respective data frames
#'

common_by <- function(by = NULL, x, y, quiet = FALSE) {

  # Input validation
  if (!is.data.frame(x) || !is.data.frame(y)) {
    stop("Both x and y must be data frames", call. = FALSE)
  }

  # If by is already a properly formatted list, return as-is
  if (is.list(by) && all(c("x", "y") %in% names(by))) {
    # Validate the list format
    if (!is.character(by$x) || !is.character(by$y)) {
      stop("List 'by' must contain character vectors named 'x' and 'y'", call. = FALSE)
    }
    if (length(by$x) != length(by$y)) {
      stop("Length of by$x must equal length of by$y", call. = FALSE)
    }

    # Validate columns exist
    missing_x <- setdiff(by$x, names(x))
    missing_y <- setdiff(by$y, names(y))

    if (length(missing_x) > 0) {
      stop("Column(s) not found in x: ", paste(missing_x, collapse = ", "), call. = FALSE)
    }
    if (length(missing_y) > 0) {
      stop("Column(s) not found in y: ", paste(missing_y, collapse = ", "), call. = FALSE)
    }

    return(by)
  }

  # Handle NULL case - auto-detect common columns
  if (is.null(by)) {
    # Use dplyr::tbl_vars if available, otherwise use names()
    x_vars <- if (requireNamespace("dplyr", quietly = TRUE)) {
      tryCatch(dplyr::tbl_vars(x), error = function(e) names(x))
    } else {
      names(x)
    }

    y_vars <- if (requireNamespace("dplyr", quietly = TRUE)) {
      tryCatch(dplyr::tbl_vars(y), error = function(e) names(y))
    } else {
      names(y)
    }

    by <- intersect(x_vars, y_vars)

    if (length(by) == 0) {
      stop("No common variables found. Please specify 'by' parameter.", call. = FALSE)
    }

    if (!quiet) {
      # Use dput format for consistency with dplyr, but fallback to simple format
      join_msg <- tryCatch({
        paste("Joining by:", utils::capture.output(dput(by)))
      }, error = function(e) {
        paste("Joining by:", paste0('"', by, '"', collapse = ", "))
      })
      message(join_msg)
    }

    return(list(x = by, y = by))
  }

  # Handle character vector case
  if (is.character(by)) {
    # Handle named character vectors
    if (!is.null(names(by))) {
      by_x <- names(by)
      by_y <- unname(by)

      # Handle partially named vectors (empty names should use the value)
      empty_names <- by_x == ""
      by_x[empty_names] <- by_y[empty_names]

    } else {
      # Unnamed character vector - same columns in both tables
      by_x <- by
      by_y <- by
    }

    # Validate columns exist
    missing_x <- setdiff(by_x, names(x))
    missing_y <- setdiff(by_y, names(y))

    if (length(missing_x) > 0) {
      stop("Column(s) not found in x: ", paste(missing_x, collapse = ", "), call. = FALSE)
    }
    if (length(missing_y) > 0) {
      stop("Column(s) not found in y: ", paste(missing_y, collapse = ", "), call. = FALSE)
    }

    return(list(x = by_x, y = by_y))
  }

  # Handle other list formats that might not have x/y structure
  if (is.list(by)) {
    stop("List 'by' must be a named list with elements 'x' and 'y'", call. = FALSE)
  }

  # Invalid type
  stop("'by' must be NULL, a character vector, or a list with elements 'x' and 'y'",
       call. = FALSE)
}


# Make sure there's a distance column included in the output
ensure_distance_col <- function(ret, distance_col, mode) {
  if (!(mode %in% c("semi", "anti")) &&
      !is.null(distance_col) &&
      is.null(ret[[distance_col]])) {
    if (nrow(ret) == 0) {
      ret[[distance_col]] <- numeric(0)
    } else {
      ret[[distance_col]] <- NA
    }
  }
  ret
}

unrowwname <- function(x) {
  rownames(x) <- NULL
  x
}
