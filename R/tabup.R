#' Cross Tabulation Function
#'
#' Generates a frequency table for the specified variables, including counts, percentages, and cumulative percentages. Supports weighted counts and optional removal of missing values.
#'
#' @param x A data.frame or tibble.
#' @param ... Variables to include in the tabulation. If only one variable is provided, a total row is added to the output.
#' @param wt Optional. A variable providing frequency weights. Defaults to \code{NULL}.
#' @param na.rm Logical. Should missing values (\code{NA}) be removed before computation? Defaults to \code{FALSE}.
#' @param sort Logical. Should the output be sorted by the grouping variables? Defaults to \code{TRUE}.
#' @return A data.frame sorted by the specified variables, including columns \code{Freq}, \code{Percent}, and \code{Cum} for counts.
#' @examples
#' # Setup
#' library(dplyr)
#' N <- 100
#' K <- 10
#' df <- tibble(
#'   id = sample(c(NA, 1:5), N / K, TRUE),
#'   v1 = sample(1:5, N / K, TRUE)
#' )
#' # One-way tabulation
#' df %>% tabup(id)
#' df %>% tabup(id, wt = v1)
#' # Two-way tabulation
#' df %>% tabup(id, v1)
#' df %>% filter(id >= 3) %>% tabup(id)
#' @export
tabup <- function(x,
                   ...,
                   wt = NULL,
                   na.rm = FALSE,
                   sort = TRUE) {
  vars <- rlang::ensyms(...)
  num_vars <- length(vars)


  # Remove missing values if na.rm is TRUE
  if (na.rm) {
    vars_char <- sapply(vars, rlang::as_string)
    x <- dplyr::filter(x, !if_any(all_of(vars_char), is.na))
  }


  # Compute the counts and get the 1st dataframe
  x <- dplyr::count(x, !!!vars, wt = {{ wt }})
  x <- dplyr::rename(x, Freq = n)
  x <- dplyr::mutate(x,
                     Percent = Freq / sum(Freq) * 100,
                     Cum = cumsum(Percent))


  # If there is only one variable:
  total_row <- NULL
  if (num_vars == 1) {
    # Prepare variable name and total row
    var_name <- rlang::as_name(vars[[1]])
    total_freq <- sum(x$Freq)

    # Coerce to character in 1st df
    x <- x %>%
      dplyr::mutate(!!var_name := as.character(.data[[var_name]]))

    # tible for 2st df
    total_row <- tibble::tibble(
      !!var_name := "Total",
      Freq = total_freq,
      Percent = 100,
      Cum = NA
    )
  }


  # Sort the data if required
  if (sort) {
    x <- dplyr::arrange(x, !!!vars)
  }


  # Add the total row at the end
  if (!is.null(total_row)) {
    x <- dplyr::bind_rows(x, total_row)
  }


  # Display the result
  print(x)

  invisible(x)
}

