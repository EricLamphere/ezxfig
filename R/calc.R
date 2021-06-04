
#' Programmatically Calculate New Columns
#' @description Create new columns using a list of column names (labels) and formulas. \code{labels} and \code{formulas} should both be character vectors of the same length
#' @param df The data frame you're manipulating. The \code{formulas} should be based off of the column names of df.
#' @param labels A character vector used as the names for the newly created columns.
#' @param formulas A character vector of formulas (e.g. \code{col1 / col2}) used to generate the new columns.
#' @param prefix A prefix to append to the beginning of all of the column names in \code{labels}.
#' @return A data frame with the newly calculated columns.
#' @examples
#' calc(dplyr::tibble(x = c(1,2,3), y = c(2,2,2)), labels = c("x_times_y"),formulas = c("x * y"))
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate
#' @importFrom ezextras "%&%"
#' @export

# TODO: if a formula references a column that doesn't exist in df, exclude it
# TODO: add fun param giving the option to send an error or warning if a formula contains a column
#       that doesn't exist in df
calc <- function(df, labels, formulas, prefix = "") {
  if (any(labels %in% colnames(df))) {
    overwriting <-
      labels[which(labels %in% colnames(df))] %>%
      paste(collapse = ", ")
    cli::cli_alert("Overwriting existing columns: {overwriting}")
  }

  mutate_vec <-
    setNames(
      rlang::parse_exprs(formulas),
      prefix %&% labels
    )

  calculated <- df %>%
    dplyr::mutate(
      !!!mutate_vec
    )
  return(calculated)
}
