#' Provide "stock" and "management_unit_name" options for a run reconstruction dataframe
#'
#' Once a run reconstruction dataframe is created, it's nice to see the options for `stock` and
#' `management_unit_name` identifiers for making plots. This function acts as a wrapper for
#' `unique(data$stock)` if no stock name is provided; if stock name is provided,
#' instead is a wrapper for filtering to that stock and then `unique(data$management_unit_name`.
#' When presenting management unit names, removes astrixes; this is consistent with `plot_rr_mu()` behavior.
#'
#' @param data Dataframe generated by `read_rr_trs()`
#' @param stock Optional, stock
#'
#' @return character vector
#' @export
list_rr_options <- function(data, stock = NULL) {
  if (is.null(stock)) {
    cli::cli_alert("`stock` not provided; listing stock options")
    unique(data$stock)
  } else {
    stock <- rlang::arg_match(
      stock,
      unique(data$stock)
    )
    cli::cli_alert("`stock` provided; listing management_unit_name options")
    vec <- data |>
      dplyr::filter(.data$stock == stock) |>
      dplyr::pull(.data$management_unit_name) |>
      unique()
    vec <- gsub("[*]*", "", vec)
    return(vec)
  }
}
