#' Plot timeseries of values for a given management unit.
#'
#' @param data Dataframe generated from `read_rr_trs()`
#' @param stock.use Character string of stock name (which identify blocks within the run reconstruction excel file) to plot. Use `list_rr_options()` to see options.
#' @param mu.use Character string of "management_unit_name" (which identify rows within each block of the run reconstruction excel file) to plot. Use `list_rr_options()` to see options. Matches versions of management unit name with and without astrices, as those are only used to add notes and should not be treated as new versions of a management unit.
#' @param normalize_fisheries Logical: should the calues of fisheries by turned to % of this stock's catch per fishery in each year (`TRUE`) or presented as raw values (`FALSE`). Defaults to `TRUE`.
#' @param base_font_size Integer passed to theme_bw() to set baseline font size in figures. Defaults to 13.
#'
#' @return Figure object made of ggplots combined using `{patchwork}`.
#' @export
#'
plot_rr_mu <- function(data, stock.use,
                       mu.use,
                       normalize_fisheries = TRUE,
                       base_font_size = 13) {
  ## Deal with asterixes
  mu.use <- gsub("[*]*", "", mu.use)
  data$management_unit_name <- gsub("[*]*", "", data$management_unit_name)

  ## input checking
  stock.use <- rlang::arg_match(
    stock.use,
    unique(data$stock)
  )
  mu.options <- data |>
    dplyr::filter(.data$stock == stock.use) |>
    dplyr::pull(.data$management_unit_name) |>
    unique()
  mu.use <- rlang::arg_match(
    mu.use,
    mu.options
  )

  ## split apart left block based on whether or not to normalize
  cols_static <- c("terminal_run", "pct_terminal", "escapement", "total_catch")
  dat.use <- data |>
    dplyr::filter(.data$stock == stock.use) |>
    dplyr::filter(.data$management_unit_name == mu.use) |>
    dplyr::filter(.data$block == "left")
  dat.use.static <- dat.use |>
    dplyr::filter(.data$col_name %in% cols_static)

  fisheries.use <- dat.use |>
    dplyr::filter(!.data$col_name %in% cols_static) |>
    dplyr::group_by(.data$col_name) |>
    dplyr::summarize(value.sum = sum(.data$value)) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$value.sum > 0) |>
    dplyr::pull(.data$col_name)


  ## normalize
  if (normalize_fisheries) {
    dat.use.fisheries <- dat.use |>
      dplyr::filter(.data$col_name %in% fisheries.use) |>
      dplyr::group_by(.data$year) |>
      dplyr::mutate(value = .data$value / sum(.data$value, na.rm = T)) |>
      dplyr::ungroup()
    fisheries.title <- "Normalized fisheries"
  } else {
    dat.use.fisheries <- dat.use |>
      dplyr::filter(.data$col_name %in% fisheries.use)
    fisheries.title <- "Fisheries (reported values)"
  }

  block.use <- "left"
  gp1a <- dat.use.static |>
    dplyr::filter(.data$stock == stock.use) |>
    dplyr::filter(.data$management_unit_name == mu.use) |>
    dplyr::filter(.data$block == block.use) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$year, y = .data$value)) +
    ggplot2::geom_path() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(. ~ .data$col_name, scales = "free_y") +
    ggplot2::labs(title = "Left block", subtitle = "Overview") +
    ggplot2::scale_y_continuous(labels = function(x) {
      format(x, big.mark = ",")
    }) +
    ggplot2::theme_bw(base_size = base_font_size)

  gp1b <- dat.use.fisheries |>
    dplyr::filter(.data$stock == stock.use) |>
    dplyr::filter(.data$management_unit_name == mu.use) |>
    dplyr::filter(.data$block == block.use) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$year, y = .data$value)) +
    ggplot2::geom_path() +
    ggplot2::geom_point() +
    ggplot2::facet_wrap(. ~ .data$col_name, scales = "free_y") +
    ggplot2::labs(subtitle = fisheries.title) +
    ggplot2::theme_bw(base_size = base_font_size)

  if (normalize_fisheries) {
    gp1b <- gp1b + ggplot2::scale_y_continuous(labels = function(x) {
      paste0(round(100 * x, 1), "%")
    })
  } else {
    gp1b <- gp1b + ggplot2::scale_y_continuous(labels = function(x) {
      format(x, big.mark = ",")
    })
  }

  gp1 <- patchwork::wrap_plots(gp1a, gp1b, ncol = 1) + patchwork::plot_layout(heights = c(1, 2))

  block.use <- "right"

  data.right <- data |>
    dplyr::filter(.data$stock == stock.use) |>
    dplyr::filter(.data$management_unit_name == mu.use) |>
    dplyr::filter(.data$block == block.use) |>
    dplyr::filter(!is.na(.data$value))

  ## making subplots when we have super titles
  if (any(!unique(data.right$col_name_super) %in% c("", "Aggregate"))) {
    fig.ls <- list()
    is.first <- TRUE
    for (i.super in seq_along(unique(data.right$col_name_super))) {
      super.cur <- unique(data.right$col_name_super)[i.super]
      fig.ls[[i.super]] <- data.right |>
        dplyr::filter(.data$col_name_super == super.cur) |>
        ggplot2::ggplot(ggplot2::aes(x = .data$year, y = .data$value)) +
        ggplot2::geom_path() +
        ggplot2::geom_point() +
        ggplot2::facet_wrap(. ~ .data$col_name, scales = "free_y") +
        ggplot2::labs(subtitle = super.cur) +
        ggplot2::scale_y_continuous(labels = function(x) {
          format(x, big.mark = ",")
        }) +
        ggplot2::theme_bw(base_size = base_font_size)
      if (is.first) {
        fig.ls[[i.super]] <- fig.ls[[i.super]] + ggplot2::ggtitle("Right block")
      }
      is.first <- FALSE
    }
    gp2 <- patchwork::wrap_plots(fig.ls, ncol = 1)
  } else {
    gp2 <- data.right
    ggplot2::ggplot(ggplot2::aes(x = .data$year, y = .data$value)) +
      ggplot2::geom_path() +
      ggplot2::geom_point() +
      ggplot2::facet_wrap(. ~ .data$col_name, scales = "free_y") +
      ggplot2::labs(title = glue::glue("{block.use} block")) +
      ggplot2::theme_bw(base_size = base_font_size)
  }
  plot_title <- glue::glue("Stock: {stock.use}\nMU: {mu.use}")

  return(patchwork::wrap_plots(gp1, gp2, nrow = 1) +
    patchwork::plot_annotation(title = plot_title))
}

