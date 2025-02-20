% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_rr_mu.R
\name{plot_rr_mu}
\alias{plot_rr_mu}
\title{Plot timeseries of values for a given management unit.}
\usage{
plot_rr_mu(
  data,
  stock.use,
  mu.use,
  normalize_fisheries = TRUE,
  base_font_size = 13
)
}
\arguments{
\item{data}{Dataframe generated from \code{read_rr_trs()}}

\item{stock.use}{Character string of stock name (which identify blocks within the run reconstruction excel file) to plot. Use \code{list_rr_options()} to see options.}

\item{mu.use}{Character string of "management_unit_name" (which identify rows within each block of the run reconstruction excel file) to plot. Use \code{list_rr_options()} to see options. Matches versions of management unit name with and without astrices, as those are only used to add notes and should not be treated as new versions of a management unit.}

\item{normalize_fisheries}{Logical: should the calues of fisheries by turned to \% of this stock's catch per fishery in each year (\code{TRUE}) or presented as raw values (\code{FALSE}). Defaults to \code{TRUE}.}

\item{base_font_size}{Integer passed to theme_bw() to set baseline font size in figures. Defaults to 13.}
}
\value{
Figure object made of ggplots combined using \code{{patchwork}}.
}
\description{
Plot timeseries of values for a given management unit.
}
