
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rrViewer

<!-- badges: start -->

[![R-CMD-check](https://github.com/cbedwards-dfw/rrViewer/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/cbedwards-dfw/rrViewer/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of rrViewer is to assist in QA/QC for the \[Coho\] run
reconstruction process for the FRAM team at WDFW.

## Installation

You can install the development version of rrViewer from
[GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("cbedwards-dfw/rrViewer")
```

## Example

There are two key functions in this package. `read_rr_trs()` reads in an
excel file of the run reconstruction. `plot_rr_mu()` then plots
timeseries of fisheries and totals for a given stock (the identifies of
the blocks in the run reconstruction excel file) and management unit
(the rows within a block). By default `plot_rr_mu()` calcuates the
proportional values across fisheries for a given year, but can also be
set to instead present the raw values in the excel file.
`list_rr_options()` can be used to identify `stock` and
`management_unit_name` options for making plots.

    rr.dat = read_rr_trs(paste0(path, "/Copy of PScohoRR_TRS_2010-2023_2023-01-22 _draft.xlsx"))
    plot_rr_mu(rr.dat, stock.use = "South Sound", mu.use = "Nisqually River Hatchery")
