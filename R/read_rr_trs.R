#' Read and format run reconstruction
#'
#' RR sheets are organized into blocks -- one left, usually also one right -- for each
#' stock. Each block has management units for rows and fisheries for columns, with some
#' summary columns and some summary rows. This function translates those into longform; see details. This function is designed to be robust to the addition or subtraction of rows or columns within blocks, and additions of new blocks that have "Return Year" in the second column, first row and "Wild" in the final row, first column. However, changes in the number of columns between the left and right blocks (e.g., shifting the black separator column from N to something else) *will* break this function. However, it appears that this is typically addressed by creating a "continued" block for blocks that would exceed the number of columns. The `combine_continued` argument controls handling of these blocks.
#'
#' To translate each block into compatible longform dataframes, we add several identifiers. `$stock` identifies which stock is in use, `year` and `sheetname` identify
#' which datasheet the data comes from, `block` identifies whether the data comes from the right
#' or left blocks, and then `management_unit_name` and `col_name` identify the row and column identifiers from within each block. Because the columns for right-hand blocks sometimes include super-columns (e.g., "Stillaguamish/Snohomish" has "Aggregate Totals", "Stillaguamish Origin", and Snohomish Origin"). `col_name_super` identifies these. This is given it's own `block` and `stock` value.
#'
#' We also have have a specieal block for "Ar 13D Net CWT..." in columns P-Q.
#'
#' @param path Run reconstruction file
#' @param combine_continued Logical: Should "continued" type blocks be combined? Defaults to `TRUE`.
#'
#' @return tibble of run reconstruction in long form. See details.
#' @export
#'
#' @examples
#' \dontrun{
#' rr.dat <- read_rr_trs("/Copy of PScohoRR_TRS_2010-2023_2023-01-22 _draft.xlsx")
#' }
read_rr_trs <- function(path,
                        combine_continued = TRUE) {
  ## if updating, pay attention to ordering. Don't want to leave trailing chars by accident.
  continued_patterns <- c("[(]Continued[)]", " [(]Continued[)]", "[-]Continued", "Continued",
                          " $")


  all.sheets <- readxl::excel_sheets(path)
  sheets.use <- grep("^[0-9]", all.sheets, value = TRUE)

  i.sheet <- 1

  res.ls <- list()

  ## for each sheet
  for (i.sheet in seq_along(sheets.use)) {
    raw <- readxl::read_excel(
      path = path, sheet = sheets.use[i.sheet],
      col_names = FALSE
    )
    sheet.year <- as.numeric(raw[1, 3])

    ## identify blocks --------
    row.starts <- which(grepl(".*Return Year.*", x = raw[[2]], ignore.case = TRUE)) + 1
    row.ends <- which(grepl("^[ ]*Wild[ ]*$", x = raw[[1]], ignore.case = TRUE))

    col.ends.b1 <- numeric(length(row.ends))
    for (i in 1:length(col.ends.b1)) {
      col.ends.b1[i] <- min(which(is.na(raw[row.starts[i], ]))) - 1
    }
    col.starts.b1 <- rep(1, length(col.ends.b1))

    ## handling the "second blocks" (to the right)
    ## Not all cases have a second block
    blocks.exist <- !is.na(raw[[15]][row.starts])
    row.starts.b2 <- row.starts[blocks.exist]
    row.ends.b2 <- row.ends[blocks.exist]
    col.starts.b2 <- rep(15, length(row.starts.b2))

    col.ends.b2 <- numeric(length(row.ends.b2))
    for (i in seq_along(col.ends.b2)) {
      vals.of.interest <- raw[row.starts.b2[i], -(1:14)]
      if (any(is.na(vals.of.interest))) {
        ## at least one NA
        col.ends.b2[i] <- min(which(is.na(vals.of.interest))) + 13
      } else {
        col.ends.b2[i] <- ncol(raw)
      }
    }


    res.df <- NULL

    ## Read in the blocks
    ## Do the left blocks
    for (i in 1:length(row.starts)) {
      temp <- raw[
        (row.starts[i] + 1):row.ends[i],
        col.starts.b1[i]:col.ends.b1[i]
      ]
      names(temp) <- raw[row.starts[i],
        col.starts.b1[i]:col.ends.b1[i],
        drop = TRUE
      ]
      temp <- temp |>
        janitor::clean_names()
      temp <- temp |>
        tidyr::pivot_longer(
          cols = 2:ncol(temp),
          names_to = "col_name"
        ) |>
        dplyr::mutate(
          block = "left",
          year = sheet.year
        ) |>
        dplyr::mutate(stock = raw[row.starts[i] - 1, 1, drop = TRUE]) |>
        dplyr::mutate(value = as.numeric(.data$value))
      res.df <- rbind(res.df, temp)
    }

    ## Do the right blocks

    for (i in seq_along(row.starts.b2)) {
      temp <- raw[
        (row.starts.b2[i] + 1):row.ends.b2[i],
        col.starts.b2[i]:col.ends.b2[i]
      ]
      ## We need some funky logic for col_names: we have superheaders in some cases,
      ## and their positioning is annoying.
      col_names <- raw[row.starts.b2[i],
        col.starts.b2[i]:col.ends.b2[i],
        drop = TRUE
      ] |>
        janitor::make_clean_names()
      super <- raw[row.starts.b2[i] - 1,
        col.starts.b2[i]:col.ends.b2[i],
        drop = TRUE
      ]
      super <- as.character(super)
      ## first entry of "super" is the stock, so cut.
      super <- super[-1]
      ## if new first entry is an NA, replace with "" to
      ## support consistent prefixing.
      super <- dplyr::if_else(is.na(super),
        super,
        paste0(super, "\n")
      )
      if (is.na(super[1])) {
        super[1] <- ""
      }
      ## using amazing zoo:: feature to fill in the NAs
      super <- zoo::na.locf(super)
      super <- c("", super)
      col_names <- paste0(super, col_names)

      names(temp) <- col_names
      temp <- temp |>
        tidyr::pivot_longer(
          cols = 2:ncol(temp),
          names_to = "col_name"
        ) |>
        dplyr::mutate(
          block = "right",
          year = sheet.year
        ) |>
        dplyr::mutate(stock = raw[row.starts.b2[i] - 1, 15, drop = TRUE]) |>
        dplyr::mutate(value = as.numeric(.data$value))
      res.df <- rbind(res.df, temp)
    }

    ## We also have the special Ar 13D Net CWT... block that goes from P-Q, and starts on the row with "Ar 13D Net CWT..." in col P (16) and ends on the starts on the row of "Reference: Original Input..." in col Q (17)
    special.start <- which(grepl("Ar 13D Net CWT.*", raw[[16]])) + 1
    special.end <- which(grepl("Reference: Original Input Ar 13D.*", raw[[17]]))
    if (length(special.start == 1)) {
      temp <- dplyr::tibble(
        management_unit_name = raw[special.start - 1, 16, drop = TRUE],
        col_name = raw[special.start:special.end, 17, drop = TRUE],
        value = as.numeric(raw[special.start:special.end, 16, drop = TRUE]),
        block = "special",
        year = sheet.year,
        stock = raw[special.start - 1, 16, drop = TRUE]
      )
      res.df <- rbind(res.df, temp)
    }

    res.df$sheetname <- sheets.use[i.sheet]

    ## add to list of results
    res.ls[[sheets.use[i.sheet]]] <- res.df
  }

  fin.df <- do.call(dplyr::bind_rows, res.ls) |>
    ## handle the inconsistencies in how percentages are reported.
    dplyr::mutate(value = dplyr::if_else(.data$col_name == "pct_terminal" & .data$value > 1,
      .data$value / 100,
      .data$value
    )) |>
    #   ## add supers column
    # mutate(col_for_split = if_else(block == "right",
    #                               col_name,
    #                               NA)) |>
    dplyr::mutate(col_for_split = dplyr::if_else(grepl("\n", .data$col_name),
      .data$col_name,
      paste0("\n", .data$col_name)
    )) |>
    tidyr::separate_wider_delim(
      cols = .data$col_for_split,
      delim = "\n", names = c("col_name_super", "col_name_sub")
    ) |>
    dplyr::mutate(value = tidyr::replace_na(.data$value, 0))

  if(combine_continued){
    for(cur.pattern in continued_patterns){
      fin.df$stock = gsub(cur.pattern, "", fin.df$stock, ignore.case = TRUE)
    }
  }


  return(fin.df)
}
