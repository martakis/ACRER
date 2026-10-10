#' Internal helper to save pivot tables
#' @noRd
save_pivots <- function(pivot_obj, filename_base, output_dir = NULL) {
  # Determine output directory:
  # 1. Use explicit output_dir if provided
  # 2. If running in package dev directory, save into inst/extdata
  # 3. Default to tempdir() for CRAN compliance
  out_dir <- if (!is.null(output_dir) && nzchar(output_dir)) {
    output_dir
  } else if (dir.exists(file.path("inst", "extdata"))) {
    file.path("inst", "extdata")
  } else {
    tempdir()
  }

  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  df <- pivot_obj$asDataFrame()
  df[is.na(df)] <- ""

  # Save CSV
  out_csv <- file.path(out_dir, paste0(filename_base, ".csv"))
  write.csv(df, out_csv, row.names = TRUE)

  # Save RDS
  out_rds <- file.path(out_dir, paste0(filename_base, ".rds"))
  saveRDS(df, out_rds)

  # Generate HTML preview
  html_dir <- file.path(out_dir, "HTML")
  if (!dir.exists(html_dir)) dir.create(html_dir, recursive = TRUE)
  html_path <- file.path(html_dir, paste0(filename_base, ".html"))
  html_content <- knitr::kable(df, format = "html", table.attr = "class='table table-striped'")
  cat(html_content, file = html_path)

  invisible(list(csv = out_csv, rds = out_rds, html = html_path))
}
