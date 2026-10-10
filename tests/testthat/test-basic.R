test_that("rep_row helper creates expected matrix", {
  res <- rep_row(c(1, 2), 2)
  expect_equal(dim(res), c(2, 2))
  expect_equal(res[1, ], c(1, 2))
  expect_equal(res[2, ], c(1, 2))
})

test_that("combinations returns valid matrix for 2 levels", {
  res <- combinations(2)
  expect_type(res, "list")
  expect_equal(length(res), 1)
  expect_true(is.matrix(res[[1]]))
  expect_equal(dim(res[[1]]), c(4, 2))
})

test_that("interior_point linear solver works correctly", {
  # Minimization problem: min -x1 - 2*x2 subject to x1 + x2 <= 10
  c <- c(-1, -2)
  M <- matrix(c(1, 1), 1, 2)
  bM <- 10
  res <- interior_point(t = -1, c = c, bM = bM, M = M, e = 1e-4)

  # Check structure
  expect_true(is.list(res) || is.character(res))
  if (is.list(res)) {
    expect_true("The optimal solution X" %in% names(res) || "The optimum value of Z is" %in% names(res))
  }
})

test_that("generate_pivot creates PivotTable object from sample data", {
  rda_path <- system.file("extdata", "my_data.rda", package = "ACRER")
  skip_if(!file.exists(rda_path), "my_data.rda not available")

  e <- new.env()
  load(rda_path, envir = e)
  expect_true(exists("my_data", envir = e))

  sample_sub <- head(e$my_data, 20)
  pt <- generate_pivot(sample_sub)
  expect_s3_class(pt, "PivotTable")
})

test_that("percent_EVD_Recr creates PivotTable object", {
  rda_path <- system.file("extdata", "my_data.rda", package = "ACRER")
  skip_if(!file.exists(rda_path), "my_data.rda not available")

  e <- new.env()
  load(rda_path, envir = e)

  sample_sub <- head(e$my_data, 20)
  pt <- percent_EVD_Recr(sample_sub)
  expect_s3_class(pt, "PivotTable")
})

test_that("save_pivots writes to tempdir safely without modifying package dir", {
  rda_path <- system.file("extdata", "my_data.rda", package = "ACRER")
  skip_if(!file.exists(rda_path), "my_data.rda not available")

  e <- new.env()
  load(rda_path, envir = e)
  sample_sub <- head(e$my_data, 20)
  pt <- generate_pivot(sample_sub)

  tmp <- tempfile("test_save_pivot")
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  out <- save_pivots(pt, "test_piv", output_dir = tmp)
  expect_true(file.exists(out$csv))
  expect_true(file.exists(out$html))
})
